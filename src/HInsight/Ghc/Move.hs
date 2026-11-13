-- |
-- Module      : HInsight.Ghc.Move
-- Description : What GHC's parsed and renamed programs say about moving a top-level definition.
module HInsight.Ghc.Move
  ( Shape (..),
    SourceSide (..),
    TargetSide (..),
    sourceSide,
    targetSide,
    moveFacts,
  )
where

import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, cast, gmapQ)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (GhcPs, HsBindLR (..), HsDecl (..), HsModule (..), LHsDecl, Sig (..))
import GHC.Parser.Annotation (LocatedN, locA)
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan, SrcSpan (..))
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Unit.Module qualified as Ghc (ModuleName)
import HInsight.Error (InsightError (..), RefactorError (..), SessionError (..))
import HInsight.Ghc.Convert (convertSpan, fromDomain)
import HInsight.Graph (ModuleName, mkModuleName)
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Refactor.Move (Anchor (..), ExportClose (..), MoveFacts (..), headerEnd)
import HInsight.Source
  ( Position,
    SourceFile,
    Span,
    mkColumn,
    mkPosition,
    mkSpan,
    positionColumn,
    positionLine,
    sourceFilePath,
    spanContains,
    spanEnd,
    spanStart,
    unColumn,
  )

-- | What a module's header and imports look like.
data Shape = Shape
  { shapeImports :: ![Span],
    shapeAnchor :: !Anchor,
    shapeExports :: !(Maybe ExportClose)
  }
  deriving stock (Eq, Show)

-- | What the module that holds the definition says.
data SourceSide = SourceSide
  { -- | The signature, when there is one, through the end of the binding.
    sideDefinition :: !Span,
    sideShape :: !Shape,
    -- | Whether the module uses the name elsewhere or exports it by name.
    sideKeeps :: !Bool
  }
  deriving stock (Eq, Show)

-- | What the module the definition moves to says.
data TargetSide = TargetSide
  { targetShape :: !Shape,
    targetModuleName :: !ModuleName,
    -- | Whether the module already has a top-level binding of the name.
    targetDefines :: !Bool
  }
  deriving stock (Eq, Show)

refuse :: RefactorError -> Either InsightError a
refuse = Left . RefactorFailure

located :: FilePath -> SrcSpan -> Either InsightError Span
located path = \case
  RealSrcSpan rs _ -> convertSpan path rs
  _ -> Left (SessionFailure (GhcThrew path "GHC gave no location for part of the module header"))

-- | The imports, export list and the place new imports go in a parsed module.
moduleShape :: FilePath -> Text -> HsModule GhcPs -> Either InsightError Shape
moduleShape path src parsed = do
  imports <- traverse (\(L loc _) -> located path (locA loc)) (hsmodImports parsed)
  exports <- traverse (\(L loc ies) -> do { sp <- located path (locA loc); pure (sp, not (null ies)) }) (hsmodExports parsed)
  closing <- traverse (\(sp, nonEmpty) -> (\p -> ExportClose (p, nonEmpty)) <$> before (spanEnd sp)) exports
  anchor <- case reverse imports of
    lastImport : _ -> Right (AfterImport (spanEnd lastImport))
    [] -> case hsmodName parsed of
      Nothing -> refuse ModuleHeaderNotRecognised
      Just (L loc _) -> do
        name <- located path (locA loc)
        let from = maybe (spanEnd name) (spanEnd . fst) exports
        AfterModuleHeader <$> first RefactorFailure (headerEnd src from)
  pure (Shape imports anchor closing)
  where
    before :: Position -> Either InsightError Position
    before p = mkPosition (positionLine p) <$> fromDomain path (mkColumn (unColumn (positionColumn p) - 1))

isBinding :: String -> LHsDecl GhcPs -> Bool
isBinding wanted = \case
  L _ (ValD _ FunBind {fun_id = L _ rdr}) -> spelled rdr == wanted
  _ -> False

spelled :: RdrName -> String
spelled = occNameString . rdrNameOcc

typeSigNames :: LHsDecl GhcPs -> Maybe [String]
typeSigNames = \case
  L _ (SigD _ (TypeSig _ names _)) -> Just [spelled rdr | L _ rdr <- names]
  _ -> Nothing

-- | A declaration that is about a name without defining or using it: a
-- pragma, a warning, a rule or an annotation.
isPragmaLike :: LHsDecl GhcPs -> Bool
isPragmaLike (L _ decl) = case decl of
  SigD _ TypeSig {} -> False
  SigD {} -> True
  WarningD {} -> True
  RuleD {} -> True
  AnnD {} -> True
  _ -> False

mentions :: (Data a) => String -> a -> Bool
mentions wanted x = case cast x of
  Just rdr -> spelled rdr == wanted
  Nothing -> or (gmapQ (mentions wanted) x)

-- | The signature (when it is directly above) and the binding of a name.
definitionSpan :: FilePath -> Identifier -> [LHsDecl GhcPs] -> Either InsightError Span
definitionSpan path name decls = do
  (before, binding) <- case break (isBinding wanted) decls of
    (_, []) -> refuse (DefinitionNotFound (unIdentifier name))
    (b, d : _) -> Right (b, d)
  start <- case reverse before of
    prev : _
      | Just names <- typeSigNames prev,
        wanted `elem` names -> do
          when (names /= [wanted]) (refuse SignatureNamesOthers)
          declSpan prev
    _ -> do
      when (any (maybe False (elem wanted) . typeSigNames) decls) (refuse SignatureNotAdjacent)
      declSpan binding
  when (any (\d -> isPragmaLike d && mentionedIn d) decls) (refuse HasPragmaOrAnnotation)
  end <- declSpan binding
  fromDomain path (mkSpan (spanStart start) (spanEnd end))
  where
    wanted :: String
    wanted = T.unpack (unIdentifier name)
    declSpan :: LHsDecl GhcPs -> Either InsightError Span
    declSpan (L loc _) = located path (locA loc)
    mentionedIn :: LHsDecl GhcPs -> Bool
    mentionedIn = mentions wanted

-- | Every name written below a node, with where it is written.
namesIn :: (Data a) => a -> [(RealSrcSpan, Name)]
namesIn x = here <> concat (gmapQ namesIn x)
  where
    here :: [(RealSrcSpan, Name)]
    here = case (cast x :: Maybe (LocatedN Name)) of
      Just (L loc n)
        | RealSrcSpan rs _ <- locA loc -> [(rs, n)]
      _ -> []

-- | The source module's side of a move, from its parsed and renamed program
-- and the module's own name. A definition that uses another top-level name
-- of its module is refused: it could not be compiled where it is moved to
-- without a cyclic import.
sourceSide :: (Data g, Data e) => SourceFile -> Text -> Identifier -> Ghc.ModuleName -> HsModule GhcPs -> g -> e -> Either InsightError SourceSide
sourceSide file src name this parsed group exports = do
  shape <- moduleShape path src parsed
  definition <- definitionSpan path name (hsmodDecls parsed)
  placed <- traverse (\(rs, n) -> do { sp <- convertSpan path rs; pure (sp, spelledName n) }) (filter (isOwn . snd) (namesIn group))
  let inside = [occ | (sp, occ) <- placed, spanContains definition sp]
      outside = [occ | (sp, occ) <- placed, not (spanContains definition sp)]
      needs = sort (nubOrd (filter (/= wanted) inside))
      exported = wanted `elem` [spelledName n | (_, n) <- namesIn exports, isOwn n]
  unless (null needs) (refuse (UsesNamesOfItsModule (map T.pack needs)))
  pure (SourceSide definition shape (wanted `elem` outside || exported))
  where
    path :: FilePath
    path = sourceFilePath file
    wanted :: String
    wanted = T.unpack (unIdentifier name)
    isOwn :: Name -> Bool
    isOwn n = isExternalName n && moduleName (nameModule n) == this
    spelledName :: Name -> String
    spelledName = occNameString . nameOccName

-- | The target module's side of a move, from its parsed program.
targetSide :: SourceFile -> Text -> Identifier -> HsModule GhcPs -> Either InsightError TargetSide
targetSide file src name parsed = do
  shape <- moduleShape path src parsed
  moduleText <- case hsmodName parsed of
    Nothing -> refuse ModuleHeaderNotRecognised
    Just (L _ n) -> Right (T.pack (moduleNameString n))
  modName <- fromDomain path (mkModuleName moduleText)
  pure (TargetSide shape modName (any (isBinding (T.unpack (unIdentifier name))) (hsmodDecls parsed)))
  where
    path :: FilePath
    path = sourceFilePath file

-- | Both sides together, as the planner wants them.
moveFacts :: SourceFile -> SourceFile -> Identifier -> SourceSide -> TargetSide -> Either InsightError MoveFacts
moveFacts from to name source target = do
  when (sourceFilePath from == sourceFilePath to) (refuse MoveToSameModule)
  when (targetDefines target) (refuse (TargetAlreadyDefines (unIdentifier name)))
  pure
    MoveFacts
      { moveName = name,
        moveFrom = from,
        moveTo = to,
        moveToModule = targetModuleName target,
        moveDefinition = sideDefinition source,
        moveFromImports = shapeImports (sideShape source),
        moveFromAnchor = shapeAnchor (sideShape source),
        moveToImports = shapeImports (targetShape target),
        moveToAnchor = shapeAnchor (targetShape target),
        moveToExports = shapeExports (targetShape target),
        moveKeepsImport = sideKeeps source
      }
