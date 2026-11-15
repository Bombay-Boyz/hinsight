-- |
-- Module      : HInsight.Ghc.ChangeType
-- Description : What GHC's parsed program says about a binding and its signature.
module HInsight.Ghc.ChangeType
  ( changeFacts,
  )
where

import Control.Monad (when)
import Data.Text qualified as T
import GHC.Hs (GhcPs, HsBindLR (..), HsDecl (..), HsModule (..), LHsDecl, Sig (..), hsSigWcType)
import GHC.Parser.Annotation (locA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..))
import HInsight.Error (InsightError (..), RefactorError (..), SessionError (..))
import HInsight.Ghc.Convert (convertSpan)
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Refactor.ChangeType (ChangeFacts (..))
import HInsight.Source (SourceFile, Span, sourceFilePath)

located :: FilePath -> SrcSpan -> Either InsightError Span
located path = \case
  RealSrcSpan rs _ -> convertSpan path rs
  _ -> Left (SessionFailure (GhcThrew path "GHC gave no location for a declaration"))

spelled :: RdrName -> String
spelled = occNameString . rdrNameOcc

-- | Where the binding of a name, and the type of its signature, are in a
-- parsed module. A signature that names other bindings too is refused.
changeFacts :: SourceFile -> Identifier -> HsModule GhcPs -> Either InsightError ChangeFacts
changeFacts file name parsed = do
  binding <- case [loc | L loc (ValD _ FunBind {fun_id = L _ rdr}) <- decls, spelled rdr == wanted] of
    loc : _ -> located path (locA loc)
    [] -> Left (RefactorFailure (DefinitionNotFound (unIdentifier name)))
  signatures <- sequence [signed names wc | L _ (SigD _ (TypeSig _ names wc)) <- decls, wanted `elem` [spelled rdr | L _ rdr <- names]]
  signature <- case signatures of
    found : _ -> Right (Just found)
    [] -> Right Nothing
  pure (ChangeFacts name file signature binding)
  where
    path :: FilePath
    path = sourceFilePath file
    wanted :: String
    wanted = T.unpack (unIdentifier name)
    decls :: [LHsDecl GhcPs]
    decls = hsmodDecls parsed
    signed names wc = do
      when (length names /= 1) (Left (RefactorFailure SignatureNamesOthers))
      case hsSigWcType wc of
        L loc _ -> located path (locA loc)
