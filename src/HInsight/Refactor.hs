-- |
-- Module      : HInsight.Refactor
-- Description : The pure part of extracting an expression into a top-level function.
module HInsight.Refactor
  ( FunctionName,
    mkFunctionName,
    unFunctionName,
    CallContext (..),
    ExtractFacts (..),
    planExtract,
  )
where

import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isLower, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (EditSet, mkEdit, mkEditSet, offsetOf)
import HInsight.Error (DomainError, InsightError (..), RefactorError (..))
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Source
  ( SourceFile,
    Span,
    mkSpan,
    positionColumn,
    spanEnd,
    spanStart,
    unColumn,
  )
import HInsight.TypeText (TypeText, unTypeText)

-- | The name of the new function. Invariant: a Haskell variable name that is
-- not a reserved word.
newtype FunctionName = UnsafeFunctionName Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'FunctionName'.
mkFunctionName :: Text -> Either RefactorError FunctionName
mkFunctionName t
  | isVariableName t = Right (UnsafeFunctionName t)
  | otherwise = Left (InvalidFunctionName t)

-- | The text inside a 'FunctionName'.
unFunctionName :: FunctionName -> Text
unFunctionName (UnsafeFunctionName t) = t

-- | Where the selection sits, as the parse shows it: whether a call that
-- replaces it needs parentheses.
data CallContext
  = -- | The selection is a whole right-hand side or already inside delimiters.
    CallIsWhole
  | -- | The selection is one part of a larger expression.
    CallIsPart
  deriving stock (Eq, Show)

-- | What GHC reported about a selection.
data ExtractFacts = ExtractFacts
  { -- | The file the selection is in.
    factsFile :: !SourceFile,
    -- | The selected expression.
    factsSelection :: !Span,
    -- | The top-level declaration that holds the selection.
    factsDeclaration :: !Span,
    -- | The variables the selection uses that are bound outside it and are
    -- not top-level, in order of first use.
    factsFreeVariables :: ![Identifier],
    factsContext :: !CallContext
  }
  deriving stock (Eq, Show)

-- | The edits that replace the selection by a call and add the function after
-- the declaration. With a signature, the function is written with it.
planExtract :: Text -> ExtractFacts -> Maybe TypeText -> FunctionName -> Either InsightError EditSet
planExtract src facts signature name = do
  vars <- refuse (traverse variable (nubOrd (factsFreeVariables facts)))
  refuse (when (unFunctionName name `elem` vars) (Left (NameClashesWithVariable (unFunctionName name))))
  refuse (unless (inside (factsSelection facts) (factsDeclaration facts)) (Left SelectionNotInDeclaration))
  selected <- refuse (selectionText src (factsSelection facts))
  body <- refuse (reindent (unColumn (positionColumn (spanStart (factsSelection facts)))) selected)
  call <- domain (mkEdit (factsFile facts) (factsSelection facts) (callText (factsContext facts) name vars))
  anchor <- domain (mkSpan (spanEnd (factsDeclaration facts)) (spanEnd (factsDeclaration facts)))
  added <- domain (mkEdit (factsFile facts) anchor (bindingText name vars signature body))
  domain (mkEditSet [call, added])
  where
    refuse :: Either RefactorError a -> Either InsightError a
    refuse = first RefactorFailure
    domain :: Either DomainError a -> Either InsightError a
    domain = first DomainFailure
    variable :: Identifier -> Either RefactorError Text
    variable i
      | isVariableName (unIdentifier i) = Right (unIdentifier i)
      | otherwise = Left (FreeVariableNotAVariable (unIdentifier i))
    inside :: Span -> Span -> Bool
    inside inner outer = spanStart outer <= spanStart inner && spanEnd inner <= spanEnd outer

bodyColumn :: Int
bodyColumn = 3

selectionText :: Text -> Span -> Either RefactorError Text
selectionText src sp = case (offsetOf src (spanStart sp), offsetOf src (spanEnd sp)) of
  (Just a, Just b)
    | b > a -> Right (T.take (b - a) (T.drop a src))
    | otherwise -> Left EmptySelection
  _ -> Left SelectionNotInText

reindent :: Int -> Text -> Either RefactorError Text
reindent startColumn body = case T.splitOn "\n" body of
  [] -> Right body
  first' : rest -> T.intercalate "\n" . (first' :) <$> traverse moved rest
  where
    delta :: Int
    delta = bodyColumn - startColumn
    moved :: Text -> Either RefactorError Text
    moved l
      | T.all isSpace l = Right l
      | T.any (== '\t') lead = Left SelectionHasTabIndent
      | width < 1 = Left SelectionLeavesLayout
      | otherwise = Right (T.replicate width " " <> text)
      where
        (lead, text) = T.span (\c -> c == ' ' || c == '\t') l
        width :: Int
        width = T.length lead + delta

callText :: CallContext -> FunctionName -> [Text] -> Text
callText context name vars = case (vars, context) of
  ([], _) -> unFunctionName name
  (_, CallIsWhole) -> applied
  (_, CallIsPart) -> "(" <> applied <> ")"
  where
    applied :: Text
    applied = T.unwords (unFunctionName name : vars)

bindingText :: FunctionName -> [Text] -> Maybe TypeText -> Text -> Text
bindingText name vars signature body =
  "\n\n"
    <> maybe "" (\t -> unFunctionName name <> " :: " <> unTypeText t <> "\n") signature
    <> T.unwords (unFunctionName name : vars)
    <> " =\n"
    <> T.replicate (bodyColumn - 1) " "
    <> body

isVariableName :: Text -> Bool
isVariableName t = case T.uncons t of
  Just (c, rest) -> (isLower c || c == '_') && T.all nameChar rest && t `notElem` reserved
  Nothing -> False
  where
    nameChar :: Char -> Bool
    nameChar c = isAlphaNum c || c == '_' || c == '\''

reserved :: [Text]
reserved =
  [ "_",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "foreign",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]
