-- |
-- Module      : HInsight.Trace.Clean
-- Description : A readable view of the type text GHC prints under @-dppr-debug@.
--
-- GHC's debug printing decorates every name: @GHC.Types.Int{(w) tc 3u}@, and
-- type variables as @(a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)@. This
-- module removes only the patterns it recognises exactly. Anything else is
-- left as GHC printed it and 'residue' says so. It is a view: callers keep the
-- raw text beside it, and nothing here changes what a type means.
module HInsight.Trace.Clean
  ( cleanType,
    cleanBlock,
    residue,
  )
where

import Control.Monad (guard)
import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.List (intercalate, isInfixOf, nub, stripPrefix)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Clean one piece of text. Module qualifiers are dropped, because a single
-- line has nothing for them to collide with.
cleanType :: Text -> Text
cleanType t = T.concat (cleanBlock [t])

-- | Clean several pieces of text together. A module qualifier is dropped only
-- when no two different qualified names in the block share the same bare name,
-- so @A.T@ and @B.T@ stay apart. The result has one entry per input.
cleanBlock :: [Text] -> [Text]
cleanBlock ts = map (T.pack . qualify collisionFree . binders . stripDecor . T.unpack) ts
  where
    stage1 :: [String]
    stage1 = map (binders . stripDecor . T.unpack) ts
    names :: [(String, String)]
    names = concatMap qualifiedNames stage1
    byOcc :: M.Map String [String]
    byOcc = M.fromListWith (\a b -> nub (a ++ b)) [(occ, [full]) | (full, occ) <- names]
    collisionFree :: String -> Bool
    collisionFree full = case [occ | (f, occ) <- names, f == full] of
      (occ : _) -> maybe True ((== 1) . length) (M.lookup occ byOcc)
      [] -> True

-- | The debug patterns still present in a text. An empty list means every
-- decoration was recognised.
residue :: Text -> [Text]
residue t = [p | p <- ["{tv}", "{v}", " Nothing [", "{(w)"], T.unpack p `isInfixOf` s]
  where
    s :: String
    s = T.unpack t

-- @{(w) tc 3u}@ becomes nothing.
stripDecor :: String -> String
stripDecor s = case s of
  '{' : '(' : c : ')' : ' ' : rest
    | isAlpha c,
      (body, '}' : after) <- break (== '}') rest,
      all (\ch -> isAlphaNum ch || ch == ' ') body ->
        stripDecor after
  x : xs -> x : stripDecor xs
  [] -> []

-- @(name{tv} Nothing [tau:0] :: kind)@ becomes @name[tau:0]@.
binders :: String -> String
binders s = case s of
  '(' : rest | Just (shown, after) <- binder rest -> shown ++ binders after
  x : xs -> x : binders xs
  [] -> []

binder :: String -> Maybe (String, String)
binder s = do
  let (name, r1) = span isIdent s
  guard (not (null name))
  let r2 = fromMaybe r1 (stripPrefix "{tv}" r1)
  r3 <- stripPrefix " Nothing [" r2
  let (info, r4) = break (== ']') r3
  guard (not (null info))
  r5 <- stripPrefix "] :: " r4
  after <- skipBalanced r5 (0 :: Int)
  Just (name ++ "[" ++ info ++ "]", after)

-- Skip to the closing parenthesis of the group we are in, honouring nesting.
skipBalanced :: String -> Int -> Maybe String
skipBalanced [] _ = Nothing
skipBalanced (c : cs) depth
  | c == '(' = skipBalanced cs (depth + 1)
  | c == ')' = if depth == 0 then Just cs else skipBalanced cs (depth - 1)
  | otherwise = skipBalanced cs depth

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '\''

-- Qualified names whose bare name starts with a capital: GHC.Types.Int.
qualifiedNames :: String -> [(String, String)]
qualifiedNames s = [(tok, occ) | tok <- chunks s, '.' `elem` tok, Just (_, occ) <- [splitQualified tok]]

qualify :: (String -> Bool) -> String -> String
qualify ok s = concatMap piece (chunks s)
  where
    piece :: String -> String
    piece tok = case splitQualified tok of
      Just (_, occ) | ok tok -> occ
      _ -> tok

-- Split a token at its last dot when every segment starts with a capital.
splitQualified :: String -> Maybe (String, String)
splitQualified tok = do
  (qualifier, occ) <- splitLast (segments tok)
  guard (not (null qualifier) && all startsUpper (occ : qualifier))
  Just (intercalate "." qualifier, occ)
  where
    startsUpper :: String -> Bool
    startsUpper (c : _) = isUpper c
    startsUpper [] = False

splitLast :: [a] -> Maybe ([a], a)
splitLast xs = case reverse xs of
  l : rest -> Just (reverse rest, l)
  [] -> Nothing

segments :: String -> [String]
segments s = case break (== '.') s of
  (a, '.' : rest) -> a : segments rest
  (a, _) -> [a]

-- Maximal runs of identifier characters and dots, with the other characters
-- as their own chunks. A run that ends in a dot (a module prefix of an
-- operator, or the end of a sentence) is kept whole and never qualifies.
chunks :: String -> [String]
chunks [] = []
chunks s@(c : cs)
  | isIdent c =
      let (run, rest) = span (\x -> isIdent x || x == '.') s
       in run : chunks rest
  | otherwise = [c] : chunks cs
