{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

mkConst :: String -> Integer -> Q [Dec]
mkConst n v = do
  let nm = mkName n
  return [SigD nm (ConT ''Integer), ValD (VarP nm) (NormalB (LitE (IntegerL v))) []]

twice :: Q Exp -> Q Exp
twice e = [| $e + $e |]

tupleT2 :: Q Type
tupleT2 = [t| (Int, Bool) |]

justP :: Q Pat
justP = [p| Just _ |]

str :: QuasiQuoter
str = QuasiQuoter {quoteExp = stringE, quotePat = refuse, quoteType = refuse, quoteDec = refuse}
  where
    refuse :: String -> Q a
    refuse _ = fail "str is an expression quoter only"

typedOne :: Code Q Int
typedOne = [|| 1 ||]
