module Gen (greeting) where

import Language.Haskell.TH (Exp, Q, litE, stringL)

greeting :: Q Exp
greeting = litE (stringL "hello")
