module PipelineMismatch (total, broken) where

total :: String -> Int
total = sum . map length . words

broken :: Int
broken = length "hello" ++ "x"
