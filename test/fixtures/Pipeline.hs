module Pipeline where

countWords :: String -> Int
countWords = sum . map length . words

shout :: String -> String
shout s = unwords $ map reverse $ words s

greet :: IO ()
greet = getLine >>= putStrLn . reverse
