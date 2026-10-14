module NeedsExt (pick) where

pick :: Maybe Int -> String
pick = \case
  Just n -> n
  Nothing -> ""
