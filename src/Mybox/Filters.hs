module Mybox.Filters where

choose_ :: Show a => [a -> Bool] -> [a] -> a
choose_ fs vs = case choose fs vs of
  Left [] -> error "No candidates to choose from."
  Left cs -> error $ "Cannot choose between: " <> show cs <> "."
  Right v -> v

choose :: Show a => [a -> Bool] -> [a] -> Either [a] a
choose _ [] = Left []
choose _ [x] = Right x
choose [] vs = Left vs
choose (f : fs) vs = case filter f vs of
  [] -> choose fs vs
  vs' -> choose fs vs'
