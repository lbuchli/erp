module Parsers where

import Base

token :: Eq t => (t -> Pos -> b) -> t -> Parser i t e b
token f t = Parser checkTok
  where
    checkTok [] = []
    checkTok ((x, pos):xs) | x == t = [Result (f t pos) 0 [] xs]
    checkTok ((x, pos):xs) = ([], xs)
