module Parsers where

import Base

token :: Eq t => t -> Parser i t e a
token t = Parser check_tok
  where
    check_tok []                     = []
    check_tok ((pos, x):xs) | x == t = [Result (Leaf pos t) 0 [] xs]
    check_tok _                      = []

checkTok :: Eq t => t -> [(Pos, t)] -> [Result e t a]
checkTok _ []                     = []
checkTok t ((pos, x):xs) | x == t = [Result (Leaf pos t) 0 [] xs]
checkTok _ _                      = []
