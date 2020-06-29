{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Base where

import Control.Applicative
import Data.Bifunctor

data Pos = Pos {
  line :: Int,
  col :: Int
}

{-|
    The result of a parsing operation.
    e: Error type
    b: AST type
-}
data Result e t b = Result {
  result :: b,
  score  :: Int,       -- keeps track of how good this result is doing; errors
                       -- increase score, successful parsing operations decrease it
  errors :: [(e, Pos)]
}

mapScore :: (Int -> Int) -> Result e t b -> Result e t b
mapScore f (Result res scr errs) = Result res (f scr) errs

instance Functor (Result e t) where
  fmap f (Result res scr errs) = Result (f res) scr errs

{-|
    A general parser
    i: Input type
    t: Token type
    e: Error type
    b: AST type
-}
newtype Parser i t e b = Parser {
  parseToks :: Tokenizable i t => [(t, Pos)] -> ([Result e t b], [(t, Pos)])
}

class Tokenizable i t where
  tokenize :: Pos -> i -> (t, Pos, i)

instance Functor (Parser i t e) where
  fmap f p = Parser $ first (map (fmap f)) . parsePossibleError p

instance Applicative (Parser i t e) where
  pure x    = Parser $ const ([Result x 0 []], [])
  (<*>) f a = Parser $ productOf f a

productOf :: Tokenizable i t => Parser i t e (a -> b) -> Parser i t e a -> [(t, Pos)] -> ([Result e t b], [(t, Pos)])
productOf a b toks = first clean $ res_b
  where
    res_a = parsePossibleError a toks
    res_b = first flatten $ first (map (`apply` b)) res_a
    flatten ([], x) = ([], x)
    flatten (x:xs) = x ++ flatten xs

apply :: Tokenizable i t => (Result e t (a -> b), [(t, Pos)]) -> Parser i t e a -> ([Result e t b], [(t, Pos)])
apply ra b = (map (\rb -> Result (result (fst ra) $ result rb) (score (fst ra) + score rb) (errors (fst ra) ++ errors rb)) (fst res_b), snd res_b)
  where
    res_b = parsePossibleError b (snd ra)

-- TODO monad instance

instance Alternative (Parser i t e) where
  empty = Parser $ const ([], [])
  (<|>) a b = Parser (\toks -> first clean (parsePossibleError a toks <++> parsePossibleError b toks))
    where
      (<++>) (res_a, rst_a) (res_b, rst_b) = (res_a ++ res_b, rst_a ++ rst_b) -- TODO WRONG! choose better variant of the two

parsePossibleError :: Tokenizable i t => Parser i t e b -> [(t, Pos)] -> ([Result e t b], [(t, Pos)])
parsePossibleError parser = parseToks (tolerate_error parser)
  where
    tolerate_error :: Parser i t e b -> Parser i t e b
    tolerate_error p
      =   Parser (first (map (mapScore (1-))) . parseToks p)
      <|> Parser (first (map (mapScore (+1))) . parseToks p . skip1) -- TODO also invent a token, end innermost production
    skip1 [] = []
    skip1 (_:xs) = xs
  
parse :: Tokenizable i t => Parser i t e b -> i -> (Maybe (Result e t b), [(t, Pos)])
parse p i = first select_result $ parsePossibleError p (tokens (Pos 0 0) i)
  where
    select_result = foldl acc Nothing
    acc Nothing n                            = Just n
    acc (Just prev) n | score n > score prev = Just n
    acc (Just prev) _                        = Just prev

tokens :: Tokenizable i t => Pos -> i -> [(t, Pos)]
tokens pos rst = (t, pos) : tokens pos' rst'
  where (t, pos', rst') = tokenize pos rst

-- clean up branches with too many points
clean :: [Result e t b] -> [Result e t b]
clean inp = filter (\x -> score x < max_score) inp
  where
    min_score = foldl (\prev n -> min (score n) prev) 0 inp
    max_score = min_score * 2
