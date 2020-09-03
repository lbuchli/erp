{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Base where

import Control.Applicative
import Data.Bifunctor
import Data.SBV
import Test.QuickCheck (Arbitrary(..))

{-|
A position in the source code.
-}
data Pos = Pos {
  row :: Int,
  col :: Int
} deriving (Eq, Show)

{-|
Abstract syntax tree, a variant of the default rose tree implementation that
can also carry data on a branch and is thus a bifunctor
-}
data AST a b = Branch b [AST a b] | Leaf Pos a
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqSymbolic (AST a b) where
  (.==) x y = toSBool (x == y) 

toSBool :: Bool -> SBool
toSBool True = sTrue
toSBool False = sFalse

instance Bifunctor AST where
  first f (Branch x xs) = Branch x (map (first f) xs)
  first f (Leaf pos x)      = Leaf pos (f x)
  second f (Branch x xs) = Branch (f x) (map (second f) xs)
  second _ (Leaf pos x)      = Leaf pos x

instance Functor (AST a) where
  fmap = second

instance Applicative (AST a) where
  pure x = Branch x []
  (<*>) (Branch a xs) (Branch b ys) = Branch (a b) [x <*> y | x <- xs, y <- ys]
  (<*>) (Branch _ _)  (Leaf pos b)  = Leaf pos b
  (<*>) (Leaf pos a)  (Branch _ _)  = Leaf pos a
  (<*>) (Leaf pos a)  (Leaf _ _)    = Leaf pos a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AST a b) where
  arbitrary = undefined

{-|
The result of a parsing operation.
e: Error type
b: AST type
-}
data Result e a b = Result {
  ast    :: AST a b,
  score  :: Int,
  errors :: [(Pos, e)],
  rest   :: [(Pos, a)]
} deriving (Eq, Show)

instance Functor (Result e a) where
  fmap f (Result a s e r) = Result (second f a) s e r

mapScore :: (Int -> Int) -> Result e a b -> Result e a b
mapScore f (Result a s e r) = Result a (f s) e r

class Tokenizable i t where
  tokenize :: Pos -> i -> (Pos, t, Maybe i)

{-|
A general parser
i: Input type
t: Token type
e: Error type
b: AST type
-}
newtype Parser i t e a = Parser {
  parseToks :: Tokenizable i t => [(Pos, t)] -> [Result e t a]
}

instance Show (Parser i t e a) where
  show _ = "Parsers can't be shown"

instance Functor (Parser i t e) where
  fmap f p = Parser $ map (fmap f) . parseToks p

instance Applicative (Parser i t e) where
  pure x    = Parser $ const [Result (Branch x []) 0 [] []]
  (<*>) f a = Parser $ productOf f a

productOf :: Tokenizable i t => Parser i t e (a -> b) -> Parser i t e a -> [(Pos, t)] -> [Result e t b]
productOf a b toks = clean res_b
  where
    res_a = parsePossibleError a toks 
    res_b = flatten $ map (`apply` b) res_a
    flatten [] = []
    flatten (x:xs) = x ++ flatten xs

apply :: Tokenizable i t => Result e t (a -> b) -> Parser i t e a -> [Result e t b]
apply result_a b = map (combine result_a) results_b
  where
    results_b = parsePossibleError b (rest result_a)
    combine (Result aa sa ea _) (Result ab sb eb rb) = Result (aa <*> ab) (sa + sb) (ea ++ eb) rb

-- TODO monad instance

instance Alternative (Parser i t e) where
  empty = Parser $ const []
  (<|>) a b = Parser (\toks -> clean (parsePossibleError a toks ++ parsePossibleError b toks))

parsePossibleError :: Tokenizable i t => Parser i t e a -> [(Pos, t)] -> [Result e t a]
parsePossibleError parser = parseToks (tolerate_error parser)
  where
    tolerate_error :: Parser i t e b -> Parser i t e b
    tolerate_error p
      =   Parser (map (mapScore (1-)) . parseToks p)
      <|> Parser (map (mapScore (+1)) . parseToks p . skip1) -- TODO also invent a token, end innermost production
    skip1 [] = []
    skip1 (_:xs) = xs
  
parse :: Tokenizable i t => Parser i t e a -> i -> Maybe (Result e t a)
parse p i = select_result $ parsePossibleError p (tokens (Pos 0 0) i)
  where
    select_result = foldl acc Nothing
    acc Nothing n                            = Just n
    acc (Just prev) n | score n > score prev = Just n
    acc (Just prev) _                        = Just prev

tokens :: Tokenizable i t => Pos -> i -> [(Pos, t)]
tokens pos r = case r' of
                 Nothing  -> [(pos, t)]
                 Just r'' -> (pos, t) : tokens pos' r''
  where (pos', t, r') = tokenize pos r

-- clean up branches with too many points
clean :: [Result e t a] -> [Result e t a]
clean inp = filter (\x -> score x < max_score) inp
  where
    min_score = foldl (\prev n -> min (score n) prev) 0 inp
    max_score = min_score * 2
