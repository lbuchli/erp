{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Lib
import Control.Applicative

main :: IO ()
--main = print (tokens (Pos 0 0) "test test\ntest test2 test4\ttest5" :: [(Pos, String)])
--main = print (map ast (checkTok "test" [(Pos 0 0, "test"), (Pos 0 5, "test")]) :: [AST String ()])
-- TODO error in (<|>) instance, find a way to test for Alternative, Applicative and Functor laws
main = print $ parseToks parser [(Pos 0 0, "test"), (Pos 0 5, "t2"), (Pos 0 7, "t3")]

-- beautiful
instance Tokenizable String String where
  tokenize (Pos r c) i = (Pos r (c + length next), next, rest i next)
    where
      next = word i
      rest s n | length s > length n = Just $ drop (length n+1) i
      rest _ _ = Nothing
      word (x:_) | is_ws x  = ""
      word (x:xs)           = x : word xs
      word []               = ""
      is_ws ' '  = True
      is_ws '\n' = True
      is_ws '\t' = True
      is_ws _    = False

parser :: Parser String String String [String]
parser = token "test" <* token "t2"
