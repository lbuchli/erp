module Combinators where 

import Base
import Control.Applicative

many1 :: Parser i t e a -> Parser i t e [a]
many1 p = (:) <$> p <*> many p 
