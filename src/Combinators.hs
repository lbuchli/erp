module Combinators where 

import Base
import Control.Applicative

many1 :: Parser i t e b -> Parser i t e [b]
many1 p = (:) <$> p <*> many p 
