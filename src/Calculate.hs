{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Parser
import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Data.Text

data Number
    = Integer
    | Double
    deriving Show

eval (Add p q) = p + q
eval (Sub p q) = p - q
eval (Mul p q) = p * q
eval (Div p q) = p / q
eval (Eq p q) = (p == q)
eval (Lt p q) = (p < q)
eval (Le p q) = (p <= q)
eval (Gt p q) = (p > q)
eval (Ge p q) = (p >= q)