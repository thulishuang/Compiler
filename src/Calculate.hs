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

eval :: Expr -> Number
eval (VarLit p) = p
eval (Add p q) = (eval p) + (eval q)
eval (Sub p q) = (eval p) - (eval q)
eval (Mul p q) = (eval p) * (eval q)
eval (Div p q) = (eval p) / (eval q)