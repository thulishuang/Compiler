{-# LANGUAGE OverloadedStrings #-}
module ExprTree where

import BoolExpr
import Parser
import PrettyTP
import REPL
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import System.Environment
import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Expr
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | + Expr Expr
	| - Expr Expr 
	| * Expr Expr 
	| / Expr Expr
	| = Expr Expr 
	| < Expr Expr 
	| <= Expr Expr 
	| > Expr Expr 
	| >= Expr Expr
    | NilLit
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr
    | CharLit
    | StringLit
    deriving Show 

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p