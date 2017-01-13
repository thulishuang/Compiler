{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Expr
    = FalseLit
    | TrueLit
    | VarLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Add Expr Expr
    | Sub Expr Expr 
    | Mul Expr Expr 
    | Div Expr Expr
    | Eq Expr Expr
    | Lt Expr Expr 
    | Le Expr Expr 
    | Gt Expr Expr 
    | Ge Expr Expr
    | NilLit
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr
    | CharLit
    | StringLit
    deriving Show

data Statement
    = Begin Statement Statements
    | Skip
    | Set Expr Expr
    | If Expr Statement Statement
    | While Expr Statement
    deriving Show

data Statements
    = Nil
    | List Statement Statements
    deriving Show
    
data Program
    = Pro Statement
    deriving Show

exprParser :: Parser Expr
exprParser = nilListParser <|> consParser <|> carParser <|> cdrParser <|> charParser <|> stringParser <|> falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser <|> lesParser <|> leqParser <|> morParser <|> mqlParser <|> douParser <|> intParser
            <|> variableParser

intParser :: Parser Expr
intParser = do
    ds <- many1 digit
    return (Int (read ds))

douParser :: Parser Expr
douParser = do 
    d <- lexeme $ double 
    return (Dou d) 

statParser :: Parser Statement
statParser = setParser <|> skipParser <|> ifParser <|> whilestatParser <|> statlistParser

statsParser :: Parser Statements
statsParser = statslistParser <|> nilParser

whileParser :: Parser Program
whileParser = do
    stat <- statParser
    return (Pro stat)
            
variableParser :: Parser Expr
variableParser = lexeme $ string "a" $> VarLit
            
falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)
    
andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ char '('
    lexeme $ char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ char '('
    lexeme $ char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Div expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ char '('
    lexeme $ char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Eq expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ char '('
    lexeme $ char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Lt expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Le expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ char '('
    lexeme $ char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Gt expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Ge expr1 expr2)

nilListParser :: Parser Expr
nilListParser = lexeme $ string "()" $> NilLit

consParser :: Parser Expr
consParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ char ')'
    return (Car expr)
    
cdrParser :: Parser Expr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    lexeme $ char '\'' 
    c <- anyChar
    lexeme $ char '\''
    return CharLit

stringParser :: Parser Expr
stringParser = do
    lexeme $ char '\"'
    s <- takeWhile1 (\x -> if x == '\"' then True else False)
    lexeme $ char '\"'
    return StringLit
    
setParser :: Parser Statement
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    var <- variableParser
    expr <- exprParser
    lexeme $ char ')'
    return (Set var expr)
    
skipParser :: Parser Statement
skipParser = do
    lexeme $ string "skip"
    return Skip
    
ifParser :: Parser Statement
ifParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    stat1 <- statParser
    stat2 <- statParser
    lexeme $ char ')'
    return (If expr stat1 stat2)
    
whilestatParser :: Parser Statement
whilestatParser = do
    lexeme $ char '('
    lexeme $ string "while"
    expr <- exprParser
    stat <- statParser
    lexeme $ char ')'
    return (While expr stat)
    
statlistParser :: Parser Statement
statlistParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stat <- statParser
    stats <- statsParser
    lexeme $ char ')'
    return (Begin stat stats)
    
statslistParser :: Parser Statements
statslistParser = do
    stat <- statParser
    stats <- statsParser
    return (List stat stats)
    
nilParser :: Parser Statements
nilParser = do
    return Nil

douEval :: Expr -> Double
douEval (Dou p) = p
douEval (Add p q) = (douEval p) + (douEval q)

getDoubleExpr :: Either String Expr -> String
getDoubleExpr (Left errStr) =  "not a valid add expr: " ++ errStr
getDoubleExpr (Right expr) = show $ douEval expr

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p