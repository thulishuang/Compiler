{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text as T
import Data.Functor
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

data Tree a = Nil | Node a (Tree a) (Tree a) (Tree a)
    deriving (Show, Generic, Out)   

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
    | Cha Char
    | St String
    | Int Integer
    | Dou Double
    deriving Show

data Statement
    = Begin Statement Statements
    | Skip
    | Set Expr Expr
    | If Expr Statement Statement
    | While Expr Statement
    deriving Show

data Statements
    = NilStat
    | List Statement Statements
    deriving Show
    
data Program
    = Pro Statement
    deriving Show

data AllExpr
    = Program
    | Statements
    | Statement
    | Expr
    deriving Show

data ExprVal 
    = Num 
    | Bool 
    | Char 
    | String 
    | ExprVal
    deriving Show

instance Eq ExprVal where
    Num == Num = True
    Bool == Bool = True
    Char == Char = True
    [ExprVal] == [ExprVal] = True
    [Char] == String = True
    _ ==_ = False

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
    d <- lexeme $ T.double 
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
    lexeme $ T.char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ T.char ')'
    return (Not expr)
    
andParser :: Parser Expr
andParser = do
    lexeme $ T.char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ T.char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ T.char '('
    lexeme $ T.char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ T.char '('
    lexeme $ T.char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ T.char '('
    lexeme $ T.char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ T.char '('
    lexeme $ T.char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Div expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ T.char '('
    lexeme $ T.char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Eq expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ T.char '('
    lexeme $ T.char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Lt expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ T.char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Le expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ T.char '('
    lexeme $ T.char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Gt expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ T.char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Ge expr1 expr2)

nilListParser :: Parser Expr
nilListParser = lexeme $ string "()" $> NilLit

consParser :: Parser Expr
consParser = do
    lexeme $ T.char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ T.char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ T.char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ T.char ')'
    return (Car expr)
    
cdrParser :: Parser Expr
cdrParser = do
    lexeme $ T.char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ T.char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    lexeme $ T.char '\'' 
    c <- anyChar
    lexeme $ T.char '\''
    return (Cha c)

stringParser :: Parser Expr
stringParser = do
    lexeme $ T.char '\"'
    s <- takeWhile1 (\x -> if x == '\"' then True else False)
    lexeme $ T.char '\"'
    return (St s)
    
setParser :: Parser Statement
setParser = do
    lexeme $ T.char '('
    lexeme $ string "set!"
    var <- variableParser
    expr <- exprParser
    lexeme $ T.char ')'
    return (Set var expr)
    
skipParser :: Parser Statement
skipParser = do
    lexeme $ string "skip"
    return Skip
    
ifParser :: Parser Statement
ifParser = do
    lexeme $ T.char '('
    lexeme $ string "if"
    expr <- exprParser
    stat1 <- statParser
    stat2 <- statParser
    lexeme $ T.char ')'
    return (If expr stat1 stat2)
    
whilestatParser :: Parser Statement
whilestatParser = do
    lexeme $ T.char '('
    lexeme $ string "while"
    expr <- exprParser
    stat <- statParser
    lexeme $ T.char ')'
    return (While expr stat)
    
statlistParser :: Parser Statement
statlistParser = do
    lexeme $ T.char '('
    lexeme $ string "begin"
    stat <- statParser
    stats <- statsParser
    lexeme $ T.char ')'
    return (Begin stat stats)
    
statslistParser :: Parser Statements
statslistParser = do
    stat <- statParser
    stats <- statsParser
    return (List stat stats)
    
nilParser :: Parser Statements
nilParser = do
    lexeme $ T.char ')'
    return NilStat

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

eval :: Expr -> ExprVal
eval FalseLit = False
eval TrueLit = True
eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)
eval (Eq p q) = (douEval p) == (douEval q)
eval (Lt p q) = (douEval p) < (douEval q)
eval (Le p q) = (douEval p) <= (douEval q)
eval (Gt p q) = (douEval p) > (douEval q)
eval (Ge p q) = (douEval p) >= (douEval q)

eval (Dou p) = p
eval (Add p q) = (douEval p) + (douEval q)
eval (Sub p q) = (douEval p) - (douEval q)
eval (Mul p q) = (douEval p) * (douEval q)
eval (Div p q) = (douEval p) / (douEval q)

eval NilLit = []
eval Cha c = c
eval St s = s
eval Cons e1 e2
    | (e2 == []) = e1:e2
    | (e2 == x:xs) && (e1 == x) = e1:e2
    -- | Otherwise (error "temp error")

eval (Car (Cons e1 e2)) = eval e1
eval (Cdr (Cons e1 e2)) = eval e2

eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)

getExpr :: Either String Expr -> String
getExpr (Left errStr) =  "not a valid expr: " ++ errStr
getExpr (Right expr) = show $ eval expr

genTree :: AllExpr -> Tree String
genTree FalseLit = Node "False" Nil Nil Nil
genTree TrueLit = Node "True" Nil Nil Nil
genTree (Not p) = Node "not" (genTree p) Nil Nil
genTree (And p q) = Node "and" (genTree p) (genTree q) Nil
genTree (Or p q) = Node "or" (genTree p) (genTree q) Nil
genTree (Add p q) = Node "+" (genTree p) (genTree q) Nil
genTree (Sub p q) = Node "-" (genTree p) (genTree q) Nil
genTree (Mul p q) = Node "*" (genTree p) (genTree q) Nil
genTree (Div p q) = Node "/" (genTree p) (genTree q) Nil
genTree (Eq p q) = Node "==" (genTree p) (genTree q) Nil
genTree (Lt p q) = Node "<" (genTree p) (genTree q) Nil
genTree (Le p q) = Node "<=" (genTree p) (genTree q) Nil
genTree (Gt p q) = Node ">" (genTree p) (genTree q) Nil
genTree (Ge p q) = Node ">=" (genTree p) (genTree q) Nil
genTree (Int p) = Node (show p) Nil Nil Nil
genTree (Dou p) = Node (show p) Nil Nil Nil
genTree (Begin p q) = Node "begin" (genTree p) (genTree q) Nil
genTree Skip = Node "skip" Nil Nil Nil
genTree (Set p q) = Node "set" (genTree p) (genTree q) Nil
genTree (If p q r) = Node "if" (genTree p) (genTree q) (genTree r)
genTree (While p q) = Node "while" (genTree p) (genTree q) Nil
genTree NilStat = Node "nil" Nil Nil Nil
genTree (List p q) = Node "statement_list" (genTree p) (genTree q) Nil
genTree (Pro p) = Node "program" (genTree p) Nil Nil

defMain :: IO ()
defMain = do
    putStrLn $ show $ parseOnly notParser "(not True)"
    putStrLn $ getExpr $ parseOnly addParser "(+ 1.2 2.2 )" 
    putStrLn $ getExpr $ parseOnly mulParser "(* 2 2.2 )" 
    putStrLn $ getExpr $ parseOnly divParser "(/ 10 2 )" 
    putStrLn $ show $ parseOnly exprParser "12.3"
    putStrLn $ getExpr $ parseOnly charParser "\'a\'" 
    putStrLn $ getExpr $ parseOnly stringParser "\"abc\""
    putStrLn $ getExpr $ parseOnly consParser "(cons \'a\' \'b\')"