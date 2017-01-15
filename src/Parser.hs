{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M
import Data.Maybe

type Env = M.Map ExprVal ExprVal

data Tree a = Nil | Node a (Tree a) (Tree a) (Tree a)
    deriving (Show, Generic, Out)

data Expr
    = FalseLit
    | TrueLit
    | Var String
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
    | Cal Expr
    deriving Show

--data Number = Integer
--    | Double
--    deriving Show

exprParser :: Parser Expr

exprParser = nilListParser <|> charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser<|> falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser 
            <|> lesParser <|> leqParser <|> morParser <|> mqlParser <|> douParser <|> intParser
            <|> variableParser

intParser :: Parser Expr
intParser = do 
    ds <- many1 digit
    return (Int (read ds))

douParser :: Parser Expr
douParser = do 
    d <- lexeme $ Data.Attoparsec.Text.double 
    return (Dou d) 

            
statParser :: Parser Statement
statParser = setParser <|> skipParser <|> ifParser <|> whilestatParser <|> statlistParser


statsParser :: Parser Statements
statsParser = statslistParser <|> nilParser

allParser :: Parser Program
allParser = whileParser <|> calParser

whileParser :: Parser Program
whileParser = do
    stat <- statParser
    return (Pro stat)
    
calParser :: Parser Program
calParser = do
    expr <- exprParser
    return (Cal expr)
            
variableParser :: Parser Expr
variableParser = do
    xs <- lexeme $ many1 letter
    return (Var xs)
            
falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Not expr)
    
andParser :: Parser Expr
andParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Div expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Eq expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Lt expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Le expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Gt expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Ge expr1 expr2)

nilListParser :: Parser Expr
nilListParser = lexeme $ string "nil" $> NilLit

consParser :: Parser Expr
consParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Car expr)
    
cdrParser :: Parser Expr
cdrParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    lexeme $ Data.Attoparsec.Text.char '\'' 
    c <- anyChar
    lexeme $ Data.Attoparsec.Text.char '\''
    return (Cha c)

stringParser :: Parser Expr
stringParser = do
    lexeme $ Data.Attoparsec.Text.char '\"'
    s <- takeWhile1 (\x -> if x == '\"' then True else False)
    lexeme $ Data.Attoparsec.Text.char '\"'
    return (St (show s))
    
setParser :: Parser Statement
setParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "set!"
    var <- variableParser
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Set var expr)
    
skipParser :: Parser Statement
skipParser = do
    lexeme $ string "skip"
    return (Skip)
    
ifParser :: Parser Statement
ifParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "if"
    expr <- exprParser
    stat1 <- statParser
    stat2 <- statParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (If expr stat1 stat2)
    
whilestatParser :: Parser Statement
whilestatParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "while"
    expr <- exprParser
    stat <- statParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (While expr stat)
    
statlistParser :: Parser Statement
statlistParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "begin"
    stat <- statParser
    --lexeme $ Data.Attoparsec.Text.char ')'
    stats <- statsParser
    return (Begin stat stats)
    
statslistParser :: Parser Statements
statslistParser = do
    stat <- statParser
    stats <- statsParser
    return (List stat stats)
    
nilParser :: Parser Statements
nilParser = do
    lexeme $ Data.Attoparsec.Text.char ')'
    return (NilStat)
 
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data ExprVal = ExprDou Double | ExprBool Bool | ExprChar Char | ExprString String | ExprList [ExprVal] | ExprCons (ExprVal, ExprVal) | ExprNil
instance Show ExprVal where
    show (ExprDou num) = show num
    show (ExprBool bool) = show bool
    show (ExprChar char) = show char
    show (ExprString string) = show string
    show (ExprList list) = show list
    show (ExprCons pair) = show pair
    show ExprNil = show ()

instance Eq ExprVal where
    (==) (ExprDou num1) (ExprDou num2) = num1 == num2
    (==) (ExprBool bool1) (ExprBool bool2) = bool1 == bool2
    (==) (ExprChar char1) (ExprChar char2) = char1 == char2
    (==) (ExprString string1) (ExprString string2) = string1 == string2
    (==) (ExprList list1) (ExprList list2)  = list1 == list2
    (==) (ExprList pair1) (ExprList pair2)  = pair1 == pair2

instance Ord ExprVal where
    (>=) (ExprDou num1) (ExprDou num2) = num1 >= num2
    (>=) (ExprChar char1) (ExprChar char2) = char1 >= char2
    (>=) (ExprString string1) (ExprString string2) = string1 >= string2
    (>=) (ExprList list1) (ExprList list2) = list1 >= list2
    (>=) (ExprCons pair1) (ExprCons pair2) = pair1 >= pair2
    (>) (ExprDou num1) (ExprDou num2) = num1 > num2
    (>) (ExprChar char1) (ExprChar char2) = char1 > char2
    (>) (ExprString string1) (ExprString string2) = string1 > string2
    (>) (ExprList list1) (ExprList list2) = list1 > list2
    (>) (ExprCons pair1) (ExprCons pair2) = pair1 > pair2
    (<=) (ExprDou num1) (ExprDou num2) = num1 <= num2
    (<=) (ExprChar char1) (ExprChar char2) = char1 <= char2
    (<=) (ExprString string1) (ExprString string2) = string1 <= string2
    (<=) (ExprList list1) (ExprList list2) = list1 <= list2
    (<=) (ExprCons pair1) (ExprCons pair2) = pair1 <= pair2
    (<) (ExprDou num1) (ExprDou num2) = num1 < num2
    (<) (ExprChar char1) (ExprChar char2) = char1 < char2
    (<) (ExprString string1) (ExprString string2) = string1 < string2
    (<) (ExprList list1) (ExprList list2) = list1 < list2
    (<) (ExprCons pair1) (ExprCons pair2) = pair1 < pair2
    
evalDou :: ExprVal -> Double
evalDou (ExprDou num) = num
evalBool :: ExprVal -> Bool
evalBool (ExprBool bool) = bool
evalChar :: ExprVal -> Char
evalChar (ExprChar char) = char
evalString :: ExprVal -> String
evalString (ExprString string) = string
evalList :: ExprVal -> [ExprVal]
evalList (ExprList list) = list
evalCons :: ExprVal -> (ExprVal, ExprVal)
evalCons (ExprCons pair) = pair

eval :: Expr -> Env -> ExprVal
eval FalseLit env = ExprBool False
eval TrueLit env = ExprBool True
eval (Var xs) env = if M.member (eval (St xs) env) env then fromJust (M.lookup (eval (St xs) env) env) else ExprString xs
eval (Not p) env = ExprBool (not (evalBool(eval p env)))
eval (And p q) env = ExprBool ((evalBool (eval p env)) && (evalBool (eval q env)))
eval (Or p q) env = ExprBool ((evalBool (eval p env)) || (evalBool (eval q env)))
eval (Eq p q) env = ExprBool ((eval p env) == (eval q env))
eval (Lt p q) env = ExprBool ((evalDou (eval p env)) < (evalDou (eval q env)))
eval (Le p q) env = ExprBool ((evalDou (eval p env)) <= (evalDou (eval q env)))
eval (Gt p q) env = ExprBool ((evalDou (eval p env)) > (evalDou (eval q env)))
eval (Ge p q) env = ExprBool ((evalDou (eval p env)) >= (evalDou (eval q env)))

eval (Dou p) env = ExprDou p
eval (Add p q) env = ExprDou ((evalDou (eval p env)) + (evalDou (eval q env)))
eval (Sub p q) env = ExprDou ((evalDou (eval p env)) - (evalDou (eval q env)))
eval (Mul p q) env = ExprDou ((evalDou (eval p env)) * (evalDou (eval q env)))
eval (Div p q) env = ExprDou ((evalDou (eval p env)) / (evalDou (eval q env)))
eval NilLit env = ExprNil
eval (Cha c) env = ExprChar c
eval (St s) env = ExprString s
eval (Cons (Dou d) NilLit) env = eval (Dou d) env
eval (Cons FalseLit NilLit) env = eval FalseLit env
eval (Cons TrueLit NilLit) env = eval TrueLit env
eval (Cons e1 e2) env = ExprCons ((eval e1 env), (eval e2 env))
eval (Car NilLit) env = ExprNil
eval (Car (Cons e1 e2)) env = eval e1 env
eval (Cdr NilLit) env = ExprNil
eval (Cdr (Cons e1 e2)) env = eval e2 env

getExpr :: Either String Expr -> Env -> Expr
getExpr (Left errStr) env =  NilLit
getExpr (Right expr) env = expr

getStat :: Either String Statement -> Statement
getStat (Left errStr) = Skip
getStat (Right stat) = stat

getPro :: Either String Program -> Program
getPro (Left errStr) = Pro Skip
getPro (Right pro) = pro

genExprTree :: Expr -> Tree String
genExprTree (Var s) = Node s Nil Nil Nil
genExprTree FalseLit = Node "False" Nil Nil Nil
genExprTree TrueLit = Node "True" Nil Nil Nil
genExprTree NilLit = Node "()" Nil Nil Nil
genExprTree (St s) = Node s Nil Nil Nil
genExprTree (Cha c) = Node (c:[]) Nil Nil Nil
genExprTree (Cons (Dou d) NilLit) = genExprTree (Dou d)
genExprTree (Cons FalseLit NilLit) = genExprTree FalseLit
genExprTree (Cons TrueLit NilLit) = genExprTree TrueLit
genExprTree (Cons e1 e2) = Node "cons" (genExprTree e1) (genExprTree e2) Nil
genExprTree (Car NilLit) = Node "()" Nil Nil Nil
genExprTree (Car (Cons e1 e2)) = genExprTree e1
genExprTree (Cdr NilLit) = Node "()" Nil Nil Nil
genExprTree (Cdr (Cons e1 e2)) = genExprTree e2
genExprTree (Not p) = Node "not" (genExprTree p) Nil Nil
genExprTree (And p q) = Node "and" (genExprTree p) (genExprTree q) Nil
genExprTree (Or p q) = Node "or" (genExprTree p) (genExprTree q) Nil
genExprTree (Add p q) = Node "+" (genExprTree p) (genExprTree q) Nil
genExprTree (Sub p q) = Node "-" (genExprTree p) (genExprTree q) Nil
genExprTree (Mul p q) = Node "*" (genExprTree p) (genExprTree q) Nil
genExprTree (Div p q) = Node "/" (genExprTree p) (genExprTree q) Nil
genExprTree (Eq p q) = Node "==" (genExprTree p) (genExprTree q) Nil
genExprTree (Lt p q) = Node "<" (genExprTree p) (genExprTree q) Nil
genExprTree (Le p q) = Node "<=" (genExprTree p) (genExprTree q) Nil
genExprTree (Gt p q) = Node ">" (genExprTree p) (genExprTree q) Nil
genExprTree (Ge p q) = Node ">=" (genExprTree p) (genExprTree q) Nil
genExprTree (Int p) = Node (show p) Nil Nil Nil
genExprTree (Dou p) = Node (show p) Nil Nil Nil


genStatTree :: Statement -> Tree String
genStatTree (Begin p q) = Node "begin" (genStatTree p) (genStatsTree q) Nil
genStatTree Skip = Node "skip" Nil Nil Nil
genStatTree (Set p q) = Node "set" (genExprTree p) (genExprTree q) Nil
genStatTree (If p q r) = Node "if" (genExprTree p) (genStatTree q) (genStatTree r)
genStatTree (While p q) = Node "while" (genExprTree p) (genStatTree q) Nil

genStatsTree :: Statements -> Tree String
genStatsTree NilStat = Node "nil" Nil Nil Nil
genStatsTree (List p q) = Node "statement_list" (genStatTree p) (genStatsTree q) Nil

genProTree :: Program -> Tree String
genProTree (Pro p) = Node "program" (genStatTree p) Nil Nil
genProTree (Cal p) = Node "program" (genExprTree p) Nil Nil

procStat :: Env -> Statement -> Env
procStat env (Begin p q) = (procStats (procStat env p) q)
procStat env (Skip) = env
procStat env (Set (Var xs) p) = (M.insert (eval (St xs) env) (eval p env) env)
procStat env (If expr p q) = if evalBool (eval expr env) then (procStat env p) else (procStat env q)
procStat env (While expr p) = if evalBool (eval expr env) then (procStat (procStat env p) (While expr p)) else env
    
procStats :: Env -> Statements -> Env
procStats env (NilStat) = env
procStats env (List p q) = (procStats (procStat env p) q)

procPro :: Env -> Program -> Either Env ExprVal
procPro env (Pro p) = Left (procStat env p)
procPro env (Cal p) = Right (eval p env)

--defMain :: IO ()
--defMain = do
    
    --putStrLn $ getExpr (parseOnly notParser "(not True)") env
    --putStrLn $ getExpr (parseOnly addParser "(+ 1.2 2.2 )") env 
    --putStrLn $ getExpr (parseOnly mulParser "(* 2 2.2 )") env
    --putStrLn $ getExpr (parseOnly divParser "(/ 10 2 )") env
    --putStrLn $ show $ parseOnly exprParser "12.3"
    --putStrLn $ getExpr (parseOnly charParser "\'a\'" ) env
    --putStrLn $ getExpr (parseOnly stringParser "\"abc\"") env
    --putStrLn $ getExpr (parseOnly consParser "(cons \'a\' \'b\')") env
--    putStrLn "-------"
    --putStrLn $ getStat (parseOnly setParser "(set! a 1)")
    --let env = procPro M.empty (getPro (parseOnly allParser "(begin (set! a 1) (set! b (+ a 1)))")) in putStrLn (show (fromJust (M.lookup (eval (St "b") M.empty) env)))
    --putStrLn (render (doc (genProTree (getPro (parseOnly allParser "(begin (set! a 1) (while (< a 10) (set! a (+ a 1))))")))))
    --putStrLn (evalString (fromJust (M.lookup (eval (St "a") M.empty) env)))