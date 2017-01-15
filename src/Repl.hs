{-# LANGUAGE OverloadedStrings #-}

module REPL where

import qualified Data.Map as M
import System.IO
import Data.Attoparsec.Text
import Data.Functor
import Data.Text.Internal
import Data.Text
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import Data.Either
import Parser

errorhandle :: Env -> String -> Env
errorhandle env str = env


getWord :: [[Char]] -> [Char]
getWord x
    | Prelude.length x == 1 = Prelude.head x
    | otherwise = Prelude.head x ++ " " ++ getWord (Prelude.tail x) 

--procPro :: Env -> Program -> Either Env ExprVal
--procPro env (Pro p) = Left (procStat env p)
--procPro env (Cal p) = Right (eval p env)

--getProEnv :: Either Env ExprVal -> 

getLeft :: Either Env ExprVal -> Env
getLeft (Left env) = env
getLeft (Right s) = M.empty

getRight :: Either Env ExprVal -> ExprVal
getRight (Left x) = ExprString "error"
getRight (Right x) = x

mainLoop :: Env -> String -> IO ()
mainLoop env lastSentence = do
    putStr "> "
    hFlush stdout
    l <- getLine
    case Prelude.words l of
        ":i":pro -> do
            if isLeft res
                then do
                    putStrLn "program complete"
                    mainLoop (getLeft res) (getWord pro)
                else do
                    putStrLn "eval complete"
                    putStrLn $ show (getRight res)
                    mainLoop env (getWord pro)
            where res = (procPro env (getPro (parseOnly allParser (pack (getWord pro)))))
        [":t"] -> do 
            if lastSentence == "" 
                then do 
                    putStrLn "error"
                    mainLoop env ""
                else do 
                    putStrLn (render (doc (genProTree (getPro (parseOnly allParser (pack lastSentence))))))
                    mainLoop env lastSentence
        [":q"] -> putStrLn "Bye~"
        _ -> do
            putStrLn "unrecognized command!"
            mainLoop env lastSentence