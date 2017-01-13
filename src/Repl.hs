module REPL where

import qualified Data.Map as M
import System.IO
import Data.Attoparsec.Text
import Data.Functor

type Env = M.Map String String

getString :: [[Char]] -> [Char]
getString x
    | length x == 1 = head x
    | otherwise = head x ++ " " ++ getString (tail x) 

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "> "
    hFlush stdout
    l <- getLine
    case words l of
        ["set",var,val] -> do
            putStrLn (var ++ " is set to " ++ val)
            mainLoop (M.insert var val env)
        "set":var:val -> do
            putStrLn (var ++ " is set to " ++ (getString val))
            mainLoop (M.insert var (getString val) env)
        ["view",var] -> case M.lookup var env of
            Just val -> do
                putStrLn (var ++ " = " ++ val)
                mainLoop env
            Nothing -> do
                putStrLn "variable not found!"
                mainLoop env
        ["exit"] -> putStrLn "Bye~"
        _ -> do
            putStrLn "unrecognized command!"
            mainLoop env
            
defMain :: IO ()
defMain = do
    putStrLn "This is a simple REPL"
    mainLoop (M.empty)