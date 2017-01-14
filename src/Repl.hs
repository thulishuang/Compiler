module REPL where

import qualified Data.Map as M
import System.IO
import Data.Attoparsec.Text
import Data.Functor
import Data.Text.Internal
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import While
import Parser
import PrettyTreePrint

type Env = M.Map String String

errorhandle :: Env -> String -> Env
errorhandle env str = env

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
        ":i":pro -> do
            mainLoop (either (errorhandle env) (procStat env) (parseOnly statParser (getWord pro)))
        [":t"] -> putStrLn "To do"
        [":q"] -> putStrLn "Bye~"
        _ -> do
            putStrLn "unrecognized command!"
            mainLoop env

-- REPL
defMain :: IO ()
defMain = do
    putStrLn "This is a simple REPL"
    mainLoop (M.empty)