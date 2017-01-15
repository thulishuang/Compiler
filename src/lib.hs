module Lib where

import REPL
import Parser
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import System.Environment

import System.IO
import Data.Char(toUpper)
import Data.Text(pack)
import Data.Maybe
import Data.Attoparsec.Text
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

data Option = Option {
    inPath :: String,
    outPath :: String,
    optType :: String
}
    deriving Show

type ParserI a = StateT [String] Maybe a

parseFlag :: String -> ParserI String
parseFlag f = do
    args <- get
    case args of
        [] -> Control.Applicative.empty
        (arg : args')
            | arg == "--" ++ f -> do
                put args'
                return f
            | otherwise -> Control.Applicative.empty
            
parseField :: String -> ParserI String
parseField f = do
    parseFlag f
    args <- get
    case args of
        [] -> Control.Applicative.empty
        (arg : args') -> do
            put args'
            return arg
            
parseInPath :: ParserI String
parseInPath = parseField "in"

parseOutPath :: ParserI String
parseOutPath = parseField "out"

parseTreePath :: ParserI String
parseTreePath = parseField "tree"

parseReplPath :: ParserI String
parseReplPath = parseField "repl"

--parseOption :: Parser Option
parseOption = p0 <|> p1 <|> p2 <|>p3 <|> p4 <|> p5 <|>p6 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (Option i o "in")

    p1 = do
        i <- parseInPath
        return (Option i "" "in")
        
    p2 = do
        o <- parseOutPath
        i <- parseInPath
        return (Option i o "in")

    p3 = do
        i <- parseTreePath
        return (Option i "" "tree")

    p4 = do
        i <- parseTreePath
        o <- parseOutPath
        return (Option i o "tree")

    p5 = do
        o <- parseOutPath
        i <- parseTreePath
        return (Option i o "tree")

    p6 = do
        return (Option "" "" "repl")

--processFile :: Option -> IO()
processFile (Option inPath outPath optType,strlis) = do
    --if (inPath != "") && (outPath != "") then 
    --    do inh <- openFile inPath ReadMode
    --       ouh <- openFile outPath WriteMode
    case optType of
        "in" -> do inh <- openFile inPath ReadMode
                   if outPath == ""
                       then do stdinProcessLine inh
                               hClose inh
                               --hClose ouh
                       else do ouh <- openFile outPath WriteMode
                               inProcessLine inh ouh
                               hClose inh
                               hClose ouh
        "tree" -> do inh <- openFile inPath ReadMode
                     if outPath == ""
                         then do stdtreeProcessLine inh
                                 hClose inh
                               --hClose ouh
                         else do ouh <- openFile outPath WriteMode
                                 treeProcessLine inh ouh
                                 hClose inh
                                 hClose ouh
        "repl" -> putStrLn "This is a simple REPL. Be my guest!"

inProcessLine :: Handle -> Handle -> IO()
inProcessLine inh ouh = 
    do isEof <- hIsEOF inh
       if isEof 
            then return()
            else do lineStr <- hGetLine inh
                    hPutStrLn ouh $ show $ procPro M.empty (getPro (parseOnly allParser (pack lineStr))) 
                    inProcessLine inh ouh

stdinProcessLine :: Handle -> IO()
stdinProcessLine inh = 
    do isEof <- hIsEOF inh
       if isEof 
            then return()
            else do lineStr <- hGetLine inh
                    putStrLn $ show $ procPro M.empty (getPro (parseOnly allParser (pack lineStr))) 
                    stdinProcessLine inh 

treeProcessLine :: Handle -> Handle -> IO()
treeProcessLine inh ouh = 
    do isEof <- hIsEOF inh
       if isEof 
            then return()
            else do lineStr <- hGetLine inh
                    hPutStrLn ouh (render (doc (genProTree (getPro (parseOnly allParser (pack lineStr))))))
                    treeProcessLine inh ouh

stdtreeProcessLine :: Handle -> IO()
stdtreeProcessLine inh = 
    do isEof <- hIsEOF inh
       if isEof 
            then return()
            else do lineStr <- hGetLine inh
                    putStrLn (render (doc (genProTree (getPro (parseOnly allParser (pack lineStr))))))
                    stdtreeProcessLine inh 

defMain :: IO ()
defMain = do
    args <- getArgs
    print args
    case args of
        ["--repl"] -> do
            putStrLn "Welcome to REPL mode. Be my guest!"
            mainLoop (M.empty) ""
        _ -> do
            processFile $ fromJust (runStateT parseOption args)
    print $ fromJust (runStateT parseOption args)