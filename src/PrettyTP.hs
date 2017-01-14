{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PrettyTP where

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

data Tree a = Nil | Node a (Tree a) (Tree a) (Tree a)
    deriving (Show, Generic, Out)
    
genTree :: String -> Tree String
genTree n
	= Node n t t t
	where t = genTree n
    
printDoc :: IO ()
printDoc = putStrLn (render (doc (genTree "S")))

-- 语法树
-- defMain = printDoc