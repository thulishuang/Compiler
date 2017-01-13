{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module BoolExpr where

import Parser
import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Data.Text

eval :: Expr -> Bool
eval FalseLit = False
eval TrueLit = True
eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)

-- designed for parseOnly
--   :: Data.Attoparsec.Text.Parser a
--      -> Data.Text.Internal.Text -> Either String a
evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr

-- defMain :: IO ()
-- defMain = do
--    putStrLn $ show $ parseOnly notParser "(not True)"
--    putStrLn $ show $ parse notParser "(not True)"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly notParser "(nXXX True)"
--    putStrLn $ show $ parse notParser "(nXXX True)"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly notParser "(not Tr"
--    putStrLn $ show $ parse notParser "(not Tr"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly notParser "(not True)   MORE"
--    putStrLn $ show $ parse notParser "(not True)   MORE"
--    putStrLn "--------------"
--    putStrLn $ show $ parseOnly exprParser "(not True)"
--    putStrLn $ show $ parse exprParser "(not True)"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly exprParser "(nXXX True)"
--    putStrLn $ show $ parse exprParser "(nXXX True)"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly exprParser "(not Tr"
--    putStrLn $ show $ parse exprParser "(not Tr"
--    putStrLn "-------"
--    putStrLn $ show $ parseOnly exprParser "(not True)   MORE"
--    putStrLn $ show $ parse exprParser "(not True)   MORE"
--    putStrLn "--------------"
--    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "((not True) or False)"
--    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not Tr"
--    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(nXXX True)"
--    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not True)   MORE"