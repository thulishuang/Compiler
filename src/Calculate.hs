{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Parser

data Number
    = Integer
    | Double
    deriving Show