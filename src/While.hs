module While where

import Parser
import BoolExpr
--import Calculate
import qualified Data.Map as M

type Env = M.Map String String

procStat :: Env -> Statement -> Env
procStat env (Begin p q) = (proc (procStat env p) q)
procStat env (Skip) = env
procStat env (Set var p) = (M.insert "a" "b" env)
procStat env (If expr p q) = if (eval expr) then (procStat env p) else (procStat env q)
procStat env (While expr p) = if (eval expr) then (procStat env p) else env
    
proc :: Env -> Statements -> Env
proc env Nil = env
proc env (List p q) = (proc (procStat env p) q)