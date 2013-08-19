module Main where

import TwoLevelLex.AST
import TwoLevelLex.Step

parse :: IO Program
parse = return $ Program (LetE ("x", IntT) (LitE 42) (VarE "x"))

main :: IO ()
main = do
     e <- parse
     print $ show e
     case run e of
       Left err -> print $ "Step Error: " ++ err
       Right v -> print $ "Halted with value: " ++ show v
     