module Main where

import TwoLevelLex.AST
import TwoLevelLex.Check
import TwoLevelLex.Step

parse :: IO Program
parse = return $ Program (LetE ("x", IntT) (LitE 15) $
                          LetE ("y", IntT) (LitE 12) $ 
                          LetE ("c0", FunT [] (ProdT [IntT]))
                          (LamE [] $
                           LetE ("t", ProdT [IntT]) (AsmE [("z", IntT)]
                                                     [
                                                       AddI "x" "y" "z"
                                                     , AddI "x" "z" "z"
                                                     ]) $
                           VarE "t") $
                          LetE ("zero", IntT) (LitE 0) $
                          LetE ("r", ProdT [IntT]) (AsmE [("_dk", IntT)] [ BrI EqBC "zero" ("c0", []) ("c0", []) ]) $
                          ProjE (nat 0) (VarE "r"))
                                                     

main :: IO ()
main = do
     e <- parse
     print $ show e
     case typeCheck e of
       Left err -> print $ "Typecheck Error: " ++ err
       Right () -> print $ "Typechecked Okay"
     case run e of
       Left err -> print $ "Step Error: " ++ err
       Right v -> print $ "Halted with value: " ++ show v
     