module Main where

import TwoLevelLex.AST
import TwoLevelLex.Check
import TwoLevelLex.Step
import qualified TwoLevelLex.Parse as P

testProg :: IO Program
testProg = return $ Program (LetE ("x", IntT) (LitE 15) $
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

parse :: IO Program
parse = case res of
  Left err -> fail $ "error : " ++ show err
  Right p -> return p
  where res = P.parseString
              $ unlines ["let x : int = 15 in "
                        , "let y : int = 12 in "
                        , "let c0 : () -> [int] = "
                        , "\\ () . let t : [int] = "
                        , "  asm (z : int) ( "
                        , "    add x y z "
                        , "    add x z z "
                        , "  ) in "
                        , "  t in"
                        , "let zero : int = 0 in "
                        , "let r : [int] = asm (dk : int) ("
                        , "    breq zero c0() c0() "
                        , "  ) in "
                        , " #0 r "]

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
     
