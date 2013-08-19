module TwoLevelLex.AST 
       (
         -- * Naturals
         Nat, nat, unNat,
         -- * Types
         Ty(..),
         -- * Names and binding
         Name,
         ValBind,
         -- * Expressions
         Literal,
         Expr(..),
         -- * Inline asm
         Asm(..),
         BranchCond(..),
         -- * Programs
         Program(..)
       )
       where


newtype Program = Program { programExpr :: Expr }
                deriving Show
                           
newtype Nat = Nat { unNat :: Integer }
              deriving (Eq, Show, Ord)

nat :: Integer -> Nat
nat i | i >= 0 = Nat i
      | otherwise = error $ "nat: negative integer " ++  show i

type Name = String

type ValBind = (Name, Ty)

type Literal = Integer

data Expr =
  VarE Name
  | LitE Literal
  | TupleE [Expr]
  | ProjE Nat Expr
  | LetE ValBind Expr Expr
  | AsmE [ValBind] [Asm]
  | LabelE [ValBind] Expr
  deriving Show
  
data Ty =
  IntT
  | LabelT (Ty {- → ⊥ -})
  | ProdT [Ty]
  deriving Show

  
data Asm = 
  AddI Name Name Name
  | BrI BranchCond Name (Name, [Name])  (Name, [Name])
    deriving Show
             
data BranchCond = 
  LtBC
  | EqBC
    deriving (Show, Eq)
             
data Reg = R1 | R2 | R3 | R4    
            deriving (Eq, Show, Ord, Bounded, Enum)