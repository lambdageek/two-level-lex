{-# LANGUAGE ExistentialQuantification, RankNTypes, TupleSections #-}
module TwoLevelLex.Step (run) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.Function
import Data.List hiding (lookup)
import qualified Data.List as L
import Data.Monoid

import TwoLevelLex.AST

import qualified Data.Map as M

newtype Reversed a = Reversed { getReversed :: a }
                     deriving (Eq, Show)
                              
instance Ord a => Ord (Reversed a) where
  compare x y = oppOrd ((compare `on` getReversed) x y)
    where oppOrd LT = GT
          oppOrd GT = LT
          oppOrd EQ = EQ
  

consR :: Reversed [a] -> a -> Reversed [a]
consR r x = Reversed (x : getReversed r)



data Value =
  LitV Literal
  | ContV ValEnv [Name] Expr
  | TupleV [Value]
    deriving Show

data Frame =
  TupleF (Reversed [Value]) [Expr]
  | ProjF Nat
  | LetF ValBind Expr
    deriving Show
             
newtype ValEnv = ValEnv { envMap :: M.Map Name Value }
                 deriving Show

emptyEnv :: ValEnv
emptyEnv = ValEnv mempty


type Stack = [Frame]
haltStk :: Stack
haltStk = []

viewStk :: Stack -> Maybe (Frame, Stack)
viewStk [] = Nothing
viewStk (x:xs) = Just (x, xs)

data Op =
  EvalO Expr
  | RetO Value

data StState =
  StState {
    _stEnv :: ValEnv 
    , _stStk :: Stack
    }
  
type Lns a b = forall f . Functor f => (b -> f b) -> a -> f a

stEnv :: Lns StState ValEnv
stEnv f (StState env stk) = fmap (\x -> StState x stk) $ f env

stStk :: Lns StState Stack
stStk f (StState env stk) = fmap (\x -> StState env x) $ f stk

uses :: MonadState s m => Lns s a -> (a -> r) -> m r
uses l r = gets (getConst . asks l (Const . r))

use :: MonadState s m => Lns s a -> m a
use l = uses l id

modifies :: MonadState s m => Lns s a -> (a -> (r, a)) -> m r
modifies l f = state (l f)

type StepM m = StateT StState (ErrorT String m)


returnO :: Monad m => Value -> StepM m Op
returnO = return . RetO

evalO :: Monad m => Expr -> StepM m Op
evalO = return . EvalO

lookup :: Monad m => Name -> StepM m Value
lookup x = do
  mv <- uses stEnv (M.lookup x . envMap)
  case mv of
    Nothing -> throwError $ "unbound name " ++ x
    Just v -> return v

store :: Monad m => Name -> Value -> StepM m ()
store x v =
  modifies stEnv $ ((),) . ValEnv . M.insert x v . envMap

replaceEnv :: Monad m => ValEnv -> StepM m ()
replaceEnv env =
  modifies stEnv (((), ) . const env)

push :: Monad m => Frame -> StepM m ()
push f = modifies stStk (\stk -> ((), f : stk))

pop :: Monad m => StepM m (Maybe Frame)
pop = do
  modifies stStk $ \stk ->
    case viewStk stk of
      Nothing -> (Nothing, stk)
      Just (f, stk') -> (Just f, stk')

peek :: Monad m => StepM m (Maybe Frame)
peek = do
  mf <- pop
  case mf of
    Just f -> push f >> return (Just f)
    Nothing -> return Nothing

step :: Monad m => Op -> StepM m Op
step (EvalO e) = evaluate e
step (RetO v) = do
  mf <- pop 
  case mf of
    Nothing -> throwError "halted machine cannot step"
    Just f -> v `returnTo` f

isHalted :: Monad m => Op -> StepM m (Maybe Value)
isHalted (RetO v) = do
  mf <- peek
  case mf of
    Nothing -> return $ Just v
    Just _ -> return Nothing
isHalted (EvalO _) = return Nothing
    
evaluate :: Monad m => Expr -> StepM m Op
evaluate (LitE l) = returnO $ LitV l
evaluate (LabelE bs e) = do
  env <- use stEnv
  returnO $ ContV env (map fst bs) e
evaluate (VarE x) = do
  v <- lookup x
  returnO v
evaluate (TupleE []) = returnO $ TupleV []
evaluate (TupleE (e:es)) = do
  push $ TupleF (Reversed []) es
  evalO e
evaluate (ProjE n e) = do
  push $ ProjF n
  evalO e
evaluate (LetE b e1 e2) = do
  push $ LetF b e2
  evalO e1
evaluate (AsmE bs is) = execute is bs

execute :: Monad m => [Asm] -> [ValBind] -> StepM m Op
execute [] bs = evalO . TupleE $ map (VarE . fst) bs
execute (AddI x1 x2 x3 : is) bs = do
  v1 <- lookup x1
  v2 <- lookup x2
  v3 <- case (v1, v2) of
    (LitV i1, LitV i2) -> return . LitV $ i1 + i2
    _ -> throwError "addition expected two integer literals"
  () <- case L.lookup x3 bs of
    Just _ty -> return ()
    Nothing -> throwError $ "expected " ++ show x3 ++ " to be bound by asm"
  store x3 v3
  evalO (AsmE bs is)
execute (BrI cnd x t f : _is) _ = do
  v <- lookup x
  b <- evalCond cnd v
  applyLabel (if b then t else f)

evalCond :: Monad m => BranchCond -> Value -> StepM m Bool
evalCond c (LitV i) =
  case c of
    LtBC -> return $ i < 0 
    EqBC -> return $ i == 0
evalCond _c _v = throwError "expected integer value in evalCond"

applyLabel :: Monad m => (Name, [Name]) -> StepM m Op
applyLabel (l, args) = do
  c <- lookup l
  vs <- mapM lookup args
  (env, ps, expr) <- case c of
    (ContV env ps expr) -> return (env, ps, expr)
    _ -> throwError "expected continuation"
  replaceEnv env
  sequence_ $ zipWith store ps vs
  evalO expr

returnTo :: Monad m => Value -> Frame -> StepM m Op
returnTo v (LetF b e) = do
  store (fst b) v
  evalO e
returnTo v (TupleF vs []) = returnO . TupleV . reverse $ v : getReversed vs
returnTo v (TupleF vs (e:es)) = do
  push $ TupleF (vs `consR` v) es
  evalO e
returnTo (TupleV vs) (ProjF n) | (unNat n) < genericLength vs =
  returnO (genericIndex vs $ unNat n)
returnTo f v =
  throwError $ "unexpected return of " ++ show v ++ "to frame " ++ show f
  
initialState :: Program -> (Op, StState)
initialState p = (EvalO (programExpr p), StState emptyEnv haltStk)

steps :: Monad m => Op -> StepM m Value
steps o = do
  mv <- isHalted o
  case mv of
    Just v -> return v
    _ -> step o >>= steps

run :: Program -> Either String Value
run p = let
  (o, st) = initialState p
  act = steps o
  in
   runIdentity $ runErrorT $ evalStateT act st