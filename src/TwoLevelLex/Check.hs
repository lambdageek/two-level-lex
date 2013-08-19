{-# LANGUAGE FlexibleContexts #-}
module TwoLevelLex.Check (typeCheck) where

import Prelude hiding (lookup)

import Control.Monad.Reader.Class
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Functor

import Data.List hiding (lookup)  
import qualified Data.Map as M

import TwoLevelLex.AST

newtype TyEnv = TyEnv { getTyEnv :: M.Map Name Ty }

newtype ModEnv = ModEnv { getModEnv :: M.Map Name Ty }

type C m = ReaderT TyEnv (ErrorT String m)

type CA m = ReaderT ModEnv m

lookup :: Monad m => Name -> C m Ty
lookup x = do
  mt <- asks (M.lookup x . getTyEnv)
  case mt of
    Just ty -> return ty
    Nothing -> throwError $ "unbound variable " ++ show x

bind :: Name -> Ty -> TyEnv -> TyEnv
bind x t = TyEnv . M.insert x t . getTyEnv

checkLit :: Monad m => Literal -> C m Ty
checkLit _ = return IntT

checkExpr :: (Functor m, Monad m) => Expr -> C m Ty
checkExpr (VarE x) = lookup x
checkExpr (LitE l) = checkLit l
checkExpr (TupleE es) = ProdT <$> mapM checkExpr es
checkExpr (ProjE n e) = do
  t <- checkExpr e
  case t of
    ProdT ts
      | unNat n < genericLength ts -> return $ genericIndex ts (unNat n)
      | otherwise -> throwError $ "product type " ++ show t 
                     ++ " doesn't have an " ++ show n ++ "th component"
    _ -> throwError $ "expected a product type, got " ++ show t
checkExpr (LetE (x,t0) e1 e2) = do
  t <- checkExpr e1
  unless (t0 == t) $ throwError $ "expected expression of type " ++ show t0 ++ ", got " ++ show t
  local (bind x t) $ checkExpr e2
checkExpr (AsmE bs is) = do
  withModifiable bs $ checkAsm is
  return . ProdT $ map snd bs
checkExpr (LabelE bs e) = do
  void $ local (flip (foldl (\g (x,t) -> bind x t g)) bs) $ checkExpr e
  return . LabelT $ map snd bs

modifiableEnv :: [ValBind] -> ModEnv
modifiableEnv = ModEnv . M.fromList

withModifiable :: Monad m => [ValBind] -> CA (C m) a -> C m a
withModifiable bs act = runReaderT act (modifiableEnv bs)

lookupAsm :: Monad m => Name -> CA (C m) Ty
lookupAsm x = do
  mt <- asks (M.lookup x . getModEnv)
  case mt of
    Just t -> return t
    Nothing -> lift $ lookup x

checkStore :: MonadError String m => Name -> Ty -> CA m ()
checkStore x t0 = do
  mt <- asks (M.lookup x . getModEnv)
  case mt of
    Just t1 | t0 == t1 -> return ()
            | otherwise -> throwError $ "expected assignment to " ++  show x 
                           ++ " with type " ++ show t0
                           ++ ", but got " ++  show t1
    Nothing -> throwError $ "unbound modifiable variable " ++ show x
    
checkAsm :: Monad m => [Asm] -> CA (C m) ()
checkAsm [] = return ()
checkAsm (AddI x1 x2 x3 : is) = do
  t1 <- lookupAsm x1
  t2 <- lookupAsm x2
  unless (t1 == IntT && t2 == IntT) $ throwError "expected integer arguments to AddI"
  checkStore x3 IntT
  checkAsm is
checkAsm (BrI cond x t f : []) = do
  checkCond cond x
  checkApply t
  checkApply f
checkAsm (BrI _ _ _ _ : _is ) = throwError "unexpected instructions after branch"

checkCond :: Monad m => BranchCond -> Name -> CA (C m) ()
checkCond _cond x = do
  t <- lookupAsm x
  case t of
    IntT -> return ()
    _ -> throwError $ "expected conditional branch with integer argument, got " ++  show t

checkApply :: Monad m => (Name, [Name]) -> CA (C m) ()
checkApply (x, args) = do
  t <- lookupAsm x
  case t of
    LabelT ts -> zipWithM_ checkArg args ts
    _ -> throwError $ "expected " ++ show x ++ "  of label type, got " ++ show t

checkArg :: Monad m => Name -> Ty -> CA (C m) ()
checkArg x t0 = do
  t1 <- lookupAsm x
  unless (t0 == t1) $
    throwError $ "expected " ++ show x ++ " to have type " ++ show t0
    ++ "in application, but it has type " ++ show t1

checkProgram :: (Functor m, Monad m) => Program -> C m ()
checkProgram p = void $ checkExpr (programExpr p)

typeCheck :: Program -> Either String ()
typeCheck p = let
  act = checkProgram p
  env0 = TyEnv M.empty
  in
   runIdentity $ runErrorT $ runReaderT act env0