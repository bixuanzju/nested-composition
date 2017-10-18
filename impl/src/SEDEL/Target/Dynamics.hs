{-# LANGUAGE RecursiveDo #-}


module SEDEL.Target.Dynamics where

import Control.Monad.Reader
import Unbound.Generics.LocallyNameless
import qualified Data.Map as M

import SEDEL.Common
import SEDEL.Target.Syntax


------------------------
-- big-step evaluation
------------------------

type Env = M.Map UName Value


data Value = VLit Double
           | VBool Bool
           | VStr String
           | VPair Value Value
           | VUnit
           | VClosure (Bind UName UExpr) Env

instance Show Value where
  show (VLit n) = show n
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show VUnit = "()"
  show (VStr s) = show s
  show VClosure{} = "<<closure>>"


type M = FreshMT (Reader Env)

evaluate :: UExpr -> Value
evaluate e = runReader (runFreshMT (eval e)) M.empty

-- test :: UExpr
-- test = UApp (ULam (bind (s2n "x") (ULitV 3))) Bot

eval :: UExpr -> M Value
eval (UVar x) = do
  Just v <- asks (M.lookup x)
  return v
eval (UApp e1 e2) = do
  VClosure b env' <- eval e1
  (x, body) <- unbind b
  v2 <- eval e2
  local (const $ M.insert x v2 env') $ eval body
eval (ULam b) = do
  ctx <- ask
  return $ VClosure b ctx
eval (ULet b) = mdo
  (x, (e, body)) <- unbind b
  v <- local (M.insert x v) $ eval e -- Recursive let binding
  local (M.insert x v) $ eval body
eval (UPair e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ VPair v1 v2
eval (UP1 e) = do
  VPair v1 _ <- eval e
  return v1
eval (UP2 e) = do
  VPair _ v2 <- eval e
  return v2
eval (ULitV n) = return $ VLit n
eval (UBoolV n) = return $ VBool n
eval (UStrV n) = return $ VStr n
eval UUnit = return VUnit
eval (UPrimOp op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ evalOp op v1 v2
eval (UIf e1 e2 e3) = do
  (VBool v) <- eval e1
  if v then eval e2 else eval e3
eval (UToString e) = do
  v <- eval e
  return $ VStr (show v)
eval (USqrt e) = do
  (VLit n) <- eval e
  return $ VLit (sqrt n)

evalOp :: Operation -> Value -> Value -> Value
evalOp (Arith Add) (VLit x) (VLit y) = VLit $ x + y
evalOp (Arith Sub) (VLit x) (VLit y) = VLit $ x - y
evalOp (Arith Mul) (VLit x) (VLit y) = VLit $ x * y
evalOp (Arith Div) (VLit x) (VLit y) = VLit $ x / y
evalOp (Comp Equ) (VLit x) (VLit y) = VBool $ x == y
evalOp (Comp Equ) (VStr x) (VStr y) = VBool $ x == y
evalOp (Comp Equ) (VBool x) (VBool y) = VBool $ x == y
evalOp (Comp Lt) (VLit x) (VLit y) = VBool $ x < y
evalOp (Comp Gt) (VLit x) (VLit y) = VBool $ x > y
evalOp (Comp Neq) (VLit x) (VLit y) = VBool $ x /= y
evalOp (Comp Neq) (VStr x) (VStr y) = VBool $ x /= y
evalOp (Comp Neq) (VBool x) (VBool y) = VBool $ x /= y
evalOp (Logical LAnd) (VBool x) (VBool y) = VBool $ x && y
evalOp (Logical LOr) (VBool x) (VBool y) = VBool $ x || y
evalOp Append (VStr x) (VStr y) = VStr $ x ++ y
evalOp _ _ _ = error "Impossible happened in evalOp"
