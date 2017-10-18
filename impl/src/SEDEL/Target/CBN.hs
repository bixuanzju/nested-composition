
module SEDEL.Target.CBN
  ( Env
  , evaluate
  , extendCtx
  , emptyEnv
  ) where

import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Unbound.Generics.LocallyNameless

import           SEDEL.Common
import           SEDEL.PrettyPrint
import           SEDEL.Target.Syntax

data ClosureExp = CExp UExpr Env deriving Show

type Env = M.Map UName ClosureExp

emptyEnv :: Env
emptyEnv = M.empty

extendCtx :: (UName, UExpr, Env) -> Env -> Env
extendCtx (n, e, env) = M.insert n (CExp e env)

data Value = VLit Double
           | VBool Bool
           | VStr String
           | VPair ClosureExp ClosureExp
           | VUnit
           | VClosure (Bind UName UExpr) Env

instance Show Value where
  show (VLit n) = show n
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VPair (CExp e1 _) (CExp e2 _)) = "(" ++ show (pprint e1) ++ ", " ++ show (pprint e2) ++ ")"
  show VUnit = "()"
  show (VStr s) = show s
  show VClosure{} = "<<closure>>"

type M = FreshMT (Reader Env)


evaluate :: Env -> UExpr -> Value
evaluate env e = runReader (runFreshMT (eval e)) env


eval :: UExpr -> M Value
eval (UVar x) = do
  Just (CExp e env) <- asks (M.lookup x)
  local (const env) $ eval e
eval (UApp e1 e2) = do
  VClosure b env' <- eval e1
  (x, body) <- unbind b
  env <- ask
  local (const $ M.insert x (CExp e2 env) env') (eval body)
eval (ULam b) = do
  env <- ask
  return $ VClosure b env
eval (ULet b) = do
  (x, (e, body)) <- unbind b
  env <- ask
  let env' = M.insert x (CExp e env') env -- Recursive let binding
  local (M.insert x (CExp e env')) $ eval body
eval (UPair e1 e2) = do
  env <- ask
  return $ VPair (CExp e1 env) (CExp e2 env)
eval (UP1 e) = do
  VPair (CExp e1 env') _ <- eval e
  local (const env') $ eval e1
eval (UP2 e) = do
  VPair _ (CExp e2 env') <- eval e
  local (const env') $ eval e2
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
