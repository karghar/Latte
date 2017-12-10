module TypeChecker where

import Control.Monad.State
import Control.Monad.Except
--import Control.Monad.Trans.Except

import System.IO ( stdin, hGetContents, hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Exit
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import Data.Int
import Control.Monad


import qualified Data.Map as M

type TypeError = String

data ClassDeff = ClassDeff
              { className ::  String
              , parent :: Maybe (String)
              , attr :: M.Map Ident Type
              }

data Env = Env
           { locals :: M.Map Ident Type
           , globals :: M.Map Ident Type
           , classes :: M.Map Ident ClassDeff
           }
type Eval = ExceptT (Error) (State Env)

data Error
  = TypeError Expr
  | Redecl Ident
  | DummyError String
  | IntOutofBounds Integer
  | SubstractionTypeError Expr
  | AdditionTypeError Expr
  | ComparisonTypeError Expr
  | OrAndTypeError Expr Expr
  | TypeMismatch Type Type
  | MainFunctionMissing
  | MainFunctionArgsMismatch
  | MainFunctionRetTypeMismatch
  deriving (Eq, Ord, Show)


evalTypes :: Program -> Either Error ()
evalTypes prog =
    fmap (const ()) (runEval initStore (evalProg prog))
--evalTypes prog = do
--  res<- runEval initStore (evalProg prog)
--  return res



--
--compileProg :: Program -> Err String
--compileProg prog = do
--   (_, state) <- runEval initStore (evalProg prog)
--   return "ABBA"
--runEval:: Env -> Eval a -> (Either Error Env)


runEval s e = evalState (runExceptT e) s

initStore :: Env
initStore = Env M.empty M.empty M.empty

evalProg :: Program -> Eval ()
evalProg (Prog topdefs) = do
    env <- get
    prepared <- prepareTopDefs topdefs
    validateMain topdefs
    checkWholeDefs topdefs

prepareTopDefs :: [TopDef] -> Eval Env
prepareTopDefs = undefined
--TODO wczytaj wszystkie klasy


checkWholeDefs :: [TopDef] -> Eval ()
checkWholeDefs (def:topdefs) = do
    env <- get
    checkTopDef def
    put env
    checkWholeDefs topdefs
checkWholeDefs [] = return ()


checkTopDef :: TopDef -> Eval ()
checkTopDef (FnDef func) =
    checkFuncDef func
checkTopDef (ClassDef name classDef) =
--zastanów sie czy nazwa parenta, czy wyciagamy parenta czy tylko nazwa
-- toposorta uzyj you fool
    checkClassDef name Nothing classDef
checkTopDef (ClassDefExt name parent classDef) =
    checkClassDef name (Just parent) classDef


checkClassDef :: Ident -> Maybe Ident -> CDef -> Eval ()
checkClassDef ident parentIdent classDef = undefined



--TODO doprowadz chociaz do uzycia bloku
checkFuncDef :: FuncDef -> Eval ()
checkFuncDef (FunDef typ ident args block) = undefined





validateMain :: [TopDef] -> Eval ()
validateMain [] = throwError MainFunctionMissing
validateMain (FnDef(FunDef Int (Ident "main") [] _):_) = return ()
validateMain (FnDef(FunDef _ (Ident "main") [] _):_) = throwError MainFunctionRetTypeMismatch
validateMain (FnDef(FunDef _ (Ident "main") _ _):_) = throwError MainFunctionArgsMismatch
validateMain (_:tl) = validateMain tl


findType :: Expr -> Eval Type
--TODO sprawdzenie czy taki typ istnieje
findType (ECastNull t) =
    return t
--findType (EAttrAccess AttrAccess) =
--findType (EMthCall MethodCall) =
--method in scope
--arguments
--findType (EArrAccess ArrElemAccess) =

findType (EVar ident) =
--check if type exists in scope
    return (Obj ident)

findType (ENewArr t exp) =
--check if type exists in scope
-- check expr type
--    checkType exp Int
    return (Arr t)

findType (ENew ident) =
--check if type exists in scope
    return (Obj ident)
findType (ELitInt n) =
    if ((fromIntegral (minBound :: Int32)) > n)
          && ((fromIntegral (maxBound :: Int32)) < n) then
        return Int
    else
        throwError (IntOutofBounds n)

findType (ELitTrue) =
    return Bool

findType (ELitFalse) = return Bool


--findType (EApp (FunctionCall ident args)) =
-- sprawdz czy funkcja defined
-- sprawdz czy typ zwracany sie zgadza
-- sprawdz czy typy argumentow sie zgadzaja


findType (EString _) =
    return Str

findType (Neg expr) =
    checkType expr Int

findType (Not expr) =
    checkType expr Bool

findType (EMul lhs _ rhs) = do
    checkType lhs Int >> checkType rhs Int

findType exp@(EAdd lhs Plus rhs) = do
    joinedType <- checkType rhs =<< (findType lhs)
    let cond = (joinedType `elem` [Int, Str])
    let throwMsg = (AdditionTypeError exp)
    unless cond (throwError throwMsg)
    return joinedType

findType exp@(EAdd lhs Minus rhs) = do
    joinedType <- checkType rhs =<< (findType lhs)
    let cond  = (joinedType == Int)
    let throwMsg = (SubstractionTypeError exp)
    unless cond (throwError throwMsg)
    return joinedType


findType (ERel lhs EQU rhs) =
    checkType rhs =<< (findType lhs)
findType (ERel lhs NE rhs) =
    checkType rhs =<< (findType lhs)
findType exp@(ERel lhs op rhs) = do
    joinedType <- checkType rhs =<< (findType lhs)
    unless (joinedType == Int) (throwError (ComparisonTypeError exp))
    return joinedType

findType (EAnd lhs rhs) =
    findEAndOr lhs rhs

findType (EOr lhs rhs) =
    findEAndOr lhs rhs

findType _ = throwError (DummyError "Lel fail")


findEAndOr lhs rhs = do
    joinedType <- checkType rhs =<< (findType lhs)
    let cond = (joinedType == Bool)
    let throwMsg = (OrAndTypeError lhs rhs)
    unless cond  (throwError throwMsg)
    return joinedType

checkType:: Expr -> Type -> Eval Type
checkType exp expectedType = do
    resType <- findType exp
    if (expectedType /= resType) then
        throwError (TypeMismatch expectedType resType)
    else
        return resType