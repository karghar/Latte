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
import Data.Bool
import Data.Graph
import Control.Monad


import qualified Data.Map as M

type TypeError = String

data ClassDeff = ClassDeff
              { className ::  Ident
              , parent :: Maybe (Ident)
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
  | FunctionArgumentVoidType String
  | FunctionArgumentNameDuplicated String
  | VariableAlreadyDefined String
  | ClassAlreadyDefined String
  | CyclicClassInheritance
  deriving (Eq, Ord, Show)


evalTypes :: Program -> Either Error ()
evalTypes prog =
    fmap (const ()) (runEval initStore (evalProg prog))

runEval s e = evalState (runExceptT e) s

initStore :: Env
initStore = Env M.empty M.empty M.empty

getIdent :: Ident -> String
getIdent (Ident ident) = ident



---- Validating functions and classes --
validateFunction :: Type -> Ident -> [Arg] -> Eval (Ident, Type)
validateFunction retType ident args = do
    args <- validateFunArgs args []
    return (ident, Fun retType args)


validateClass :: Ident -> Maybe Ident -> CDef -> Eval (Ident, ClassDeff)
validateClass name parent (ClassBlock classElems) =  do
    env <- get
    let clsDef = ClassDeff name parent M.empty
    classDeff <- validateClassDef classElems clsDef
    put env
    return (name, classDeff)

validateClassDef :: [ClassElem] -> ClassDeff -> Eval ClassDeff
validateClassDef []  classDef = return classDef
validateClassDef (ClassMeth(FunDef retType ident args block):rest) classDef = do
  (funIdent, types) <- validateFunction retType ident args
  env' <- mapInsertLocalVar funIdent types 
  let attr' = M.insert funIdent types (attr classDef)
  put env'
  validateClassDef rest (ClassDeff (className classDef) (parent classDef) attr')

validateClassDef ((ClassAtr t ident):rest) classDef = do
  env' <- mapInsertLocalVar ident t
  let attr' = M.insert ident t (attr classDef)
  put env'
  validateClassDef rest (ClassDeff (className classDef) (parent classDef) attr') 




--function arguments validation : no voids, no duplicates
validateFunArgs :: [Arg] -> [String]-> Eval [Type]
validateFunArgs [] _ = return []
validateFunArgs ((Arg t (Ident ident)):args) visited =
    if (t == Void) then
        throwError (FunctionArgumentVoidType ident)
    else
        if (alreadyVisited ident visited) then
            throwError (FunctionArgumentNameDuplicated ident)
        else do
            rest <- validateFunArgs args (ident:visited)
            return (t:rest)

--funct to check duplicated in list of arguments
alreadyVisited :: String -> [String] ->  Bool
alreadyVisited argName [] = False
alreadyVisited argName (arg:xs) =
    if (argName == arg) then
        True
    else
        alreadyVisited argName xs
--END BLOCK---


-- BLOCK mapInserts Local global, classes -

--TODO maybe empty unit return type, decide

mapInsertLocalVar :: Ident -> Type -> Eval Env
mapInsertLocalVar ident t = do
    env <- get
    let loc = locals env
    case M.lookup ident loc of
        Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
        Nothing -> do
            return (Env (globals env) (M.insert ident t loc) (classes env))

--no need to return env if it is a global variable --
mapInsertGlobalVar :: Ident -> Type -> Eval ()
mapInsertGlobalVar ident t = do
    env <- get
    let glob = globals env
    case M.lookup ident glob of
        Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
        Nothing -> do
            put (Env (M.insert ident t glob) (locals env) (classes env))
            return ()

mapInsertClass :: Ident -> ClassDeff -> Eval ()
mapInsertClass ident classDef = do
    env <- get
    let cls = classes env
    case M.lookup ident cls of
        Just (_) -> throwError (ClassAlreadyDefined (getIdent ident))
        Nothing -> do
            put (Env (globals env) (locals env) (M.insert ident classDef cls))

-- END BLOCK INSERTS

--- DEF check classes inheritance

checkClassesInheritance :: Eval ()
checkClassesInheritance = do
  env <- get
  let classes_ = M.elems (classes env)
  let extractedEdges =  fmap extractClassesEdges classes_
  edgesInt <- convertClassesToEdges extractedEdges  M.empty 0 []
  let bounds = (0,  length classes_ + 1)
  let graph = buildG bounds edgesInt
  if (any (\x -> path graph x x) [0, length classes_ +1] == True) then
      throwError (CyclicClassInheritance)
  else
    return ()


extractClassesEdges :: ClassDeff -> (String, String)
extractClassesEdges classDef = do
  let myName = className classDef
  case parent classDef of 
    Just parentName -> (getIdent myName, getIdent parentName)
    Nothing -> (getIdent myName, "object") 

convertClassesToEdges :: [(String, String)] -> (M.Map String Int) -> Int -> [(Int, Int)] -> Eval [(Int, Int)]
convertClassesToEdges [] edgeHashMap nextInt edgesIntArr = return edgesIntArr 
convertClassesToEdges ((lhs,rhs):rest) edgeHashMap nextInt edgesIntArr = do
  let (nextInt', edgeHashMap') =  insertEdgeIntoHashMap lhs edgeHashMap nextInt
  let (nextInt'', edgeHashMap'') = insertEdgeIntoHashMap rhs edgeHashMap' nextInt'
  case (M.lookup lhs edgeHashMap'') of
    Just lhsInt -> do
      case (M.lookup rhs edgeHashMap'') of
        Just rhsInt -> 
          convertClassesToEdges (rest) (edgeHashMap'') nextInt'' ((lhsInt, rhsInt):edgesIntArr)
        Nothing ->
          throwError (DummyError "not gonna happen")
    Nothing -> 
      throwError (DummyError "not gonna happen2")



checkIfEdgePresent :: String -> (M.Map String Int) -> Bool
checkIfEdgePresent edgeName edgeHashMap = 
  case (M.lookup edgeName edgeHashMap) of
      Just number -> True
      Nothing -> False

insertEdgeIntoHashMap :: String -> (M.Map String Int) -> Int -> (Int, M.Map String Int)
insertEdgeIntoHashMap edgeName edgeHashMap nextInt = do
  if ((checkIfEdgePresent edgeName edgeHashMap) == False) then do
    let edgeHashMap' = M.insert edgeName nextInt edgeHashMap
    (nextInt + 1, edgeHashMap')
  else
    (nextInt, edgeHashMap)

--- Evaluation of program -----
evalProg :: Program -> Eval ()
evalProg (Prog topdefs) = do
    env <- get
    prepareTopDefs topdefs
    checkClassesInheritance
    validateMain topdefs

    checkWholeDefs topdefs

prepareTopDefs :: [TopDef] -> Eval ()
prepareTopDefs [] = return () --return enviroment
prepareTopDefs (FnDef(FunDef retType ident args block):topDefs) = do
    (ident, types) <- validateFunction retType ident args
    mapInsertGlobalVar ident types
    prepareTopDefs topDefs

prepareTopDefs ((ClassDef ident classDef):topDefs) = do
    (className, clsDef) <- validateClass ident Nothing classDef
    mapInsertClass className clsDef
    prepareTopDefs topDefs
prepareTopDefs ((ClassDefExt ident parent classDef):topDefs) = do
    (className, clsDef) <- validateClass ident (Just parent) classDef
    mapInsertClass className  clsDef
    prepareTopDefs topDefs



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