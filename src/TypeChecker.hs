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
              , attrs :: M.Map Ident Type
              , methods :: M.Map Ident Type
              }

data Context = Context 
            { functions :: M.Map Ident Type
            , attributes :: M.Map Ident Type
              }

data Env = Env
           { locals :: Context
           , globals :: Context
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
  | VariableNotFoundInContext String
  | ReturnStmtMissing
  | IncDecWrongType LValue
  | WrongRetType Type Type
  | VoidDeclarationType [Item]
  | ClassNotDeclared Ident
  | WrongArrayTypeDecl Type
  deriving (Eq, Ord, Show)


evalTypes :: Program -> Either Error ()
evalTypes prog =
    fmap (const ()) (runEval initStore (evalProg prog))

runEval s e = evalState (runExceptT e) s

initStore :: Env
initStore = Env (Context M.empty M.empty) (Context M.empty M.empty) M.empty

emptyContext = Context M.empty M.empty

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
    let clsDef = ClassDeff name parent M.empty M.empty
    classDeff <- validateClassDef classElems clsDef
    put env
    return (name, classDeff)

validateClassDef :: [ClassElem] -> ClassDeff -> Eval ClassDeff
validateClassDef []  classDef = return classDef

validateClassDef (ClassMeth(FunDef retType ident args block):rest) classDef = do
  (funIdent, types) <- validateFunction retType ident args
  env' <- mapInsertLocalVar funIdent types 
  let methods' = M.insert funIdent types (methods classDef)
  put env'
  validateClassDef rest (ClassDeff (className classDef) (parent classDef) (attrs classDef) methods')

validateClassDef ((ClassAtr t ident):rest) classDef = do
  env' <- mapInsertLocalVar ident t
  let attrs' = M.insert ident t (attrs classDef)
  put env'
  validateClassDef rest (ClassDeff (className classDef) (parent classDef) attrs' (methods classDef)) 


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
mapInsertLocalVar ident (Fun retType types) = do
    env <- get
    let loc = locals env
    let locFun = functions loc
    let locAttrs = attributes loc
    case M.lookup ident locFun of
        Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
        Nothing -> do
            let locFun' = M.insert ident (Fun retType types) locFun
            return (Env (globals env) (Context locFun' locAttrs) (classes env))

mapInsertLocalVar ident t = do
    env <- get
    let loc = locals env
    let locAttrs = attributes loc
    let locFun = functions loc
    case M.lookup ident locAttrs of
      Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
      Nothing -> do
        let locAttrs' = M.insert ident t locAttrs
        return (Env (globals env) (Context locFun locAttrs') (classes env))


--no need to return env if it is a global variable --
mapInsertGlobalVar :: Ident -> Type -> Eval ()
mapInsertGlobalVar ident (Fun retType types) = do
    env <- get
    let glob = globals env
    let globFun = functions glob
    let globAttrs = attributes glob
    case M.lookup ident globFun of
        Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
        Nothing -> do
            let globFun' = M.insert ident (Fun retType types) globFun
            put (Env (Context globFun' globAttrs) (locals env) (classes env))
            return ()

mapInsertGlobalVar ident t = do
    env <- get
    let glob = globals env
    let globFun = functions glob
    let globAttrs = attributes glob
    case M.lookup ident globAttrs of
        Just (_) -> throwError (VariableAlreadyDefined (getIdent ident))
        Nothing -> do
            let globAttrs' = M.insert ident t globAttrs
            put (Env (Context globFun globAttrs') (locals env) (classes env))
            return ()

mapInsertClass :: Ident -> ClassDeff -> Eval ()
mapInsertClass ident classDef = do
    env <- get
    let cls = classes env
    case M.lookup ident cls of
        Just (_) -> throwError (ClassAlreadyDefined (getIdent ident))
        Nothing -> do
            put (Env (globals env) (locals env) (M.insert ident classDef cls))


mapLookUpLocalVar :: Ident -> Eval Type
mapLookUpLocalVar ident = do
  env <- get
  let local = locals env
  case M.lookup ident (attributes local) of
    Just t -> return t
    Nothing -> throwError (VariableNotFoundInContext (getIdent ident))





-- END BLOCK INSERTS

--- DEF check classes inheritance

--TODO class method inheritance evalution
checkClassesInheritance :: Eval ()
checkClassesInheritance = do
  checkClassesAcyclicInheritance



checkClassesAcyclicInheritance :: Eval ()
checkClassesAcyclicInheritance = do
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

-- END BLOCK CLASS INHERITANCE EVALUTATOR


--- Evaluation of program -----
evalProg :: Program -> Eval ()
evalProg (Prog topdefs) = do
    env <- get
    prepareTopDefs topdefs
    checkClassesInheritance
    validateMain topdefs
    checkTopDefs topdefs

--- def block validate main

validateMain :: [TopDef] -> Eval ()
validateMain [] = throwError MainFunctionMissing
validateMain (FnDef(FunDef Int (Ident "main") [] _):_) = return ()
validateMain (FnDef(FunDef _ (Ident "main") [] _):_) = throwError MainFunctionRetTypeMismatch
validateMain (FnDef(FunDef _ (Ident "main") _ _):_) = throwError MainFunctionArgsMismatch
validateMain (_:tl) = validateMain tl

--end block 


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



checkTopDefs :: [TopDef] -> Eval ()
checkTopDefs (def:topdefs) = do
    env <- get
    checkTopDef def
    put env
    checkTopDefs topdefs
checkTopDefs [] = return ()


checkTopDef :: TopDef -> Eval ()
checkTopDef (FnDef func) =
    checkFuncDef func
checkTopDef (ClassDef name classDef) =
    checkClassDef name Nothing classDef
checkTopDef (ClassDefExt name parent classDef) =
    checkClassDef name (Just parent) classDef


checkClassDef :: Ident -> Maybe Ident -> CDef -> Eval ()
checkClassDef ident parentIdent classDef = undefined

returnTypeName :: Ident
returnTypeName = (Ident "__ret__")


getReturnType :: Eval Type
getReturnType = mapLookUpLocalVar returnTypeName


--TODO doprowadz chociaz do uzycia bloku
checkFuncDef :: FuncDef -> Eval ()
checkFuncDef (FunDef retType ident args (Block stmts)) = do
  retEnv <- mapInsertLocalVar returnTypeName retType
  put retEnv
  argsEnv <- checkArgsEnv args
  put argsEnv
  checkRetStmts stmts
  put argsEnv
  checkBlock (Block stmts)
  --now checkstatements, then check return block? of should it be other way
  return ()

checkArgsEnv :: [Arg] -> Eval Env
checkArgsEnv [] = get
checkArgsEnv ((Arg t ident):rest) = do
  env <- mapInsertLocalVar ident t
  put env
  checkArgsEnv rest


checkRetStmts :: [Stmt] -> Eval ()
checkRetStmts [] = do
  retType <- getReturnType
  unless (retType == Void) (throwError ReturnStmtMissing)
checkRetStmts (x:[]) =
    checkRetStmt x
checkRetStmts (x:xs) = do
    env <- (checkStmt x)
    put env
    checkRetStmts xs



checkRetStmt :: Stmt -> Eval ()

checkRetStmt (BStmt (Block stmts)) = do
    env <- get
    checkRetStmts stmts
    put env

checkRetStmt (Ret exp) = do
    retType <- getReturnType
    checkType exp retType
    return ()
checkRetStmt (Cond exp stmt) =
    checkRetStmt stmt
checkRetStmt (CondElse exp lhsStmt rhsStmt) = do
    checkRetStmt lhsStmt
    checkRetStmt rhsStmt
checkRetStmt (While exp stmt) =
    checkRetStmt stmt
checkRetStmt (For typ ident1 ident2 stmt) =
    checkRetStmt stmt

--inconsequential statements in last block
checkRetStmt _ =  do
    retType <- getReturnType
    unless (retType == Void) (throwError ReturnStmtMissing)
--checkRetStmt (Decl typ [items]) = undefined
--checkRetStmt (Ass lval exp) = undefined
--checkRetStmt (Incr lval) = undefined
--checkRetStmt (Decr lval) = undefined
--checkRetStmt Empty =  do
--    retType <- getReturnType
--    unless (retType == Void) (throwError ReturnStmtMissing)
--checkRetStmt VRet = do
--    retType <- getReturnType
--    unless (retType == Void) (throwError ReturnStmtMissing)
--checkRetStmt (SExp exp) = do
--    retType <- getReturnType
--    unless (retType == Void) (throwError ReturnStmtMissing)


--DEF BLOCK checkStmt

checkBlock :: Block -> Eval ()
checkBlock (Block stmts) = do
  env <- get
  let globalsFuncJoined  = M.union (functions (locals env)) (functions (globals env))
  let globalsAttrJoined = M.union (attributes (locals env)) (attributes (globals env))
  let envModified = (Env (Context globalsFuncJoined globalsAttrJoined) (emptyContext) (classes env))
  put envModified
  checkStmts stmts
  put env

checkStmts :: [Stmt] -> Eval ()
checkStmts [] = return ()
checkStmts (stmt:rest) = do
  env' <- checkStmt stmt
  put env'
  checkStmts rest

checkStmt :: Stmt -> Eval Env
checkStmt Empty = get
checkStmt (BStmt block) = do
    checkBlock block
    get
checkStmt (Decl t items) = undefined
    checkDeclList t items
checkStmt (Ass lvalue exp) = do
    expType <- findType exp
    identType <- findIdentType lvalue
    if (expType == identType) then get
    else do
        checkIfSubClass identType expType
        get
checkStmt (Incr lvalue) = do
    lvalType <- findIdentType lvalue
    unless (lvalType == Int) (throwError (IncDecWrongType lvalue))
    get
checkStmt (Decr lvalue) = do
    lvalType <- findIdentType lvalue
    unless (lvalType == Int) (throwError (IncDecWrongType lvalue))
    get
checkStmt (Ret expr) = do
    expType <- findType expr
    retType <- getReturnType
    if (expType == retType) then get
    else do
        checkIfSubClass retType expType
        get
checkStmt (VRet) = do
    retType <- getReturnType
    unless (retType  == Void) (throwError (WrongRetType (retType) (Void)))
    get
checkStmt (Cond exp stmt) = do
    checkType exp Bool
    checkStmt stmt
checkStmt (CondElse exp stmt1 stmt2) = do
    checkType exp Bool
    checkStmt stmt1
    checkStmt stmt2
checkStmt (While exp stmt) = do
    checkType exp Bool
    checkStmt stmt
--TODO implement this sheet
checkStmt (For t ident1 ident2 stmt) = undefined
checkStmt (SExp exp) = do
    findType exp
    get

checkDeclList :: Type -> [Item] -> Eval Env
checkDeclList _ [] = get
checkDeclList t (item:items) = do
    when (t == Void) (throwError (VoidDeclarationType (item:items)))
    ident <- checkItem item t
    env' <- mapInsertLocalVar ident t
    put env'
    checkDeclList t items

-- end block checkStmt

checkItem :: Item -> Type -> Eval Ident
checkItem (NoInit ident) _ = return ident
checkItem (Init ident exp) t = do
    expType <- findType exp
    if (expType == t) then
        return ident
    else do
        checkIfSubClass t expType
        return ident


--TODO maybe optimize this fucking equal type or subclass

findIdentType = undefined
checkIfSubClass = undefined


--DEF BLOCK findTYPE

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

findType (ENewArr (Arr Bool) exp) =
    return (Arr Bool)
findType (ENewArr (Arr Int) exp) =
    return (Arr Int)
findType (ENewArr (Arr Str) exp) = do
    return (Arr Str)
findType (ENewArr (Arr (Obj ident)) exp) = do
    env <- get
    case M.lookup ident (classes env) of
        Just _ -> return (Arr (Obj ident))
        Nothing -> throwError (ClassNotDeclared ident)
findType (ENewArr (Arr t) exp) =
    throwError (WrongArrayTypeDecl t)
findType (ENewArr t exp) =
    throwError (WrongArrayTypeDecl t)

findType (ENew ident) = do
    env <- get
    case M.lookup ident (classes env) of
        Just _ -> return (Obj ident)
        Nothing -> throwError (ClassNotDeclared ident)
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