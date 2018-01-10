module Compiler where

import Control.Monad.State

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
import Debug.Trace

import qualified Data.Map as M


type Code = String

data ClassDeff = ClassDeff
              { className ::  Ident
              , parent :: Maybe (Ident)
              , attrs :: M.Map Ident Type
              , methods :: M.Map Ident Type
              }

type EnvMap = M.Map Ident (Int, Type) -- int position in stock, type you know

data AsData = AsData
           {
             label :: Int
           , stackSize :: Int
           , tempLabel :: Int
           }

-- klasy $
-- metody _
data  CEnv = CEnv
           { env :: EnvMap
           , strings :: M.Map String String
           , fEnv :: M.Map Ident Type
           , clsEnv :: M.Map Ident ClassDeff
           , utils :: AsData
           }

type Compile = State CEnv

initStore :: CEnv
initStore = CEnv M.empty M.empty M.empty M.empty initAsData

initAsData :: AsData
initAsData = AsData 0 0 0

prolog :: Code
prolog = unlines [
               "section .text",
               "global main",
               "extern printInt, printString, concatStrings, readInt, readString, initClass"
               ]

compilationProcess :: Program -> String
compilationProcess prog =
    prolog ++ (evalState (compileProg prog) (initStore))

compileProg :: Program -> Compile Code
compileProg prog = do
    strings <- compileStringLabels prog
    prepareFunctions prog
    prepareClasses prog
    insides <- compileProgram prog
    return strings

compileProgram :: Program -> Compile Code
compileProgram (Prog topDefs) = compileTopDefs topDefs

prepareFunctions :: Program -> Compile ()
prepareFunctions (Prog topDefs) = do
    prepareProgFunctions topDefs
    addBuiltInFunctions
    return ()

prepareClasses :: Program -> Compile ()
prepareClasses prog = undefined

builtIns = [(Ident "__concatStrings", Void)]

addBuiltInFunctions :: Compile ()
addBuiltInFunctions = undefined
--TODO

prepareProgFunctions :: [TopDef] -> Compile ()
prepareProgFunctions [] = return ()
prepareProgFunctions ((FnDef (FunDef t ident _ _)):rest) = do
    cEnv <- get
    let funcEnv = fEnv cEnv
    put (CEnv (env cEnv) (strings cEnv) (M.insert ident t funcEnv) (clsEnv cEnv) (utils cEnv))
prepareProgFunctions ((ClassDef _ _ ):rest) = prepareProgFunctions rest
prepareProgFunctions ((ClassDefExt _ _ _):rest) = prepareProgFunctions rest


-- strings extraction part --
compileStringLabels :: Program -> Compile Code
compileStringLabels prog = do
    let strings = getStrings prog
    return "dummy" --TODO finish

--helper function for removing duplicates
removeDuplicates :: [String] -> [String]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs


getStrings :: Program -> [String]
getStrings (Prog topDefs) = removeDuplicates (getStringsTopDefs topDefs)

getStringsTopDefs :: [TopDef] -> [String]
getStringsTopDefs [] = []
getStringsTopDefs (topDef:rest) = (getStringsTopDef topDef) ++ (getStringsTopDefs rest)

getStringsTopDef :: TopDef -> [String]
getStringsTopDef (FnDef (FunDef t ident args (Block stmts))) = getStringsStmts stmts
getStringTopDef (ClassDef id cdef) = getStringsCDef cdef
getStringTopDef (ClassDefExt ident parent cdef) = getStringsCDef cdef

getStringsCDef :: CDef -> [String]
getStringsCDef (ClassBlock classElems) = concat $ map (getStringsClassElems) classElems

getStringsClassElems :: ClassElem -> [String]
getStringsClassElems (ClassMeth (FunDef t ident args (Block stmts))) = getStringsStmts stmts
getStringsClassElems (ClassAtr t ident) = return []

getStringsStmts :: [Stmt] -> [String]
getStringsStmts [] = []
getStringsStmts (stmt:rest) = (getStringsStmt stmt) ++ (getStringsStmts rest)

getStringsStmt :: Stmt -> [String]
getStringsStmt (Empty) = []
getStringsStmt (BStmt (Block stmts)) = getStringsStmts stmts
getStringsStmt (Decl Str []) = []
getStringsStmt (Decl Str items) = concat $ map (getStringsItem) items
getStringsStmt (Ass lval exp) = (getStringsLValue lval) ++ (getStringsExp exp)
getStringsStmt (Ret exp) = getStringsExp exp
getStringsStmt (Cond exp stmt) = (getStringsExp exp) ++ (getStringsStmt stmt)
getStringsStmt (CondElse exp lhsStmt rhsStmt) = (getStringsExp exp) ++
            (getStringsStmt lhsStmt) ++ (getStringsStmt rhsStmt)
getStringsStmt (While exp stmt) = (getStringsExp exp) ++ (getStringsStmt stmt)
getStringsStmt (For typ ident arrayIdent stmt) = getStringsStmt stmt
getStringsStmt (SExp exp) = getStringsExp exp
getStringsStmt _ = []

getStringsItem :: Item -> [String]
getStringsItem (Init ident exp) = getStringsExp exp
getStringsItem (NoInit ident) = [""]

getStringsExp :: Expr -> [String]
getStringsExp (EApp (FunctionCall ident exps)) = concat (map (getStringsExp) exps)
getStringsExp (EString string) = [string]
getStringsExp (EAdd lhsExp addOp rhsExp) = (getStringsExp lhsExp) ++ (getStringsExp rhsExp)
getStringsExp (ERel lhsExp relOp rhsExp) = (getStringsExp lhsExp) ++ (getStringsExp rhsExp)
getStringsExp (EAnd lhsExp rhsExp) = (getStringsExp lhsExp) ++ (getStringsExp rhsExp)
getStringsExp (EOr lhsExp rhsExp) = (getStringsExp lhsExp) ++ (getStringsExp rhsExp)
getStringsExp _ = []

getStringsLValue _ = []

compileTopDefs :: [TopDef] -> Compile Code
compileTopDefs [] = return ""
compileTopDefs ((FnDef funDef):rest) = do
    funDef <- compileFunDef funDef
    rest <- compileTopDefs rest
    return $ funDef ++ rest
--TODO classes not done
compileTopDefs ((ClassDef id cdef):rest) = compileTopDefs rest
compileTopDefs ((ClassDefExt ident parent cdef):rest) = compileTopDefs rest


compileFunDef :: FuncDef -> Compile Code
compileFunDef funDef = undefined




--TODO DO OLANIA
compileExp :: Expr -> Compile Code
compileExp (ECastNull typ) = undefined
compileExp (EAttrAccess attrAccess) = undefined
compileExp (EMthCall methodCall) = undefined
compileExp (EArrAccess arrElemAccess) = undefined
compileExp (ENew ident) = undefined
compileExp (ENewArr typ exp) =undefined

--TODO
compileExp (EVar ident) = undefined


compileExp (ELitInt int) = return $ instrL (push ("$" ++ show int))
compileExp (ELitTrue) = return $ instrL (push "$1")
compileExp (ELitFalse) = return $ instrL (push "$0")
compileExp (EApp (FunctionCall ident exprs)) = undefined --TODO
compileExp (EString str) = do
    cEnv <- get
    let stringLabels = strings cEnv
    case M.lookup str stringLabels of
        Just label -> error "SAS"
        Nothing -> error ("Shouldnt happen, searching for string label:" ++ str)
compileExp (Neg exp) = error "SAS"
compileExp (Not exp) = error "SAS"
compileExp (EMul lhsExp mulOp rhsExp) = do
    lhsCode <- compileExp lhsExp
    rhsCode <- compileExp rhsExp
    undefined --TODO

compileExp (EAdd lhsExp Plus rhsExp) = do
    lhsCode <- compileExp lhsExp
    rhsCode <- compileExp rhsExp
    expType <- getExpType lhsExp
    let expCode = lhsCode ++ rhsCode
    case expType of
        Int ->
            return $ expCode ++ pop eax ++ (instrL  ("add 0(%esp), " ++ eax)) ++ mov eax "0(%esp)"
        Str ->
            return $ expCode ++ concatStrings
        _ -> error "Shouldnt happen, shitty type when adding two exps"



compileExp (ERel lhsExp relOp rhsExp) = error "SAS"
compileExp (EAnd rhsExp lhsExp) = error "SAS"
compileExp (EOr rhsExp lhsExp) = error "SAS"


concatStrings = unlines [
  "call concatStrings",
  "add esp, 8",
  "push eax"
  ]


--def block expType
getExpType :: Expr -> Compile Type
getExpType ( ECastNull typ) =  return typ
getExpType ( EAttrAccess attrAccess) = undefined
getExpType ( EMthCall mthCall) = undefined
getExpType ( EArrAccess arrElemAccess) = undefined
getExpType ( EVar ident) = do
    cEnv <- get
    let env_ = env cEnv
    case M.lookup ident env_ of
        Just (size, typ) -> return typ
        Nothing -> error ("Shouldnt happen, lookup of Variable" ++ show ident)
getExpType ( ENewArr typ exp) = undefined
getExpType ( ENew ident) = undefined
getExpType ( ELitInt int) = return Int
getExpType ( ELitTrue) = return Bool
getExpType ( ELitFalse) = return Bool
getExpType ( EApp (FunctionCall ident exp)) = do
    cEnv <- get
    let funcs = fEnv cEnv
    case M.lookup ident funcs of
        Just typ -> return typ
        Nothing -> error ("Fatal, shouldnt happen lookup of function ident" ++ show ident)
getExpType ( EString str) = return Str
getExpType ( Neg exp) = return Bool
getExpType ( Not exp) = return Bool
getExpType ( EMul lhsExp _ rhsExp) = return Int
getExpType ( EAdd lhsExp Plus rhsExp) = getExpType lhsExp
getExpType ( EAdd lhsExp Minus rhsExp) = return Int
getExpType ( ERel lhsExp _ rhsExp) = return Bool
getExpType ( EAnd lhsExp rhsExp) = return Bool
getExpType ( EOr lhsExp rhsExp) = return Bool


getLValueType :: LValue -> Compile Type
getLValueType (LVJustIdent ident) = do
    cEnv <- get
    let env_ = env cEnv
    case M.lookup ident env_ of
        Just (size, typ) -> return typ
        Nothing -> error ("Shouldn't happen, searching for Lvalue Type" ++  show ident)
getLValueType (LVFunCall funcCall) = getExpType (EApp funcCall)
getLValueType (LVMethodCall (MCall lval funcCall)) = undefined
findLValueType (LVArrayAcc (ArrayElem lval exp)) = undefined
findLValueType (LVAttrAcc (AttrAcc lval ident)) = undefined

-- endblock expType


-- def block utils
eax = "%eax"
ebx = "%ebx"
ecx = "%ecx"
edx = "%edx"
ebp = "%ebp"
esp = "%esp"


instrL :: Code -> Code
instrL inside = tabbed ++ inside ++ endOfLine
tabbed = "\t"
endOfLine = "\n"
call (Ident str) = "call " ++ str
push src = "push " ++ src
pop src = instrL $ ("pop " ++ src)
mov src dst = instrL $ ("mov " ++ src ++ ", " ++ dst)
add lhs rhs = instrL $ ("add " ++ lhs ++ "," ++ rhs)
test src dst = instrL $ ("test " ++ src ++ ", " ++ dst)
jmp label = instrL $ ("jmp " ++ label)

getSize :: Type -> Int
getSize (Arr _) = 8
getSize Int = 4
getSize Str = 4
getSize Bool = 4
getSize (Obj _) = 4
getSize _ = 0

-- end block utils