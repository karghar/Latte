-- automatically generated by BNF Converter
module Main where

import System.IO ( stdin, hGetContents, hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Process
import System.Exit
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import TypeChecker
import Compiler
import System.FilePath.Posix (dropExtension, replaceExtension, takeDirectory, dropFileName, takeFileName)




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

--runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
--runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> String -> String -> IO ()
run v path s  = let ts = myLLexer s in case pProgram ts of
           Bad s    -> do hPutStrLn stderr $ "ERROR"
                          putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do
                          case evalTypes tree of
                                    Left error-> do
                                      hPutStrLn stderr $ "ERROR"
                                      putStrLn $ show error
                                      exitFailure
                                    Right _ -> do
                                      let compiledCode = compilationProcess tree
                                      let fileName = dropExtension $ takeFileName path
                                      let outputFile = dropExtension path
                                      let pathS =  replaceExtension path "s"
                                      let pathO = replaceExtension path "o"
                                      let directory = dropFileName path
                                      let compileBash = "ld -s -o " ++ outputFile ++ " -melf_i386 " ++  pathO ++ " ./lib/runtime.o -L. -l:lib/libc.a"
                                      writeFile pathS compiledCode
                                      let assemblyToOCmd = "as --32 " ++ pathS ++ " -o " ++ pathO
                                      assemblerToO <- runCommand $ assemblyToOCmd
                                      -- putStrLn $ assemblyToOCmd
                                      waitForProcess assemblerToO
                                      -- putStrLn compileBash
                                      -- putStrLn compiledCode
                                      systemHandler <- runCommand $ compileBash
                                      waitForProcess systemHandler
                                      hPutStrLn stderr $ "OK"
--                                      compileProg prog



showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 ""
    (path:[]) -> readFile path >>= run 2 path
    _ -> hPutStrLn stderr $ "Too many arguments"

{-    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs-}





