module Main where

import Lib

import Language.PCL.Token
import Language.PCL.Annotated.Lexer
import Language.PCL.Annotated.Parser
import Language.PCL.Annotated.Semantic
import Language.PCL.Annotated.Syntax
import Language.PCL.PrettyPrinter

import Toy.LLVM

main :: IO ()
main = toy "examples/bubblesort.pcl"

toy :: String -> IO ()
toy path = do
    f <- readFile path
    putStrLn "Text:"
    print f
    putStrLn "-------------------------------"
    putStrLn "Token:"
    print $ unToken <$> alexScanTokens f
    let p = parseNamedText program path f
    putStrLn "-------------------------------"
    putStrLn "Tree:"
    print $ silent <$> p
    putStrLn "-------------------------------"
    putStrLn "Pretty:"
    print $ pprint <$> p
    putStrLn "-------------------------------"
    putStrLn "Environment"
    print $ getEnv <$> p
    putStrLn "-------------------------------"
    putStrLn "Check Body"
    print $ verifyProgram <$> p
    return ()
    putStrLn "-------------------------------"
    putStrLn "LLVM toy"
---    runJIT myModule
    print myModule
    toLLVM myModule