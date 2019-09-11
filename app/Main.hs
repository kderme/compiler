module Main where

import Lib

import Language.PCL.Token
import Language.PCL.Annotated.Lexer
import Language.PCL.Annotated.Parser
import Language.PCL.Annotated.Syntax

main :: IO ()
main = toy "examples/toy.pcl"

toy :: String -> IO ()
toy path = do
    f <- readFile path
    print f
    print $ unToken <$> alexScanTokens f
    print $ silent <$> parseNamedText program path f
    return ()