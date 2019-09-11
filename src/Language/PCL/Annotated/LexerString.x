{

{-# OPTIONS_GHC -w #-}

module Language.PCL.Annotated.LexerString
    ( mkString
    , mkEscape
    ) where

}

%wrapper "posn"

$escape =  [n t r 0 \' \" \\]

tokens :-

    <0> \\$escape  { \p s -> mkEscape $ head $ tail s }
    <0> $printable # [\" \' \\] { \p s -> head s }

{

mkEscape :: Char -> Char
mkEscape = go
    where
        go 'n'  = '\n'
        go 't'  = '\t'
        go 'r'  = '\r'
        go '0'  = '\0'
        go '\\' = '\\'
        go '\'' = '\''
        go '\"' = '\"'
        go c    = error $ concat
            [ "Internal Error while creating escape char from "
            , show c
            , ". Please report!"
            ]

mkString :: String -> String
mkString = alexScanTokens


}