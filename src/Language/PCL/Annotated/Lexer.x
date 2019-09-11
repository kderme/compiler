{

{-# OPTIONS_GHC -w #-}

module Language.PCL.Annotated.Lexer where

import Language.PCL.Annotated.LexerString
import Language.PCL.Token

}

%wrapper "posn"

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits

$escape =  [n t r 0 \' \" \\]

$printableExcept = $printable # [\" \' \\]

@end = [\(][\*]
$all = \0-\255
-- $notend = $all # [\(][\*]

@digits    = $digit+

tokens :-

    <0> $white+  ;

    <0> \(\*(  $all  )*\*\) ;

    <0> @digits  { tokS (TokNum . read) }

    <0> @digits\.@digits([Ee][\+\-]?@digits)? { tokS (TokReal . Computed . read) }

    <0> $letter $identletter* { ident }

    <0> \"($printableExcept|\\$escape)*\" { tokS (TokSlit . mkString . tail . init) }

    <0> \'($printableExcept)\' { tokS (TokChar . head . tail) }
    <0> \'(\\$escape)\' { tokS (TokChar . mkEscape . head . tail . tail) }

    <0> "="   { tok TokEq }
    <0> ">"   { tok TokGT }
    <0> "<"   { tok TokLT }
    <0> "<>"  { tok TokNEq }
    <0> ">="  { tok TokGEq }
    <0> "<="  { tok TokLEq }
    <0> "+"   { tok TokPlus }
    <0> "-"   { tok TokMinus }
    <0> "*"   { tok TokStar }
    <0> "/"   { tok TokSlash }
    <0> "^"   { tok TokExp }
    <0> "@"   { tok TokAt }

    <0> ":="  { tok TokAssign }
    <0> ";"   { tok TokSemic }
    <0> "."   { tok TokDot }
    <0> "("   { tok TokLParen }
    <0> ")"   { tok TokRParen }
    <0> ":"   { tok TokColon }
    <0> ","   { tok TokComma }
    <0> "["   { tok TokLBracket }
    <0> "]"   { tok TokRBracket }

{

tok t = \p _ -> At p t

tokS f = \p s -> At p (f s)

ident :: AlexPosn -> String -> At AlexPosn Token
ident p s = At p tok'
    where
        tok' = case s of
                "and" -> TokAnd
                "array" -> TokArray
                "begin" -> TokBegin
                "dispose" -> TokDispose
                "div" -> TokDiv
                "do" -> TokDo
                "else" -> TokElse
                "end" -> TokEnd
                "false" -> TokFalse
                "forward" -> TokForward
                "function" -> TokFunction
                "goto" -> TokGoto
                "if" -> TokIf
                "label" -> TokLabel
                "mod" -> TokMod
                "new" -> TokNew
                "nil" -> TokNil
                "not" -> TokNot
                "of" -> TokOf
                "or" -> TokOr
                "procedure" -> TokProcedure
                "program" -> TokProgram
                "result" -> TokResult
                "return" -> TokReturn
                "then" ->  TokThen
                "true" -> TokTrue
                "var" -> TokVar
                "while" -> TokWhile
                "integer" -> TokIntegerT
                "boolean" -> TokBooleanT
                "char" -> TokCharT
                "real" -> TokRealT
                ident' -> TokIdent ident'

}