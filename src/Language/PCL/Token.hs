module Language.PCL.Token where

data At pos tok = At pos tok
    deriving (Eq, Show)

unToken :: At p t -> t
unToken (At _ tok) = tok

data Token = TokAnd
           | TokArray
           | TokBegin
           | TokDispose
           | TokDiv
           | TokDo
           | TokElse
           | TokEnd
           | TokFalse
           | TokForward
           | TokFunction
           | TokGoto
           | TokIf
           | TokLabel
           | TokMod
           | TokNew
           | TokNil
           | TokNot
           | TokOf
           | TokOr
           | TokProcedure
           | TokProgram
           | TokResult
           | TokReturn
           | TokThen
           | TokTrue
           | TokVar
           | TokWhile

           | TokIdent String   -- identifier
           | TokNum Int           -- number constants
           | TokReal RealToken
           | TokChar Char
           | TokSlit String        -- string constant

           | TokEq
           | TokGT
           | TokLT
           | TokNEq
           | TokGEq
           | TokLEq
           | TokPlus
           | TokMinus
           | TokStar
           | TokSlash
           | TokExp
           | TokAt

           | TokAssign
           | TokSemic
           | TokDot
           | TokLParen
           | TokRParen
           | TokColon
           | TokComma
           | TokLBracket
           | TokRBracket

           | TokIntegerT
           | TokRealT
           | TokBooleanT
           | TokCharT
        deriving (Eq, Show)

data RealToken = Raw String | Computed Double
    deriving (Eq, Show)

-- instance Show Token where