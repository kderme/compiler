module Language.PCL.Annotated.Parser where

import           Prelude

import           Control.Applicative hiding (many)
import           Data.Maybe (isJust)
import           Text.Parsec hiding (satisfy, string)

import Language.PCL.Annotated.Lexer hiding (tok)
import Language.PCL.Token
import Language.PCL.Annotated.Syntax
import Text.Parsec.Tok

-- Parsec is known to have 2 weeknesses:
-- + the alternative <|> sign it relies (choice uses it behind the curtains)
--   may consume part of the input while trying to produde the left part and
--   the input remains consumed even upon failure. That's not a big problem:
--   @try@ can be used to make parsec backtrack if matching fails
-- + parsec created infinite loops on left recursive gramars. The gramar of
--   PCL is left-recursive, so we have to flatten it out. Left recursion can
--   be seen in the following cases:
--   (1)
--   l-value => l-value "[" expr "]"
--   (2)
--   l-value => expr "^"
--   expr => lval
--   (3)
--   r-value => expr binop expr
--   expr => r-value

parseNamedText :: Parser a -> String -> String -> Either ParseError a
parseNamedText p n s =
    let toks = alexScanTokens s
    in parse p n toks

ls :: Parser [Header SourcePos]
ls = many1 header

program :: Parser (Program SourcePos)
program = do
    pos <- getPosition
    tok' TokProgram
    n <- name
    tok' TokSemic
    b <- body
    tok' TokDot
    return $ Program pos n b

body :: Parser (Body SourcePos)
body = do
    p <- getPosition
    Body <$> getPosition <*> try (many local) <*> block -- return (Block p (EmptyStm p) [])

local :: Parser (Local SourcePos)
local = try $ do
    pos <- getPosition
    choice
        [ try (varLocal pos)
        , try (lobelLocal pos)
        , try (bodyLocal pos)
        , try (forLocal pos)
        ]
        where
            varLocal pos = do
                tok' TokVar
                defs <- try $ do
                    pos' <- getPosition
                    n <- name
                    ns <- commas name
                    tok' TokColon
                    DefIdList pos' (n : ns) <$> typeT
                tok' TokSemic
                return $ Var pos [defs]

            lobelLocal pos = do
                tok' TokLabel
                n <- name
                ns <- commas name
                tok' TokSemic
                return $ Label pos (n : ns)

            bodyLocal pos = do
                h <- header
                tok' TokSemic
                b <- body
                tok' TokSemic
                return $ HeaderL pos h b

            forLocal pos = do
                tok' TokForward
                h <- header
                tok' TokSemic
                return $ ForL pos h

header :: Parser (Header SourcePos)
header = do
    pos <- getPosition
    choice
        [ procedure pos
        , function pos
        ]
        where
            procedure pos = do
                tok' TokProcedure
                nm <- name
                tok' TokLParen
                mfs <- optionMaybe formals
                tok' TokRParen
                return $ Procedure pos nm (fromMaybeLs mfs)
            function pos = do
                pos <- getPosition
                tok' TokFunction
                nm <- name
                tok' TokLParen
                mfs <- optionMaybe formals
                tok' TokRParen
                Function pos nm (fromMaybeLs mfs) <$> typeT
            formals = do
                f <- formal
                fs <- semicolons formal
                return $ f : fs

block :: Parser (Block SourcePos)
block = do
    pos <- getPosition
    tok' TokBegin
    s <- stmt
    stmts <- semicolons stmt
    tok' TokEnd
    return $ Block pos s stmts

stmt :: Parser (Stm SourcePos)
stmt = do
    pos <- getPosition
    m <- try $ optionMaybe $ choice
        [
          try $ satisfy' (isReturn pos . unToken)
        , try $ assignStm pos
        , try $ BlockStm pos <$> block
        , try $ CallStm pos <$> call
        , ifStm pos
        , whileStm pos
        , colonStm pos
        , gotoStm pos
        , newStm pos
        , disposeStm pos
        ]
    case m of
        Just p -> return p
        Nothing -> return $ EmptyStm pos
      where
        isReturn pos TokReturn = Just $ ReturnStm pos
        isReturn _ _           = Nothing

        assignStm pos = do
            l <- lval
            tok' TokAssign
            AssignStm pos l <$> expr

        ifStm pos = do
            tok' TokIf
            e <- expr
            tok' TokThen
            s <- stmt
            ms <- optionMaybe $ do
                tok' TokElse
                stmt
            return $ IfStm pos e s ms

        whileStm pos = do
            tok' TokWhile
            e <- expr
            tok' TokDo
            WhileStm pos e <$> stmt

        colonStm pos = do
            n <- name
            tok' TokSemic
            ColonStm pos n <$> stmt

        gotoStm pos = do
            tok' TokGoto
            GotoStm pos <$> name

        newStm pos = do
            tok' TokNew
            me <- optionMaybe $ do
                tok' TokLBracket
                s <- expr
                tok' TokRBracket
                return s
            NewStm pos me <$> lval

        disposeStm pos = do
            tok' TokNew
            bl <- optionMaybe $ do
                tok' TokLBracket
                tok' TokRBracket
            DisposeStm pos (isJust bl) <$> lval

lval :: Parser (LVal SourcePos)
lval = do
    pos <- getPosition
    l <- choice
        [ try simplelval
        , try $ rlval pos
        ]
    sub <- sublval
    return $ toLVal pos l sub
      where
        rlval pos = do
            r <- rval
            tok' TokExp
            return $ LValPow pos (RExpr pos r)

toLVal :: SourcePos -> LVal SourcePos -> SubLVal SourcePos -> LVal SourcePos
toLVal pos = go
    where
    go l (SubLValBrac _ e s') = go (LValBrac pos l e) s'
    go l (SubLValPow _ s')    = go (LValPow pos (LExpr pos l)) s'
    go l (SubLValEmpty _)     = l

sublval :: Parser (SubLVal SourcePos)
sublval = do
    pos <- getPosition
    m <- try $ optionMaybe $ choice
        [ bracl pos
        , powl pos
        ]
    case m of
        Just s -> return s
        Nothing -> return $ SubLValEmpty pos
      where
        bracl pos = do
            tok' TokLBracket
            e <- expr
            tok' TokRBracket
            s' <- sublval
            return $ SubLValBrac pos e s'
        powl pos = do
            tok' TokExp
            s' <- sublval
            return $ SubLValPow pos s'


simplelval :: Parser (LVal SourcePos)
simplelval = do
    pos <- getPosition
    let lVal (TokIdent s) = Just $ LValId pos $ Name pos s
        lVal TokResult    = Just $ Result pos
        lVal (TokSlit s)  = Just $ LValStr pos s
        lVal _            = Nothing
    choice
      [ try $ satisfy' (lVal . unToken)
--      , try $ lval' pos
--      , try $ expr' pos
      , try $ paren' pos
      ]
      where
        lval' pos = do
            l <- lval
            tok' TokLBracket
            e <- expr
            tok' TokRBracket
            return $ LValBrac pos l e
        expr' pos = do
            e <- expr
            tok' TokExp
            return $ LValPow pos e
        paren' pos = do
            tok' TokLParen
            l <- lval
            tok' TokRParen
            return $ LValRec pos l

rval :: Parser (RVal SourcePos)
rval = do
    pos <- getPosition
    r <- choice
        [ try simplerval
        , try $ lrval pos
        ]
    sub <- subrval
    return $ toRVal pos r sub
      where
        lrval pos = do
            l <- simplelval
            subl <- sublval
            let l' = toLVal pos l subl
            BiSubRVal _ bi e <- biRVal
            return $ RValBiNop pos (LExpr pos l') bi e

biRVal :: Parser (BiSubRVal SourcePos)
biRVal = do
    pos <- getPosition
    bi <- binop
    e <- expr
    return $ BiSubRVal pos bi e

subrval :: Parser (SubRVal SourcePos)
subrval = do
    pos <- getPosition
    m <- try $ optionMaybe $ choice
        [ try $ birval pos
        , try $ powr pos
        ]
    case m of
        Just s -> return s
        Nothing -> return $ SubRValEmpty pos
      where
        powr pos = do
            tok' TokExp
            subl <- sublval
            BiSubRVal _ bi e <- biRVal
            s <- subrval
            return $ SubRValMutRec pos subl bi e s
        birval pos = do
            BiSubRVal _ bi e <- biRVal
            s <- subrval
            return $ SubRValSimple pos bi e s

toRVal :: SourcePos -> RVal SourcePos -> SubRVal SourcePos -> RVal SourcePos
toRVal pos = go
    where
        go r (SubRValMutRec p' subl bi e subr) =
            -- TODO fix positions here.
            let e' = RExpr pos r
                l = LValPow pos e'
                l' = toLVal pos l subl
                e'' = LExpr pos l'
                r' = RValBiNop pos e'' bi e
            in go r' subr
        go r (SubRValSimple p' bi e subr) =
            go (RValBiNop pos (RExpr pos r) bi e) subr
        go r (SubRValEmpty p') = r

simplerval :: Parser (RVal SourcePos)
simplerval = do
    pos <- getPosition
    let ls =
            [ satisfy' (simple pos . unToken)
            , rec pos
            , RValCal pos <$> call
            , at pos
            , unop' pos
            ]
    choice ls
        where
            simple pos (TokNum n) = Just $ RValInt pos n
            simple pos TokTrue = Just $ RValTrue pos
            simple pos TokFalse = Just $ RValFalse pos
            simple pos (TokReal (Computed r)) = Just $ RValReal pos r
            simple pos (TokChar c) = Just $ RValChar pos c
            simple pos TokNil = Just $ RValNil pos
            simple _ _        = Nothing

            rec pos = do
                    tok' TokLParen
                    r <- rval
                    tok' TokRParen
                    return $ RValParen pos r

            at pos = do
                tok' TokAt
                RValAt pos <$> lval

            unop' pos = do
                u <- unop
                RValUnop pos u <$> expr

            binop' pos =
                RValBiNop pos <$> expr <*> binop <*> expr

call :: Parser (Call SourcePos)
call = do
    pos <- getPosition
    nm <- name
    tok' TokLParen
    es <- optionMaybe $ do
        e <- expr
        es <- commas $ expr
        return $ e : es
    tok' TokRParen
    return $ Call pos nm (fromMaybeLs es)

expr :: Parser (Expr SourcePos)
expr = do
    pos <- getPosition
    choice
        [ LExpr pos <$> lval
         -- RExpr pos <$> rval
        ]

typeT :: Parser (Type SourcePos)
typeT = do
    pos <- getPosition
    choice
        [ tok TokIntegerT >> return (TInt pos)
        , tok TokRealT    >> return (TReal pos)
        , tok TokBooleanT >> return (TBoolean pos)
        , tok TokCharT    >> return (TChar pos)
        , arrayT
        , refT
        ]

    where
        arrayT = do
            tok TokArray
            m <- optionMaybe bracInt
            tok TokOf
            TArray m <$> typeT
        bracInt = do
            tok TokLBracket
            n <- satisfy' unInt
            tok TokRBracket
            return n
        unInt (At _ (TokNum n)) = Just n
        unInt _                 = Nothing
        refT = do
            tok TokExp
            RefT <$> typeT

formal :: Parser (Formal SourcePos)
formal = do
    pos <- getPosition
    m <- optionMaybe (tok' TokVar)
    n <- name
    ns <- commas name
    tok' TokColon
    Formal pos (isJust m) n ns <$> typeT

commas :: Parser a -> Parser [a]
commas p = many $ do
    tok' TokComma
    p

semicolons :: Parser a -> Parser [a]
semicolons p = many $ do
    tok' TokSemic
    p

name :: Parser (Name SourcePos)
name = do
    pos <- getPosition
    let unLit (At _ (TokIdent s)) = Just $ Name pos s
        unLit _                   = Nothing
    satisfy' unLit

unop :: Parser (Unop SourcePos)
unop = do
    pos <- getPosition
    choice
        [ tok TokNot   >> return (OpNot pos)
        , tok TokPlus  >> return (OpPlus pos)
        , tok TokMinus >> return (OpMinus pos)
        ]

binop :: Parser (Binop SourcePos)
binop = do
    pos <- getPosition
    choice
      [ tok TokPlus >> return (BiPlus pos)
      , tok TokMinus >> return (BiMinus pos)
      , tok TokStar >> return (BiStar pos)
      , tok TokSlash >> return (BiSlash pos)
      , tok TokDiv >> return (BiDiv pos)
      , tok TokMod >> return (BiMod pos)
      , tok TokOr >> return (BiOr pos)
      , tok TokAnd >> return (BiAnd pos)
      , tok TokEq >> return (BiEq pos)
      , tok TokNEq >> return (BiNotEq pos)
      , tok TokLT >> return (BiLT pos)
      , tok TokLEq >> return (BiLEq pos)
      , tok TokGT >> return (BiGT pos)
      , tok TokGEq >> return (BiGEq pos)
      ]

fromMaybeLs :: Maybe [a] -> [a]
fromMaybeLs Nothing = []
fromMaybeLs (Just ls) = ls
