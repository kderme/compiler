module Language.PCL.Annotated.Semantic where

import           Prelude

import Control.Monad (forM)
import qualified Data.Map.Strict as M
-- import Control.Monad.State.Strict

import Language.PCL.Annotated.Lexer hiding (tok)
import Language.PCL.Annotated.Parser
import Language.PCL.Token
import Language.PCL.Annotated.Environment
import Language.PCL.Annotated.Syntax
import Text.Parsec hiding (State)
import Text.Parsec.Tok

verifyProgram :: Program SourcePos -> Either String ()
verifyProgram (Program _ (Name _ str) (Body _ ls (Block _ stms))) =
    let ret = fmap snd $ runState (getLocalsEnv ls) $ initEnv str
    in case ret of
        Left s -> Left s
        Right en -> checkTypeStms en stms

getEnv :: Program SourcePos -> Either String Environment
getEnv (Program _ (Name _ str) (Body _ ls _)) =
    fmap snd $ runState (getLocalsEnv ls) $ initEnv str

getBodyEnv :: Body SourcePos -> StateMonad ()
getBodyEnv (Body _ ls (Block _ stms)) = do
    getLocalsEnv ls
    en <- get
    liftEither $ checkTypeStms en stms

getLocalsEnv :: [Local SourcePos] -> StateMonad ()
getLocalsEnv = mapM_ getLocalEn

getLocalEn :: Local SourcePos -> StateMonad ()
getLocalEn = go
    where
        go (Var _ dls) = mapM_ getDefEnv dls
        go (Label _ ns) = mapM_ (\(Name pos str) -> modifyEnv False (LEnv pos) str pos) ns
        go (HeaderL _ h b) = do
            fun <- getHeadEnv h True
            en <- get
            scopeEnv fun
            getBodyEnv b
            put en
            -- TODO also verify the block b
        go (ForL _ h) = getHeadEnv h False >> return ()

modifyEnv :: Bool -> Env -> String -> SourcePos -> StateMonad ()
modifyEnv replace newEnv str pos = do
    en <- get
    case (M.lookup str (env en), replace) of
        (Nothing, _) -> modify' $ \e ->
            e { env = M.insert str newEnv (env e)
            , incr = incr e + 1
            }
        (Just e', False) -> failEnv $ doubleDef str (getPos e') pos
        (Just e', True) -> modify' $ \e ->
            e { env = M.insert str newEnv (env e)
            , incr = incr e + 1
            }

getDefEnv :: DefIdList SourcePos -> StateMonad ()
getDefEnv (DefIdList _ ns tp) =
    mapM_ (\(Name pos str) -> modifyEnv False (VEnv (forgetF tp) pos) str pos) ns

getHeadEnv :: Header SourcePos -> Bool -> StateMonad Fun
getHeadEnv (Procedure pos (Name _ str) fs) bl = do
    let ret = Fun pos (concatMap getArgs fs) Nothing
    modifyEnv False ((if bl then FunEnv else ForEnv) ret) str pos
    return ret
getHeadEnv (Function pos (Name _ str) fs t) bl = do
    let ret = Fun pos (concatMap getArgs fs) (Just $ forgetF t)
    modifyEnv False ((if bl then FunEnv else ForEnv) ret) str pos
    return ret

scopeEnv :: Fun -> StateMonad ()
scopeEnv fun = do
    en <- get
    mapM_ (\(_, str, t, pn) -> modifyEnv True (VEnv t pn) str pn) (fargs fun)
    case fret fun of
        Nothing -> return ()
        Just t -> modifyEnv False (VEnv t $ srcPos fun) "result" $ srcPos fun

getArgs :: Formal SourcePos -> [(Bool, String, Tp, SourcePos)]
getArgs (Formal _ bl ns t) = fmap (\(Name pn str) -> (bl, str, forgetF t, pn)) ns

forgetF :: Functor f => f a -> f ()
forgetF = fmap (const ())

doubleDef :: String -> SourcePos -> SourcePos -> String
doubleDef str p p' = concat
        [ "Id "
        , str
        , " defined first time in "
        , show p
        , " and second time in "
        , show p'
        ]

infereType :: Environment -> Expr SourcePos -> Either String Tp
infereType en (LExpr _ l) = infereTypeL en l
infereType en (RExpr _ r) = infereTypeR en r

isLeft :: Expr a -> Bool
isLeft (LExpr _ _) = True
isLeft _           = False

-- First list is the real parameters and second the typical.
-- Bools indicate for the real par that it's an lvalue and for the
-- typical that it passes by reference.
compareArgs :: String -> [(Bool, Tp)] -> [(Bool, Tp)] -> Either String ()
compareArgs name = go 1
    where
        go _ [] [] = Right ()
        go n _ [] = Left $ concat
                [ "calling of function "
                   , show name
                   , ": real parameters are more"]
        go n [] _ = Left $ concat
                [ "calling of function "
                , show name
                , ": real parameters are less"]
        go n ((islval, a):as) ((isvar, b):bs) = case (islval, isvar) of
            (False, True) -> Left $ concat
                [ "calling of function "
                , show name
                , ": real parameter number "
                , show n
                , " should be an l-value since it passes by reference" ]
            (True, True) -> do
                checkCompatible (RefT () b) (RefT () a)
                go (n+1) as bs
            _ -> do
                checkCompatible b a
                go (n+1) as bs

infereTypeR :: Environment -> RVal SourcePos -> Either String Tp
infereTypeR en = go
  where
    go (RValInt _ _) = Right $ TInt ()
    go (RValTrue _ ) = Right $ TBoolean ()
    go (RValFalse _) = Right $ TBoolean ()
    go (RValReal _ _) = Right $ TReal ()
    go (RValChar _ _) = Right $ TChar ()
    go (RValParen _ r) = go r
    go (RValNil _) = Right $ TNil ()
    go (RValCal p c@(Call _ (Name _ n) _)) = case infereCall en c of
        Left err -> Left err
        Right Nothing -> Left $ concat
            -- TODO: should this be an error or should it just be
            -- untyped.
            ["r-value called procedure ", show n, " at ", show p]
        Right (Just t) -> Right t
    go (RValAt p l) = do
        t <- infereTypeL en l
        Right $ RefT () t
    go (RValUnop p u expr) = do
        t <- infereType en expr
        case (u, t) of
            (OpNot _, TBoolean _) -> Right $ TBoolean ()
            (OpNot p, t) -> Left $ concat
                ["not at ", show p, " should be applied to boolean, not ", show t]
            (OpPlus _, TInt _) -> Right $ TInt ()
            (OpPlus _, TReal _) -> Right $ TReal ()
            (OpPlus _, t) -> Left $ concat
                ["+ at ", show p, " should be applied to numerical, not ", show t]
            (OpMinus _, TInt _) -> Right $ TInt ()
            (OpMinus _, TReal _) -> Right $ TReal ()
            (OpMinus _, t) -> Left $ concat
                ["- at ", show p, " should be applied to numerical, not ", show t]
    go (RValBiNop pos e1 bi e2) = do
        t <- infereType en e1
        t' <- infereType en e2
        preLeft pos $ case (bi, t, t') of
            (BiPlus _, t1, t2)  -> areNumerical t1 t2
            (BiMinus _, t1, t2) -> areNumerical t1 t2
            (BiStar _, t1, t2)  -> areNumerical t1 t2
            (BiSlash _, t1, t2) -> do
                _ <- areNumerical t1 t2
                Right $ TReal ()
            (BiDiv _, TInt _, TInt _) -> Right (TInt ())
            (BiMod _, TInt _, TInt _) -> Right (TInt ())
            (BiOr _, TBoolean _, TBoolean _) -> Right (TBoolean ())
            (BiAnd _, TBoolean _, TBoolean _) -> Right (TBoolean ())
            (BiEq _, t1, t2) -> eqOrNeq t1 t2
            (BiNotEq _, t1, t2) -> eqOrNeq t1 t2
            (BiLT _, t1, t2) -> fmap (const $ TBoolean ()) $ areNumerical t1 t2
            (BiLEq _, t1, t2) -> fmap (const $ TBoolean ()) $ areNumerical t1 t2
            (BiGT _, t1, t2) -> fmap (const $ TBoolean ()) $ areNumerical t1 t2
            (BiGEq _, t1, t2) -> fmap (const $ TBoolean ()) $ areNumerical t1 t2
            _ -> Left "wrong type of arguments 1"

areNumerical :: Tp -> Tp -> Either String Tp
areNumerical (TInt _) (TInt _)   = Right (TInt ())
areNumerical (TReal _) (TInt _)  = Right (TReal ())
areNumerical (TInt _) (TReal _)  = Right (TReal ())
areNumerical (TReal _) (TReal _) = Right (TReal ())
areNumerical _ _ = Left "wrong type of arguments 2"

-- TODO: manual says compare binary represantation of types.
-- types cannot be arrays.
-- QQ: Can it be reference??
eqOrNeq :: Tp -> Tp -> Either String Tp
eqOrNeq (TInt _) (TInt _)   = Right (TBoolean ())
eqOrNeq (TReal _) (TInt _)  = Right (TBoolean ())
eqOrNeq (TInt _) (TReal _)  = Right (TBoolean ())
eqOrNeq (TReal _) (TReal _) = Right (TBoolean ())
eqOrNeq (TChar _) (TChar _) = Right (TBoolean ())
eqOrNeq (TBoolean _) (TBoolean _) = Right (TBoolean ())
eqOrNeq t t' = Left $ concat
    ["can't compare equality for types ", show t, " and ", show t']

infereCall :: Environment -> Call SourcePos -> Either String (Maybe Tp)
infereCall en (Call p (Name _ n) exprs) = case M.lookup n (env en) of
    Just (FunEnv (Fun _ args mtp)) -> do
        realArgs <- forM exprs $ \e -> do
                t <- infereType en e
                return (isLeft e, t)
        let typicArgs = (\(isvar, _ , t, _) -> (isvar, t)) <$> args
        preLeft p $ compareArgs n realArgs typicArgs
        return mtp
    Nothing -> Left $ concat
            -- TODO: if it's inside a function, we should also check forward.
            ["function ", n, " called at ", show p, " not defined"]

infereTypeL :: Environment -> LVal SourcePos -> Either String Tp
infereTypeL e = go
    where
    go (LValId _ (Name p str)) = infereId e p str
    go (Result p) = infereId e p "result"
    go (LValStr p str) = Right $ TArray () (Just $ length str + 1) (TChar ())
    go (LValBrac p l expr) = do
        t <- infereTypeL e l
        t' <- infereType e expr
        case (t, t') of
            (TArray _ _msize tp, TInt a) -> Right tp
            -- if msize can be evaluated, we may want to check on runtime the size?
            (t, TInt a) -> Left $ concat
                ["l-value ", show l, "should have type array, but instead has", show t]
            (TArray _ _msize tp, t) -> Left $ concat
                ["type of array, at ", show (ann expr), " should be integer, but it is ", show t, " instead"]
    go (LValPow p expr) = do
        t <- infereType e expr
        case t of
            RefT _ tp -> Right tp
            _ -> Left $ concat
                ["tried to dereference type ", show t, " at ", show p]
    go (LValRec p l) = go l

infereId :: Environment -> SourcePos -> String -> Either String Tp
infereId e p str = case M.lookup str (env e) of
    Just (VEnv tp _) -> Right tp
    Nothing -> Left $ concat $ ["Id ", str, " at ", show p, " not defined"]

checkLabel :: Environment -> SourcePos -> String -> Either String ()
checkLabel en p str = case M.lookup str (env en) of
    Just (LEnv _) -> Right ()
    Nothing -> Left $ concat $ ["label ", str, " at ", show p, " not defined"]

checkTypeStms :: Environment -> [Stm SourcePos] -> Either String ()
checkTypeStms en = mapM_ (checkTypeStm en)

checkCompatible :: Tp -> Tp -> Either String ()
checkCompatible = go
    where
        failed t t' = concat
            ["incompatible types between "
            , show t, " and ", show t']
        go (TReal _) (TInt _) = Right ()
        go (RefT _ (TArray _ Nothing tt)) (RefT _ (TArray _ (Just _) tt')) =
            if tt == tt' then Right () else Left "incompatible types of arrays"
        go t t' = if t == t' then Right () else Left $ failed t t'

checkTypeStm :: Environment -> Stm SourcePos -> Either String ()
checkTypeStm en = go
    where

    go (EmptyStm _) = Right ()
    go (AssignStm p l expr) = do
        t <- infereTypeL en l
        t' <- infereType en expr
        checkCompatible t t'
    go (BlockStm _ (Block a stmts)) = checkTypeStms en stmts
    go (CallStm _ c@(Call _ _ _)) = do
        _ <- infereCall en c
        return ()
    go (IfStm p e stm melsestm) = do
        t <- infereType en e
        case t of
            TBoolean _ -> do
                go stm
                case melsestm of
                    Nothing -> Right ()
                    Just stmelse -> go stmelse
            _ -> Left $ concat ["if expression at ", show p, " should be boolean not ", show t]
    go (WhileStm p e st) = do
        t <- infereType en e
        case t of
            TBoolean _ -> go st
            _ -> Left $ concat ["while expression at ", show p, " should be boolean not ", show t]

    go (ColonStm _ (Name p' str) stm) = do
        _ <- checkLabel en p' str
        go stm
    go (GotoStm _ (Name p str)) = checkLabel en p str
    go (ReturnStm _ ) = Right ()
    go (NewStm p Nothing l) = preLeft p $ checkLRef en l
    go (NewStm p (Just e) l) = preLeft p $ checkLArray en l >> checkInt en e
    go (DisposeStm p False l) = preLeft p $ checkLRef en l
    go (DisposeStm p True l) = preLeft p $ checkLArray en l

checkLRef :: Environment -> LVal SourcePos -> Either String ()
checkLRef en l = do
    lt <- infereTypeL en l
    case lt of
        RefT _ tp -> do -- ^t where t is complete
            b <- isCompleteType tp
            if b then Right () else
             Left "can only reference a complete type"
        _ -> Left "type should be reference"

checkLArray :: Environment -> LVal SourcePos -> Either String ()
checkLArray en l = do
    lt <- infereTypeL en l
    case lt of
        -- ^array of t and integer
        RefT _ (TArray _ Nothing _) -> Right ()
        _ -> Left $ "wrong type: should be ^array not complete of some type t"

checkInt :: Environment -> Expr SourcePos -> Either String ()
checkInt en e = do
    et <- infereType en e
    case et of
        TInt _ -> Right ()
        _ -> Left "wrong type: should be int"

preLeft :: SourcePos -> Either String a -> Either String a
preLeft pos ret = case ret of
    Left str -> Left $ prefixPosition pos str
    Right r -> Right r

prefixPosition :: SourcePos -> String -> String
prefixPosition pos str = concat
        ["At ", show pos, ": ", str]