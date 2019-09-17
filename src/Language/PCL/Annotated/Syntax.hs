{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}

module Language.PCL.Annotated.Syntax where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data, Typeable)
import           GHC.Generics    (Generic)
import           Prelude         hiding (EQ, GT, LT)

import Language.PCL.Token

data Name a = Name a String deriving (Show, Functor, Data, Typeable, Generic)

data Program a = Program a (Name a) (Body a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Body a = Body a [Local a] (Block a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Local a = Var a [DefIdList a]
             | Label a [Name a]
             | HeaderL a (Header a) (Body a)
             | ForL a (Header a)
             deriving (Show, Eq, Functor, Data, Typeable, Generic)

data DefIdList a = DefIdList a [Name a] (Type a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Header a = Procedure a (Name a) [Formal a]
              | Function a (Name a) [Formal a] (Type a)
              deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Formal a = Formal a Bool [Name a] (Type a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Type a = TInt a | TReal a | TBoolean a | TChar a
            | TArray a (Maybe Int) (Type a)
            | RefT a (Type a)
            | TNil a
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Block a = Block a [Stm a]
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Stm a = EmptyStm a
           | AssignStm a (LVal a) (Expr a)
           | BlockStm a (Block a)
           | CallStm a (Call a)
           | IfStm a (Expr a) (Stm a) (Maybe (Stm a))
           | WhileStm a (Expr a) (Stm a)
           | ColonStm a (Name a) (Stm a)
           | GotoStm a (Name a)
           | ReturnStm a
           | NewStm a (Maybe (Expr a)) (LVal a)
           | DisposeStm a Bool (LVal a)
           deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Expr a = LExpr a (LVal a) | RExpr a (RVal a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data LVal a = LValId a (Name a)
            | Result a
            | LValStr a String
            | LValBrac a (LVal a) (Expr a)
            | LValPow a (Expr a)
            | LValRec a (LVal a)
            deriving (Show, Eq, Functor, Data, Typeable, Generic)

data SubLVal a = SubLValBrac a (Expr a) (SubLVal a)
               | SubLValPow a (SubLVal a)
               | SubLValEmpty a
               deriving (Show, Eq, Functor, Data, Typeable, Generic)

data RVal a = RValInt a Int
            | RValTrue a
            | RValFalse a
            | RValReal a Double
            | RValChar a Char
            | RValParen a (RVal a)
            | RValNil a
            | RValCal a (Call a)
            | RValAt a (LVal a)
            | RValUnop a (Unop a) (Expr a)
            | RValBiNop a (Expr a) (Binop a) (Expr a)
            deriving (Show, Eq, Functor, Data, Typeable, Generic)

data SubRVal a = SubRValMutRec a (SubLVal a) (Binop a) (Expr a) (SubRVal a)
               | SubRValSimple a (Binop a) (Expr a) (SubRVal a)
               | SubRValEmpty a
               deriving (Show, Eq, Functor, Data, Typeable, Generic)

data BiSubRVal a = BiSubRVal a (Binop a) (Expr a)
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Call a = Call a (Name a) [Expr a]
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Unop a = OpNot a | OpPlus a | OpMinus a
    deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Binop a = BiPlus a
             | BiMinus a
             | BiStar a
             | BiSlash a
             | BiDiv a
             | BiMod a
             | BiOr a
             | BiAnd a
             | BiEq a
             | BiNotEq a
             | BiLT a
             | BiLEq a
             | BiGT a
             | BiGEq a
             deriving (Show, Eq, Functor, Data, Typeable, Generic)

data Silent = Silent

instance Show Silent where
    show _ = ""

silent :: Program a -> Program Silent
silent = fmap (const Silent)

getNamePos :: Name a -> a
getNamePos (Name a _) = a

instance Eq (Name a) where
    (Name _ s1) == (Name _ s2) = s1 == s2

instance Ord (Name a) where
    compare (Name _ a) (Name _ b) = compare a b

class Functor ast => Annotated ast where
    -- | Retrieve the annotation of an AST node.
    ann :: ast l -> l
    -- | Change the annotation of an AST node. Note that only the annotation of
    --   the node itself is affected, and not the annotations of any child nodes.
    --   if all nodes in the AST tree are to be affected, use 'fmap'.
    amap :: (l -> l) -> ast l -> ast l

instance Annotated Expr where
    ann (LExpr p _) = p
    ann (RExpr p _) = p

-- instance Annotated SubLVal where
--     ann (SubLValBrac a _) = a
--     ann (SubLValPow a)    = a
--     ann (SubLValEmpty a)  = a

-- instance Annotated Name where
--     ann (Name a _) = a
--     amap f (Name a x1) = Name (f a) x1
-- 
-- instance Annotated Program where
--     ann ()

