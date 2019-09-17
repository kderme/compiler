{-# LANGUAGE FlexibleInstances #-}

-- | Lua pretty-printer.
module Language.PCL.PrettyPrinter
  ( pprint
  , renderPretty
  , displayS
  , displayIO
  , LPretty
  ) where

import           Prelude                 hiding (EQ, GT, LT, lines, (<$>))

import           Text.PrettyPrint.Leijen

import           Language.PCL.Annotated.Syntax

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

fold f []       = empty
fold f ds       = foldr1 f ds

commas :: [Doc] -> Doc
commas = fold (\x y -> x <> text "," <> y) -- encloseSep empty empty (text ", ")

semis :: [Doc] -> Doc
semis = fold (\x y -> x <> text "; " <> y) -- encloseSep empty empty (text ", ")

type Precedence = Int

class LPretty a where
    pprint :: a -> Doc
    pprint = pprint' 0

    pprint' :: Precedence -> a -> Doc
    pprint' _ = pprint

instance LPretty [Char] where
    pprint = text

instance LPretty Bool where
    pprint True  = text "true"
    pprint False = text "false"

instance LPretty (Expr a) where
    pprint (LExpr a l) = pprint l
    pprint (RExpr a r) = pprint r

instance LPretty (Name a) where
    pprint (Name _ str) = text str

instance LPretty (Binop a) where
    pprint (BiPlus a)  = char '+'
    pprint (BiMinus a) = char '-'
    pprint (BiStar a)  = char '*'
    pprint (BiSlash a) = char '/'
    pprint (BiDiv a)   = text "div"
    pprint (BiMod a)   = text "mod"
    pprint (BiOr a)    = text "or"
    pprint (BiAnd a)   = text "and"
    pprint (BiEq a)    = text "="
    pprint (BiNotEq a) = text "<>"
    pprint (BiLT a)    = text "<"
    pprint (BiLEq a)   = text "<="
    pprint (BiGT a)    = text ">"
    pprint (BiGEq a)   = text ">="

instance LPretty (Unop a) where
    pprint (OpNot _)   = text "not"
    pprint (OpPlus _)  = text "+"
    pprint (OpMinus _) = text "-"

instance LPretty (LVal a) where
    pprint (LValId _ n) = pprint n
    pprint (Result _)   = text "result"
    pprint (LValStr _ str) = text str
    pprint (LValBrac _ l e) = pprint l <> text "[" <> pprint e <> text "]"
    pprint (LValPow _ e) =  pprint e <> text "^"
    pprint (LValRec _ l) = pprint l

instance LPretty (RVal a) where
    pprint (RValInt _ n)  = pprint $ show n
    pprint (RValTrue _ )  = text "true"
    pprint (RValFalse _ ) = text "false"
    pprint (RValReal _ d) = pprint $ show d
    pprint (RValChar _ c) = pprint $ show c
    pprint (RValParen _ r) = pprint r
    pprint (RValNil _ )    = text "nil"
    pprint (RValCal _ c)   = text "TODO: call"
    pprint (RValAt _ r)     = text "@" <> pprint r
    pprint (RValUnop _ u e) = pprint u <> pprint e
    pprint (RValBiNop _ e1 bi e2)   = text "(" <> pprint e1 <> pprint bi <> pprint e2 <> text ")"


instance LPretty (Program a) where
    pprint (Program _ n b) = empty <$> text "Program " <> pprint n <> text ":" <$> pprint b

instance LPretty (Body a) where
    pprint (Body _ ls b) = vsep (fmap pprint ls) <$> pprint b

instance LPretty (Local a) where
    pprint (Var _ defs) = text "var" <+> commas (fmap pprint defs)
    pprint (Label _ ns) = text "label" <+> list (fmap pprint ns)
    pprint (HeaderL _ h b) = pprint h <> text ";" <> pprint b <> text ";"
    pprint (ForL _ h) = text "forward" <+> pprint h

instance LPretty (DefIdList a) where
    pprint (DefIdList _ ns t) = commas (fmap pprint ns) <> text ":" <> pprint t <> text ";"

instance LPretty (Header a) where
    pprint (Procedure _ n fs) =
        pprint "procedure" <+> pprint n <> semis (fmap pprint fs)
    pprint (Function _ n fs t) =
        pprint "function" <+> pprint n <> text "(" <> semis (fmap pprint fs) <> text "):" <> pprint t

instance LPretty (Formal a) where
    pprint (Formal _ bl ns t) = printmvar bl <> commas (fmap pprint ns)
                                  <> text ":" <> pprint t

printmvar :: Bool -> Doc
printmvar True = text "var "
printmvar False = empty

instance LPretty (Type a) where
    pprint (TInt _) = text "integer"
    pprint (TReal _) = text "real"
    pprint (TBoolean _) = text "boolean"
    pprint (TChar _) = text "char"
    pprint (TArray _ m t) = text "array" <> pprintbrac m <+> text "of" <+> pprint t
    pprint (RefT _ t) = text "^" <> pprint t

pprintbrac :: Maybe Int -> Doc
pprintbrac Nothing = empty
pprintbrac (Just n) = text "[" <> pprint (show n) <> text "]"

instance LPretty (Block a) where
    pprint (Block _ ss) = text "begin" <$> vsep (fmap pprint ss) <$> text "end"

instance LPretty (Stm a) where
    pprint (EmptyStm _)      = empty
    pprint (AssignStm _ l e) = pprint l <> text ":=" <> pprint e
    pprint (BlockStm _ b)    = pprint b
    pprint (CallStm _ c)     = text "TODO: call"
    pprint (IfStm _ e s ms)  = text "if" <+> pprint e <+> text "then" <+> text "(" <> pprint s <> text ")" <>  pprintmElse ms
    pprint (WhileStm _ e s)  = text "while" <+> pprint e <+> text "do" <+> pprint s
    pprint (ColonStm _ n s)  = pprint n <> text ":" <> pprint s
    pprint (GotoStm _ n)     = text "goto" <> pprint n
    pprint (ReturnStm _)     = text "return"
    pprint (NewStm _ me l)   = text "new" <> pprint l
    pprint (DisposeStm _ bl l) = text "dispose" <> pprint l

pprintmElse :: Maybe (Stm a) -> Doc
pprintmElse Nothing = empty
pprintmElse (Just stm) = text " else" <+> pprint stm
