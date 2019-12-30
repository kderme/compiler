module Language.PCL.Emit where

cgenProgram :: Program a -> LLVM ()
cgenProgram (Program _ (Name _ nm) (Body _ ls (Block _ stmts))) = do
    mapM_ cgenLocal ls
    mapm cgenStmt stmts


cgenLocal :: Local a -> LLVM ()
cgenLocal (Var _ defs) = mapm_ cgenDef defs
cgenLocal _ = error "not supported local"
    where
        cgendefId _ str = do
            defineVar double str

        cgenDef (DefIdList _ nms tp) = do
            mapM_ (\(Name _ str) -> cgendefId tp str) nms

cgenStmt :: Stm a -> LLVM ()
cgenStmt (EmptyStm _) = return ()
cgenStmt (AssignStm _ lval expr) = 
cgenStmt _ = error "stmt not supported yet"

cgenExpr :: Expr a -> LLVM ()
cgenExpr (LExpr _ l) = cgenL l
cgenExpr (RExpr _ r) = cgenR r

cgenL :: LVal a -> LLVM ()
cgenL ()

cgenL :: RVal a -> LLVM ()
cgenL

cgen :: S.Expr a -> Codegen AST.Operand
cgen ()