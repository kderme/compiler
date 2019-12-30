module Toy.LLVM where

import Control.Monad
import Data.ByteString.Short.Internal
import qualified Data.ByteString.Char8 as S8

import Language.PCL.Codegen
import qualified LLVM.AST as AST
import LLVM.AST.Type hiding (double)
import LLVM.AST hiding (type')
import LLVM.AST.Global

import LLVM.Internal.Context
import LLVM.Internal.Module

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name $ toShort $ S8.pack x))

llvm :: LLVM ()
llvm =
    define double "myadd" fnargs bb
    where
        args = ["x", "y"]
        fnargs = toSig args
        bb = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
                var <- alloca double
                store var (local (AST.Name $ toShort $ S8.pack a))
                assign a var
            x <- getvar "x"
            -- y <- getvar "y" >>= load
            z <- call -- fadd x x
            ret z

myModule :: AST.Module
myModule = runLLVM (emptyModule "toy") llvm

runJIT :: AST.Module -> IO () -- (Either String ())
runJIT mod = do
  withContext $ \context ->
    withModuleFromAST context mod $ \m -> do
      s <- moduleLLVMAssembly m
      print s


------------------------------------------------------

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name $ toShort $ S8.pack "add"
  , parameters =
      ( [ Parameter int (Name $ toShort $ S8.pack "a") []
        , Parameter int (Name $ toShort $ S8.pack "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name $ toShort $ S8.pack "entry")
        [ Name (toShort $ S8.pack "result") :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name $ toShort $ S8.pack "a"))
                (LocalReference int (Name $ toShort $ S8.pack "b"))
                []
            , Name (toShort $ S8.pack "alloc") := Alloca (IntegerType 32) Nothing 0 []]
        (Do $ Ret (Just (LocalReference int (Name $ toShort $ S8.pack "result"))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = toShort $ S8.pack "basic"
  , moduleDefinitions = [defAdd]
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  -- llvm <- withModuleFromAST ctx mod moduleLLVMAssembly -- writeLLVMAssemblyToFile (File "binary") ) -- moduleLLVMAssembly
  withModuleFromAST ctx mod $ writeLLVMAssemblyToFile (File "binary")
  -- S8.putStrLn llvm
