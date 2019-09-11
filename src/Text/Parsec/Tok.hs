module Text.Parsec.Tok where

import           Prelude
import           Text.Parsec hiding (satisfy, string)

import Language.PCL.Annotated.Lexer hiding (tok)
import Language.PCL.Token

type Tok = At AlexPosn Token

type Parser = Parsec [Tok] ()

-- | This parser succeeds whenever the given predicate returns true when called with
-- parsed `Tok`. Same as 'Text.Parsec.Char.satisfy'.
satisfy :: Monad m => (Tok -> Bool) -> ParsecT [Tok] u m Token
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Tok -> Maybe Token
    tokeq t = if f t then Just (unToken t) else Nothing

satisfy' :: Monad m => (Tok -> Maybe a) -> ParsecT [Tok] u m a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Tok -> [Tok] -> SourcePos
nextPos pos _ (At (AlexPn _ l c) _ :_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                       = pos

-- | Parses given `Token`.
tok :: Monad m => Token -> ParsecT [Tok] u m Token
tok t = satisfy (\(At _ t') -> t' == t) <?> show t

tok' :: Monad m => Token -> ParsecT [Tok] u m ()
tok' p = tok p >> return ()
