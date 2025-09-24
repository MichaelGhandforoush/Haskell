module APL.Parser (parseAPL, lInteger) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)
import Data.Data (isAlgType)
import GHC.Arr (safeIndex)

-- Do not change this definition.
type Parser = Parsec Void String

pExp0 :: Parser Exp
pExp0 = do
  x <- pAtom
  chain x
  where
    chain x =
      choice
        [ 
          do
            lKeyword "/"
            y <- pExp1
            chain $ Div x y,
          do
            lKeyword "*"
            y <- pExp1
            chain $ Mul x y,
          pure x
        ]
pExp1 :: Parser Exp
pExp1 = do
  x <- pAtom
  chain x
  where
    chain x =
      choice
        [ do
            lKeyword "+"
            y <- pAtom
            chain $ Add x y,
          do
            lKeyword "-"
            y <- pAtom
            chain $ Sub x y,
            pure x] 

pLExp :: Parser Exp
pLExp = do
  x <- lKeyword "if" *> pAtom
  chain x where
  chain x = 
    choice [
      do
        lKeyword "then"
        y <- pExp0
        lKeyword "else"
        z <- pExp0
        chain $ If x y z,
        pure x]


pAtom :: Parser Exp
pAtom = choice [
  CstInt <$> lInteger,
  Var <$> lVName,
  CstBool <$> pBool,
  lKeyword "(" *> pExp <* lKeyword ")"]

pExp :: Parser Exp
pExp = pLExp

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x


lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)
{- lInteger = do
  digits <- some parseDigits
  pure $ loop 1 digits
  where
    parseDigits = do 
      c <- satisfy isDigit
      pure $ read [c]
    loop _ [] = 0
    loop w (d:ds) = d*w + loop (w*10) ds -}
  
lexeme :: Parser a -> Parser a
lexeme p = p <* space 

keywords :: [String]
keywords = ["if", "then", "else", "true", "false", "let", "in", "try", "catch", "print", "put", "get"]

lVName :: Parser VName
--lVName = lexeme $ some (satisfy isAlphaNum)
lVName = lexeme $ try $ do 
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum)
  if first : rest `elem` keywords
    then fail "keywords"
    else pure $ first : rest 

pBool :: Parser Bool
pBool = choice [
  lKeyword "true" >> pure True,
  lKeyword "false" >> pure False]


lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)


