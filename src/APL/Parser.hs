module APL.Parser (parseAPL) where

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
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "put",
    "get",
    "loop",
    "for",
    "while",
    "do"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice
    [ True <$ lKeyword "true",
      False <$ lKeyword "false"
    ]

pAtom2 :: Parser Exp
pAtom2 =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      try $ lString "(" *> pExp <* lString ")",
      KvPut <$> (lKeyword "put" *> pAtom) <*> pAtom,
      KvGet <$> (lKeyword "get" *> pAtom),
      try $ do
        lString "("
        e <-  pExp
        es <- some (lString "," *> pExp)
        lString ")"
        pure $ Tuple (e : es),
      do
        lString "("
        lString ")"
        pure $ Tuple []
    ]

pAtom :: Parser Exp
pAtom = do
  x <- pAtom2
  choice
    [
      do
        lString "."
        Project x <$> lInteger,
        pure x
    ]

pFExp :: Parser Exp
pFExp = chain =<< pAtom
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      do
        lKeyword "loop"
        v1 <- lVName
        e1 <- lString "=" *> pExp
        choice [
          do 
            lKeyword "for"
            v2 <- lVName
            lString "<"
            e2 <- pExp
            lKeyword "do"
            ForLoop (v1, e1) (v2, e2) <$> pExp,
          do
            lKeyword "while"
            e2 <- pExp
            lKeyword "do"
            WhileLoop (v1, e1) e2 <$> pExp
          ],
      pFExp
    ]

pExp3 :: Parser Exp
pExp3 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp3
            chain $ Add x y,
          do
            lString "-"
            y <- pExp3
            chain $ Sub x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp2
            chain $ Eql x y,
          pure x
        ]
pExp02 :: Parser Exp
pExp02 = pExp1 >>= chain
  where
    chain x = 
      choice
        [
          do
            lString "&&"
            y <- pExp1
            chain $ BothOf x y,
          pure x
        ]
pExp01 :: Parser Exp
pExp01 = pExp02 >>= chain
  where
    chain x = 
      choice
        [
          do
            lString "||"
            y <- pExp02
            chain $ OneOf x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp01

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
