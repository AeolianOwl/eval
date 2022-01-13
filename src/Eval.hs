{-# LANGUAGE OverloadedStrings #-}

module Eval (eval) where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, many, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal, float, signed)

type Parser = Parsec Void Text

data Node
  = -- Types
    Bool Bool
  | Num Double
  | -- Boolean Expressions
    Eq Node Node
  | Ne Node Node
  | Ge Node Node
  | Gt Node Node
  | Le Node Node
  | Lt Node Node
  | And Node Node
  | Or Node Node
  | Not Node
  | -- Arithmetic Expressions
    Add Node Node
  | Sub Node Node
  | Mul Node Node
  | Div Node Node
  | Pow Node Node
  deriving (Eq, Ord)

eval :: String -> String
eval input = case parse pAll "" (pack input) of
  Left bundle -> errorBundlePretty bundle
  Right (Num a) -> show a
  Right (Bool a) -> show a
  Right _ -> ""

pAll :: Parser Node
pAll = try pBool <|> try pNum

pBoolE :: Parser Node
pBoolE = do
  _ <- char '('
  sO
  res <- try pBool <|> try pAnd <|> try pOr <|> try pNot <|> try pEq <|> try pNe <|> try pGe <|> try pGt <|> try pLe <|> try pLt
  sO
  _ <- char ')'
  return res

pNumE :: Parser Node
pNumE = do
  _ <- char '('
  sO
  res <- try pNum <|> try pAdd <|> try pSub <|> try pMul <|> try pDiv <|> try pPow
  sO
  _ <- char ')'
  return res

pBool :: Parser Node
pBool = Bool True <$ string "true" <|> Bool False <$ string "false" <|> pBoolE

pNum :: Parser Node
pNum = (Num <$> signed sN (try float <|> try decimal)) <|> pNumE

pAnd :: Parser Node
pAnd = do
  _ <- string "and"
  (a, b) <- p2Bool
  return $ Bool $ a && b

pOr :: Parser Node
pOr = do
  _ <- string "or"
  (a, b) <- p2Bool
  return $ Bool $ a || b

pNot :: Parser Node
pNot = do
  _ <- string "not"
  sR
  (Bool a) <- pBool
  return $ Bool $ not a

pEq :: Parser Node
pEq = do
  _ <- string "="
  try
    ( do
        (a, b) <- p2Num
        return $ Bool $ a == b
    )
    <|> try
      ( do
          (a, b) <- p2Bool
          return $ Bool $ a == b
      )

pNe :: Parser Node
pNe = do
  _ <- string "/="
  try
    ( do
        (a, b) <- p2Num
        return $ Bool $ a /= b
    )
    <|> try
      ( do
          (a, b) <- p2Bool
          return $ Bool $ a /= b
      )

pGe :: Parser Node
pGe = do
  _ <- string ">="
  (a, b) <- p2Num
  return $ Bool $ a >= b

pGt :: Parser Node
pGt = do
  _ <- string ">"
  (a, b) <- p2Num
  return $ Bool $ a > b

pLe :: Parser Node
pLe = do
  _ <- string "<="
  (a, b) <- p2Num
  return $ Bool $ a <= b

pLt :: Parser Node
pLt = do
  _ <- string "<"
  (a, b) <- p2Num
  return $ Bool $ a < b

pAdd :: Parser Node
pAdd = do
  _ <- string "+"
  (a, b) <- p2Num
  return $ Num $ a + b

pSub :: Parser Node
pSub = do
  _ <- string "-"
  (a, b) <- p2Num
  return $ Num $ a - b

pMul :: Parser Node
pMul = do
  _ <- string "*"
  (a, b) <- p2Num
  return $ Num $ a * b

pDiv :: Parser Node
pDiv = do
  _ <- string "/"
  (a, b) <- p2Num
  return $ Num $ a / b

pPow :: Parser Node
pPow = do
  _ <- string "**"
  (a, b) <- p2Num
  return $ Num $ a ** b

p2Num :: Parser (Double, Double)
p2Num = do
  sR
  (Num a) <- pNum
  sR
  (Num b) <- pNum
  return (a, b)

p2Bool :: Parser (Bool, Bool)
p2Bool = do
  sR
  (Bool a) <- pBool
  sR
  (Bool b) <- pBool
  return (a, b)

sN :: Parser ()
sN = return ()

sO :: Parser ()
sO = do
  _ <- many (char ' ' <|> char '\n' <|> char '\t')
  return ()

sR :: Parser ()
sR = do
  _ <- some (char ' ' <|> char '\n' <|> char '\t')
  return ()
