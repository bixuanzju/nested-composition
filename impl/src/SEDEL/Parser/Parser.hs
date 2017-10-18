module SEDEL.Parser.Parser (parseExpr) where

import           Control.Arrow (first, second)
import           Control.Monad (void, liftM3)
import           Data.List (foldl', foldl1')
import           Data.Maybe (fromMaybe)
import           Data.Scientific (toRealFloat)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String
import           Unbound.Generics.LocallyNameless

import           SEDEL.Common
import           SEDEL.Source.Syntax
import           SEDEL.Util

parseExpr :: String -> Either String Module
parseExpr s =
  case runParser (whole prog) "" s of
    Left err -> Left $ parseErrorPretty err
    Right e -> Right e

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

------------------------------------------------------------------------
-- Programs
------------------------------------------------------------------------

-- prog :: Parser Module
-- prog =
--    -- hack for repl
--   try (expr >>= \e -> return $ Module [] (DefDecl (TmBind "main" [] [] e Nothing))) <|> prog'

prog :: Parser Module
prog = do
  decls <- sepEndBy decl semi
  m <- optional mainDecl
  let d = fromMaybe (DefDecl (TmBind "main" [] [] Top Nothing)) m
  return $ Module decls d

mainDecl :: Parser SDecl
mainDecl = do
  rword "main"
  symbol "="
  e <- expr
  return $ DefDecl (TmBind "main" [] [] e Nothing)


decl :: Parser SDecl
decl = sedel <|> traitDecl

traitDecl :: Parser SDecl
traitDecl = do
  (tr, n) <- pTrait
  case n of
    Just n' -> return $ DefDecl (TmBind n' [] [] tr Nothing)
    Nothing -> fail "Need trait name"

sedel :: Parser SDecl
sedel = DefDecl <$> tmBind <|> TypeDecl <$> tyBind

tmBind :: Parser TmBind
tmBind = do
  n <- lidentifier
  ts <- many ctyparam
  xs <- many param
  ret <- optional (colon *> pType)
  symbol "="
  e <- expr
  return $ TmBind n (map (first s2n) ts) (map (first s2n) xs) e ret

tmBind2 :: Parser TmBind
tmBind2 = do
  m <- lidentifier
  symbol "@"
  (n, ts, xs) <- parens $ liftM3 (,,) lidentifier (many ctyparam) (many param)
  tts <- many ctyparam
  xxs <- many param
  ret <- optional (colon *> pType)
  symbol "="
  e <- expr
  return $
    TmBind
      n
      (map (first s2n) ts)
      (map (first s2n) xs)
      (DRec' (TmBind m (map (first s2n) tts) (map (first s2n) xxs) e ret))
      Nothing


tyBind :: Parser TypeBind
tyBind = do
  rword "type"
  n <- uidentifier
  ts <- optional typaramList
  symbol "="
  t <- pType
  return $ TypeBind n (fromMaybe [] ts) t

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: Parser Expr
expr = makeExprParser term pOperators

term :: Parser Expr
term = postfixChain factor (try fapp <|> bapp)

fapp :: Parser (Expr -> Expr)
fapp = do
  e <- factor
  return (`App` e)

bapp :: Parser (Expr -> Expr)
bapp = do
  e <- pType
  return (`TApp` e)


factor :: Parser Expr
factor = postfixChain atom (rmOperator <|> dotOperator <|> colonOperator)

dotOperator :: Parser (Expr -> Expr)
dotOperator = do
  symbol "."
  k <- lidentifier
  return (`Acc` k)

colonOperator :: Parser (Expr -> Expr)
colonOperator = do
  colon
  t <- pType
  return (`Anno` t)


rmOperator :: Parser (Expr -> Expr)
rmOperator = do
  symbol "\\"
  (l, t) <-
    braces
      (do l <- lidentifier
          colon
          t <- pType
          return (l, t))
  return (\e -> Remove e l t)

atom :: Parser Expr
atom =
  choice
    [ pLambda
    , pBLambda
    , pLet
    , pIf
    , fst <$> pTrait
    , pNew
    , LitV <$> float
    , StrV <$> stringLiteral
    , evar <$> lidentifier
    , record
    , bconst
    , parens expr
    ]

record :: Parser Expr
record = braces (mkRecds' <$> sepBy1 tmBind comma)

bconst :: Parser Expr
bconst =
  choice
    [ rword "true" *> pure (BoolV True)
    , rword "false" *> pure (BoolV False)
    ]

pLambda :: Parser Expr
pLambda = do
  symbol "\\"
  xs <- some (lidentifier <|> symbol "_")
  symbol "->"
  e <- expr
  return $ foldr elam (elam (last xs) e) (init xs)

pBLambda :: Parser Expr
pBLambda = do
  symbol "/\\"
  xs <- some pCtyparam
  symbol "."
  e <- expr
  return $ foldr dlam (dlam (last xs) e) (init xs)


pTrait :: Parser (Expr, Maybe String)
pTrait = do
  rword "trait"
  n <- optional lidentifier
  ts <- many ctyparam
  xs <- fromMaybe [] <$> optional (parens (sepBy1 tparam comma))
  ret <- optional (colon *> pType)
  i <- optional inherits
  (self, sty, body) <- braces (pTraitBody <|> return ("self", TopT, []))
  return
    ( AnonyTrait
        (TraitDef
           (self, sty)
           (fromMaybe [] i)
           ret
           (map (first s2n) ts)
           (map (first s2n) xs)
           body)
    , n)

pTraitBody :: Parser (String, Type, [TmBind])
pTraitBody = do
  n <- lidentifier
  ret <- optional (colon *> pType)
  symbol "=>"
  decls <- sepBy1 (try tmBind <|> tmBind2) semi
  return (n, fromMaybe TopT ret, decls)

pNew :: Parser Expr
pNew = do
  rword "new"
  t <- brackets pType
  cs <- pTraitConstructs
  return $ transNew t cs

inherits :: Parser [Expr]
inherits = do
  rword "inherits"
  pTraitConstructs

pTraitConstructs :: Parser [Expr]
pTraitConstructs = sepBy1 pTraitConstruct (symbol "&")

pTraitConstruct :: Parser Expr
pTraitConstruct = do
  n <- lidentifier
  ts <- fromMaybe [] <$> optional tyList
  xs <- fromMaybe [] <$> optional pArgs
  rm <- optional rmOperator
  let res = App (foldl' App (foldl' TApp (evar n) ts) xs) (evar "self")
  case rm of
    Nothing -> return res
    Just f -> return (f res)

pLet :: Parser Expr
pLet = do
  rword "let"
  n <- lidentifier
  colon
  t <- pType
  symbol "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ elet n t e1 e2

pIf :: Parser Expr
pIf = do
  rword "if"
  a <- expr
  rword "then"
  b <- expr
  rword "else"
  c <- expr
  return $ If a b c


pOperators :: [[Operator Parser Expr]]
pOperators =
  [ [ InfixL (PrimOp (Arith Mul) <$ symbol "*")
    , InfixL (PrimOp (Arith Div) <$ symbol "/")
    ]
  , [ InfixL (PrimOp Append <$ symbol "++")
    , InfixL (PrimOp (Arith Add) <$ symbol "+")
    , InfixL (PrimOp (Arith Sub) <$ symbol "-")
    ]
  , [ InfixN (PrimOp (Comp Lt) <$ symbol "<")
    , InfixN (PrimOp (Comp Gt) <$ symbol ">")
    ]
  , [ InfixN (PrimOp (Comp Equ) <$ symbol "==")
    , InfixN (PrimOp (Comp Neq) <$ symbol "/=")
    ]
  , [InfixL (PrimOp (Logical LAnd) <$ symbol "&&")]
  , [InfixL (PrimOp (Logical LOr) <$ symbol "||")]
  , [InfixL (Merge <$ symbol ",,")]
  ]


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

pType :: Parser Type
pType = makeExprParser atype tOperators

tOperators :: [[Operator Parser Type]]
tOperators =
  [ [ Postfix
        (tyList >>= \xs ->
           return $ \f -> foldl' OpApp (OpApp f (head xs)) (tail xs))
    ]
  , [InfixL (And <$ symbol "&")]
  , [InfixR (Arr <$ symbol "->")]
  ]

atype :: Parser Type
atype =
  choice
    [pForall, traitType, tvar <$> uidentifier, recordType, tconst, parens pType]

pForall :: Parser Type
pForall = do
  rword "forall"
  xs <- some pCtyparam
  symbol "."
  t <- pType
  return $ foldr tforall (tforall (last xs) t) (init xs)

traitType :: Parser Type
traitType = do
  rword "Trait"
  ts <- tyList
  if length ts == 1
    then return $ Arr TopT (head ts)
    else return $ foldl1' Arr ts

recordType :: Parser Type
recordType = braces (mkRecdsT <$> sepBy1 tparam comma)

tconst :: Parser Type
tconst =
  choice
    [ rword "Int" *> pure NumT
    , rword "String" *> pure StringT
    , rword "Bool" *> pure BoolT
    , rword "T" *> pure TopT
    ]


------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------


-- [A,B,C]
tyList :: Parser [Type]
tyList = brackets $ sepBy1 pType comma

-- [X, Y, Z]
typaramList :: Parser [(TyName, Kind)]
typaramList =
  brackets $ sepBy1 (uidentifier >>= \n -> return (s2n n, Star)) comma


-- (a, b, c)
pArgs :: Parser [Expr]
pArgs = parens $ sepBy1 expr comma

-- x : A
tparam :: Parser (Label, Type)
tparam = do
  l <- lidentifier <|> symbol "_"
  colon
  e <- pType
  return (l, e)

-- (x : A) or x
param :: Parser (String, Maybe Type)
param =
  choice
    [ (lidentifier <|> symbol "_") >>= \n -> return (n, Nothing)
    , parens $ tparam >>= pure . second Just
    ]


-- x * A
constrainTy :: Parser (String, Type)
constrainTy = do
  n <- uidentifier
  symbol "*"
  t <- pType
  return (n, t)

-- (x * A) or X
pCtyparam :: Parser (String, Type)
pCtyparam =
  choice
    [ do n <- uidentifier
         return (n, TopT)
    , parens constrainTy
    ]

-- [x * A] or X
ctyparam :: Parser (String, Type)
ctyparam =
  choice
    [ do n <- uidentifier
         return (n, TopT)
    , brackets constrainTy
    ]

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Double
float = lexeme $ toRealFloat <$> L.number

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

semi :: Parser String
semi = symbol ";"

colon :: Parser String
colon = symbol ":"

comma :: Parser String
comma = symbol ","

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x =
      (do f <- op
          rest $ f x) <|>
      return x

rws :: [String] -- list of reserved words
rws =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "type"
  , "defrec"
  , "forall"
  , "trait"
  , "new"
  , "Trait"
  , "main"
  , "inherits"
  , "undefined"
  , "Int"
  , "String"
  , "Bool"
  , "true"
  , "false"
  , "T"
  ]


identifier :: Parser Char -> Parser String
identifier s = (lexeme . try) (p >>= check)
  where
    p = (:) <$> s <*> many identChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "_#'"

lidentifier :: Parser String
lidentifier = identifier lowerChar

uidentifier :: Parser String
uidentifier = identifier upperChar
