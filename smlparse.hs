module SMLParse (program) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.List (intercalate)
import Text.Parsec


class ToOcaml a where
  toOcaml :: a -> String

type Parser = Parsec String ()

-- -----------------------------------------------------------------------------
-- Programs and Modules

newtype Program = Program [TopLevelDeclaration]
  deriving Show

instance ToOcaml Program where
  toOcaml (Program decls) = concat $ map ((++ ";;") . toOcaml) decls

program :: Parser Program
program = liftM Program $ topLevelDeclaration `endBy1` (char ';' >> spaces)


data TopLevelDeclaration = Exp Expression | ObjDecl ObjectDeclaration
  deriving Show

instance ToOcaml TopLevelDeclaration where
  toOcaml (Exp e) = toOcaml e
  toOcaml (ObjDecl d) = toOcaml d

topLevelDeclaration :: Parser TopLevelDeclaration
topLevelDeclaration =
      liftM Exp expression
  <|> liftM ObjDecl objectDeclaration
  <?> "top level declaration"


data ObjectDeclaration = Decl Declaration
  deriving Show

instance ToOcaml ObjectDeclaration where
  toOcaml (Decl d) = toOcaml d

objectDeclaration :: Parser ObjectDeclaration
objectDeclaration = liftM Decl declaration


-- -----------------------------------------------------------------------------
-- Declarations

data Declaration = ValDecls [VariableDeclaration]
  deriving Show

instance ToOcaml Declaration where
  toOcaml (ValDecls ds) = "let " ++ intercalate " and " (map toOcaml ds)

declaration :: Parser Declaration
declaration = liftM ValDecls valDeclarations

valDeclarations :: Parser [VariableDeclaration]
valDeclarations =
  string "val" >> (sp1 >> dropSpacesAfter valDeclaration) `sepBy1` string "and"

newtype VariableDeclaration = VariableDeclaration (IsRec, Pattern, Expression)
  deriving Show

instance ToOcaml VariableDeclaration where
  toOcaml (VariableDeclaration (IsRec r, p, e)) =
    unwords $ (if r then ["rec"] else []) ++ [toOcaml p, "=", toOcaml e]

newtype IsRec = IsRec Bool deriving Show

valDeclaration :: Parser VariableDeclaration
valDeclaration =
  option (IsRec False) (string "rec" >> sp1 >> return (IsRec True)) >>= \ rec ->
  pattrn >>= \ pat -> sp1 >>
  char '=' >> sp1 >>
  expression >>= \ e ->
  return (VariableDeclaration (rec, pat, e))



-- -----------------------------------------------------------------------------
-- Expressions

data Expression = InfixExp InfixExpression
  deriving Show

instance ToOcaml Expression where
  toOcaml (InfixExp e) = toOcaml e

expression :: Parser Expression
expression = liftM InfixExp infixExpression


data InfixExpression = AtomicExps [AtomicExpression]
  deriving Show

instance ToOcaml InfixExpression where
  toOcaml (AtomicExps es) = intercalate " " $ map toOcaml es

infixExpression :: Parser InfixExpression
infixExpression = liftM AtomicExps (many1 atomicExpression)


data AtomicExpression =
    ConstExp Constant
  | TupleExp [Expression]
  |  ListExp [Expression]
  deriving Show

instance ToOcaml AtomicExpression where
  toOcaml (ConstExp c) = toOcaml c
  toOcaml (TupleExp es) = "(" ++ intercalate "," (map toOcaml es) ++ ")"
  toOcaml (ListExp  es) = "(" ++ intercalate ";" (map toOcaml es) ++ ")"

atomicExpression :: Parser AtomicExpression
atomicExpression =
      liftM ConstExp constant
  <|> liftM TupleExp (expList `encloseBy` ('(', ')'))
  <|> liftM  ListExp (expList `encloseBy` ('[', ']'))
  <?> "atomic expression"

expList :: Parser [Expression]
expList = expression `sepBy1` (spaces >> char ',' >> spaces)


-- -----------------------------------------------------------------------------
-- Matches and Patterns

data Pattern = AtomicPat AtomicPattern
  deriving Show

instance ToOcaml Pattern where
  toOcaml (AtomicPat p) = toOcaml p

pattrn :: Parser Pattern
pattrn = liftM AtomicPat atomicPattern

data AtomicPattern = AnyPattern
  deriving Show

instance ToOcaml AtomicPattern where
  toOcaml AnyPattern = "_"

atomicPattern :: Parser AtomicPattern
atomicPattern = char '_' >> return AnyPattern


-- -----------------------------------------------------------------------------
-- Lexical Matters: Identifiers, Constants, Comments

data Constant =
    NumConst NumericConstant
  | StrConst String
  deriving Show

type NumericConstant = (Numeral, Maybe Digits, Maybe Numeral)

type Digits = String

instance ToOcaml Constant where
  toOcaml (NumConst (num, digits, e)) =
    toOcaml num ++ maybe "" ("." ++) digits ++ maybe "" ("E" ++) (toOcaml <$> e)
  toOcaml (StrConst s) = show s

constant :: Parser Constant
constant =
      liftM NumConst numericConstant
  <|> liftM StrConst stringConstant
  <?> "constant"


numericConstant :: Parser NumericConstant
numericConstant =
  numeral >>= \ n ->
  optionMaybe (char '.' >> many1 digit) >>= \ d ->
  optionMaybe (char 'E' >> numeral) >>= \ e ->
  return (n, d, e)

stringConstant :: Parser String
stringConstant =
  char '"' >>
  many (noneOf ['\\', '"']) >>= \ s ->
  char '"' >>
  return s


newtype Numeral = Numeral (NumeralSign, String) deriving Show

instance ToOcaml Numeral where
  toOcaml (Numeral (sign, num)) = toOcaml sign ++ num

numeral :: Parser Numeral
numeral = numeralSign >>= \ s -> many1 digit >>= \ d -> return $ Numeral (s, d)


data NumeralSign = PosSign | NegSign deriving Show

instance ToOcaml NumeralSign where
  toOcaml PosSign = ""
  toOcaml NegSign = "-"

numeralSign :: Parser NumeralSign
numeralSign = option PosSign (char '~' >> spaces >> return NegSign)


typeVar :: Parser String
typeVar =
  many1 (char '\'') >>= \ a ->
  option "" (char '_' >> return "_") >>= \ u ->
  alphanumericIdent >>= \ i ->
  return $ a ++ u ++ i

-- TODO: Clarify ' in syntax!
ident :: Parser String
ident =
      many1 (oneOf "!%&$#+-/:<=>?@\\~'^|*")
  <|> alphanumericIdent
  <?> "identifier"

alphanumericIdent :: Parser String
alphanumericIdent = letter >>= \ l ->
  many (letter <|> digit <|> char '_' <|> char '\'') >>= \ ls ->
  return $ l : ls


-- -----------------------------------------------------------------------------
-- Parser combinators

sp1 :: Parser String
sp1 = many1 space

dropSpacesAfter :: Parser a -> Parser a
dropSpacesAfter p = p >>= \ x -> spaces >> return x

encloseBy :: Parser a -> (Char, Char) -> Parser a
p `encloseBy` (s, e) = char s >> p >>= \ x -> char e >> return x
