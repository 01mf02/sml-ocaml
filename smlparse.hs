module SMLParse (program) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.List (intercalate)
import Text.Parsec
import qualified Text.Parsec.Token as T

import Language (smlDef)


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
program = liftM Program $ topLevelDeclaration `endBy1` semi


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
valDeclarations = reserved "val" >> valDeclaration `sepBy1` reserved "and"

newtype VariableDeclaration = VariableDeclaration (IsRec, Pattern, Expression)
  deriving Show

instance ToOcaml VariableDeclaration where
  toOcaml (VariableDeclaration (IsRec r, p, e)) =
    unwords $ (if r then ["rec"] else []) ++ [toOcaml p, "=", toOcaml e]

newtype IsRec = IsRec Bool deriving Show

valDeclaration :: Parser VariableDeclaration
valDeclaration =
  (IsRec <$> option False (reserved "rec" >> return True)) >>= \ rec ->
  pattrn >>= \ pat ->
  reservedOp "=" >>
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
  <|> liftM TupleExp (parens   $ commaSep expression)
  <|> liftM  ListExp (brackets $ commaSep expression)
  <?> "atomic expression"


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
atomicPattern = reservedOp "_" >> return AnyPattern


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
  <|> liftM StrConst (T.stringLiteral lexer)
  <?> "constant"


numericConstant :: Parser NumericConstant
numericConstant = lexeme $
  numeral >>= \ n ->
  optionMaybe (char '.' >> many1 digit) >>= \ d ->
  optionMaybe (char 'E' >> numeral) >>= \ e ->
  return (n, d, e)

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
numeralSign = option PosSign (reservedOp "~" >> return NegSign)


typeVar :: Parser String
typeVar = lexeme $
  many1 (char '\'') >>= \ a ->
  option "" (char '_' >> return "_") >>= \ u ->
  alphanumericIdent >>= \ i ->
  return $ a ++ u ++ i

ident :: Parser String
ident =
      T.operator lexer
  <|> alphanumericIdent
  <?> "identifier"

alphanumericIdent :: Parser String
alphanumericIdent = T.identifier lexer


-- -----------------------------------------------------------------------------
-- Parsec Lexer

lexer :: T.TokenParser st
lexer = T.makeTokenParser smlDef

parens, braces, brackets :: Parser a -> Parser a
parens      = T.parens lexer
braces      = T.braces lexer
brackets    = T.brackets lexer

lexeme :: Parser a -> Parser a
lexeme      = T.lexeme lexer

semi :: Parser String
semi        = T.semi lexer

reserved, reservedOp :: String -> Parser ()
reserved    = T.reserved lexer
reservedOp  = T.reservedOp lexer

commaSep, semiSep1 :: Parser a -> Parser [a]
commaSep    = T.commaSep lexer
semiSep1    = T.semiSep1 lexer


