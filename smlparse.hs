module SMLParse (program) where

import Control.Applicative ((<$>), (<|>), optional)
import Control.Monad (liftM)
import Data.Attoparsec.ByteString.Char8
import Data.List (intercalate)


class ToOcaml a where
  toOcaml :: a -> String

-- -----------------------------------------------------------------------------
-- Programs and Modules

newtype Program = Program [TopLevelDeclaration]
  deriving Show

instance ToOcaml Program where
  toOcaml (Program decls) = concat $ map ((++ ";;") . toOcaml) decls

program :: Parser Program
program = liftM Program $
  many1 (topLevelDeclaration >>= \ d -> char ';' >> skipSpace >> return d)


data TopLevelDeclaration = Exp Expression | ObjDecl ObjectDeclaration
  deriving Show

instance ToOcaml TopLevelDeclaration where
  toOcaml (Exp e) = toOcaml e

topLevelDeclaration :: Parser TopLevelDeclaration
topLevelDeclaration =
  liftM Exp expression <|>
  liftM ObjDecl objectDeclaration


data ObjectDeclaration = Decl Declaration
  deriving Show

objectDeclaration :: Parser ObjectDeclaration
objectDeclaration = liftM Decl declaration


-- -----------------------------------------------------------------------------
-- Declarations

data Declaration = ValDecl [VariableDeclaration]
  deriving Show

declaration :: Parser Declaration
declaration = liftM ValDecl valDeclarations


newtype VariableDeclaration = VariableDeclaration (IsRec, Pattern, Expression)
  deriving Show

newtype IsRec = IsRec Bool deriving Show

valDeclaration :: Parser VariableDeclaration
valDeclaration =
  option (IsRec False) (string "rec" >> sp >> return (IsRec True)) >>= \ rec ->
  pattrn >>= \ pat -> sp >> char '=' >> sp >>
  expression >>= \ e -> sp >>
  return (VariableDeclaration (rec, pat, e))

valDeclarations :: Parser [VariableDeclaration]
valDeclarations =
  string "val" >> sp >> valDeclaration >>= \ d ->
  many' (string "and" >> sp >> valDeclaration) >>= \ ds ->
  return $ d : ds


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
  liftM ConstExp constant <|>
  liftM TupleExp    tuple <|>
  liftM  ListExp     list
  where
    tuple = char '(' >> expList >>= \ e -> char ')' >> return e
    list  = char '[' >> expList >>= \ e -> char ']' >> return e

expList :: Parser [Expression]
expList = expression >>= \ e ->
  many' (char ',' >> expression) >>= \ es ->
  return $ e : es


-- -----------------------------------------------------------------------------
-- Matches and Patterns

data Pattern = AtomicPat AtomicPattern
  deriving Show

pattrn :: Parser Pattern
pattrn = liftM AtomicPat atomicPattern

data AtomicPattern = AnyPattern
  deriving Show

atomicPattern :: Parser AtomicPattern
atomicPattern = char '_' >> sp >> return AnyPattern


-- -----------------------------------------------------------------------------
-- Lexical Matters: Identifiers, Constants, Comments

data Constant =
    NumConst (Numeral, Maybe Digits, Maybe Numeral)
  | StrConst String
  deriving Show

type Digits = String

instance ToOcaml Constant where
  toOcaml (NumConst (num, digits, e)) =
    toOcaml num ++ maybe "" ("." ++) digits ++ maybe "" ("E" ++) (toOcaml <$> e)
  toOcaml (StrConst s) = show s

constant :: Parser Constant
constant = liftM NumConst numConst <|> liftM StrConst strConst where
  numConst =
    numeral >>= \ n ->
    optional (char '.' >> many1 digit) >>= \ d ->
    optional (char 'E' >> numeral) >>= \ e ->
    return (n, d, e)

  strConst =
    char '"' >>
    many' (satisfy (\ c -> c /= '\\' && c /= '"')) >>= \ s ->
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
numeralSign = option PosSign (char '~' >> skipSpace >> return NegSign)


typeVar :: Parser String
typeVar =
  many1 (char '\'') >>= \ a ->
  option "" (char '_' >> return "_") >>= \ u ->
  alphanumericIdent >>= \ i ->
  return $ a ++ u ++ i

-- TODO: Clarify ' in syntax!
ident :: Parser String
ident = many1 (satisfy $ inClass "!%&$#+-/:<=>?@\\~'^|*") <|> alphanumericIdent

alphanumericIdent :: Parser String
alphanumericIdent = letter >>= \ l ->
  many' (letter <|> digit <|> char '_' <|> char '\'') >>= \ ls ->
  return $ l : ls

letter :: Parser Char
letter = letter_ascii

sp :: Parser String
sp = many1 space
