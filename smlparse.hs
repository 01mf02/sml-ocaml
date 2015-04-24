module SMLParse (program) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.List (intercalate)
import Text.Parsec
import qualified Text.Parsec.Token as T

import Language (smlDef)


class ToOcaml a where
  toOcaml :: a -> String

binariesSepBy :: (ToOcaml l, ToOcaml r) => String -> String -> [(l, r)] -> String
binariesSepBy sep op =
  unwords . intercalate [sep] . map (\ (x, y) -> [toOcaml x, op, toOcaml y])


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

data Declaration = ValDecls [ValDeclaration]
  deriving Show

instance ToOcaml Declaration where
  toOcaml (ValDecls ds) = "let " ++ intercalate " and " (map toOcaml ds)

declaration :: Parser Declaration
declaration = liftM ValDecls valDeclarations

valDeclarations :: Parser [ValDeclaration]
valDeclarations = reserved "val" >> valDeclaration `sepBy1` reserved "and"

data ValDeclaration = ValDeclaration IsRec Pattern Expression
  deriving Show

instance ToOcaml ValDeclaration where
  toOcaml (ValDeclaration (IsRec r) p e) =
    unwords $ (if r then ["rec"] else []) ++ [toOcaml p, "=", toOcaml e]

newtype IsRec = IsRec Bool deriving Show

valDeclaration :: Parser ValDeclaration
valDeclaration =
  (IsRec <$> option False (reserved "rec" >> return True)) >>= \ r ->
  pattrn >>= \ p -> reservedOp "=" >> expression >>= return . ValDeclaration r p



-- -----------------------------------------------------------------------------
-- Expressions

data Expression =
    InfixExp InfixExpression
  | ExpType Expression Type
  | BoolExp Expression BoolOp Expression
  | HandleExp Expression Match
  | RaiseExp Expression
  | IfThenElseExp Expression Expression Expression
  | WhileDoExp Expression Expression
  | CaseExp Expression Match
  | FnExp Match
  deriving Show

data BoolOp = AndAlso | OrElse deriving Show

instance ToOcaml BoolOp where
  toOcaml AndAlso = "&&"
  toOcaml OrElse  = "||"

instance ToOcaml Expression where
  toOcaml (InfixExp e) = toOcaml e
  toOcaml (ExpType e t) = unwords [toOcaml e, ":", toOcaml t]
  toOcaml (BoolExp e1 op e2) = unwords [toOcaml e1, toOcaml op, toOcaml e2]
  toOcaml (HandleExp e m) = unwords ["try", toOcaml e, "with", toOcaml m]
  toOcaml (RaiseExp e) = unwords ["raise", toOcaml e]
  toOcaml (IfThenElseExp i t e) =
    unwords ["if", toOcaml i, "then", toOcaml t, "else", toOcaml e]
  toOcaml (WhileDoExp w d) = unwords ["while", toOcaml w, "do", toOcaml d, "done"]
  toOcaml (CaseExp e m) = unwords ["match", toOcaml e, "with", toOcaml m]
  toOcaml (FnExp m) = unwords ["function", toOcaml m]

expression :: Parser Expression
expression = detExpression >>= expression' where
  detExpression =
        liftM InfixExp infixExpression
    <|> raiseExpression
    <|> ifThenElseExpression
    <|> whileDoExpression
    <|> caseExpression
    <|> fnExpression

  raiseExpression =
    reserved "raise" >> expression >>= return . RaiseExp

  ifThenElseExpression =
    reserved "if" >> expression >>= \ i ->
    reserved "then" >> expression >>= \ t ->
    reserved "else" >> expression >>= return . IfThenElseExp i t

  whileDoExpression =
    reserved "while" >> expression >>= \ w -> 
    reserved "do" >> expression >>= return . WhileDoExp w

  caseExpression =
    reserved "case" >> expression >>= \ e ->
    reserved "of" >> match >>= return . CaseExp e

  fnExpression =
    reserved "fn" >> match >>= return . FnExp


expression' :: Expression -> Parser Expression
expression' e = (choice opts >>= expression') <|> return e where
  opts =
    [ reservedOp ":" >> typ >>= \ t -> return $ ExpType e t
    , reserved "andalso" >> expression >>= return . BoolExp e AndAlso
    , reserved "orelse"  >> expression >>= return . BoolExp e OrElse
    , reserved "handle"  >> match >>= return . HandleExp e
    ]


data InfixExpression = AtomicExps [AtomicExpression]
  deriving Show

instance ToOcaml InfixExpression where
  toOcaml (AtomicExps es) = intercalate " " $ map toOcaml es

infixExpression :: Parser InfixExpression
infixExpression = liftM AtomicExps (many1 atomicExpression)


data AtomicExpression =
     ConstExp Constant
  |  TupleExp [Expression]
  |   ListExp [Expression]
  | RecordExp [(Label, Expression)]
  deriving Show


instance ToOcaml AtomicExpression where
  toOcaml (ConstExp c) = toOcaml c
  toOcaml (TupleExp es) = "(" ++ intercalate ", " (map toOcaml es) ++ ")"
  toOcaml (ListExp  es) = "[" ++ intercalate "; " (map toOcaml es) ++ "]"
  toOcaml (RecordExp le) = "{" ++ binariesSepBy "," "=" le ++ "}"

atomicExpression :: Parser AtomicExpression
atomicExpression =
      liftM  ConstExp constant
  <|> liftM  TupleExp (parens   $ commaSep expression)
  <|> liftM   ListExp (brackets $ commaSep expression)
  <|> liftM RecordExp (braces   $ commaSep labelExpression)
  <?> "atomic expression"

  where
    labelExpression =
      labl >>= \ l -> reservedOp "=" >> expression >>= \ e -> return (l, e)


-- -----------------------------------------------------------------------------
-- Matches and Patterns

newtype Match = Match [(Pattern, Expression)] deriving Show

instance ToOcaml Match where
  toOcaml (Match m) = binariesSepBy "|" "->" m

match :: Parser Match
match = pe `sepBy1` reservedOp "|" >>= return . Match where
  pe = pattrn >>= \ p -> reservedOp "=>" >> expression >>= \ e -> return (p, e)


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
-- Types

data Type = TypeVar String | RecordTy [(Label, Type)]
  deriving Show

instance ToOcaml Type where
  toOcaml (TypeVar t) = t
  toOcaml (RecordTy lt) = "{" ++ binariesSepBy ";" ":" lt ++ "}"

typ :: Parser Type
typ =
      liftM TypeVar typeVar
  <|> parens typ
  <|> liftM RecordTy (braces $ commaSep1 labelType)

  where labelType = labl >>= \ l -> reservedOp ":" >> typ >>= \ t -> return (l, t)


-- -----------------------------------------------------------------------------
-- Lexical Matters: Identifiers, Constants, Comments

data Constant =
    NumConst NumericConstant
  | StrConst String
  deriving Show

type NumericConstant = (Integer, Maybe Digits, Maybe Integer)

type Digits = String

instance ToOcaml Constant where
  toOcaml (NumConst (num, digits, e)) =
    show num ++ maybe "" ("." ++) digits ++ maybe "" ("E" ++) (show <$> e)
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

numeral :: Parser Integer
numeral =
  option id (reservedOp "~" >> return negate) >>= \ sign ->
  decimal >>= return . sign


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

data Label = IdentLbl String | DigitsLbl Integer deriving Show

instance ToOcaml Label where
  toOcaml (IdentLbl s) = s
  toOcaml (DigitsLbl i) = show i

labl :: Parser Label
labl =  liftM IdentLbl ident <|> liftM DigitsLbl decimal

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

decimal :: Parser Integer
decimal     = T.decimal lexer

semi :: Parser String
semi        = T.semi lexer

reserved, reservedOp :: String -> Parser ()
reserved    = T.reserved lexer
reservedOp  = T.reservedOp lexer

commaSep, commaSep1, semiSep1 :: Parser a -> Parser [a]
commaSep    = T.commaSep  lexer
commaSep1   = T.commaSep1 lexer
semiSep1    = T.semiSep1  lexer


