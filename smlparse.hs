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


data TopLevelDeclaration =
    Exp Expression
  | ObjDecl ObjectDeclaration
  | FctDecl FunctorDeclaration
  deriving Show

instance ToOcaml TopLevelDeclaration where
  toOcaml (Exp e) = toOcaml e
  toOcaml (ObjDecl d) = toOcaml d

topLevelDeclaration :: Parser TopLevelDeclaration
topLevelDeclaration =
      liftM Exp expression
  <|> liftM ObjDecl objectDeclaration
  <|> liftM FctDecl functorDeclaration
  <?> "top level declaration"


data ObjectDeclaration = Decl Declaration
  deriving Show

instance ToOcaml ObjectDeclaration where
  toOcaml (Decl d) = toOcaml d

objectDeclaration :: Parser ObjectDeclaration
objectDeclaration = liftM Decl declaration


newtype FunctorDeclaration = FunctorDeclaration [FunctorBinding] deriving Show

functorDeclaration :: Parser FunctorDeclaration
functorDeclaration = liftM FunctorDeclaration
  (reserved "functor" >> functorBinding `sepBy1` reserved "and")

data FunctorBinding = FctBinding String FunctorArguments (Maybe Signature) Structure
  deriving Show

functorBinding :: Parser FunctorBinding
functorBinding =
  ident >>= \ i ->
  parens functorArguments >>= \ args ->
  optionMaybe (reservedOp ":" >> signature) >>= \ sig ->
  reservedOp "=" >>
  structure >>= \ struct ->
  return $ FctBinding i args sig struct

data FunctorArguments = SpecArgs Specification | IdSigArgs String Signature
  deriving Show

functorArguments :: Parser FunctorArguments
functorArguments =
      liftM SpecArgs specification
  <|> (ident >>= \ i -> reservedOp ":" >> signature >>= return . IdSigArgs i)
  <?> "functor arguments"

data Signature = SpecSig Specification | IdentSig String deriving Show

signature :: Parser Signature
signature =
      (reserved "sig" >> specification >>= \ s -> reserved "end" >> return (SpecSig s))
  <|> liftM IdentSig ident
  <?> "signature"

data Structure = ObjDeclStruct ObjectDeclaration
  deriving Show

structure :: Parser Structure
structure =
  reserved "struct" >> objectDeclaration >>= \ o -> reserved "end" >> return (ObjDeclStruct o)

data ValSpec = ValSpec String Type deriving Show
data Specification = ValSpecs [ValSpec]
  deriving Show

specification :: Parser Specification
specification =
      liftM ValSpecs (reserved "val" >> valSpec `sepBy1` reserved "and")
  <?> "specification"
  where
    valSpec = ident >>= \ i -> reservedOp ":" >> typ >>= return . ValSpec i

-- -----------------------------------------------------------------------------
-- Declarations

data Declaration =
    ValDecls [ValDeclaration]
  | FunDecls [FunDeclaration]
  deriving Show

instance ToOcaml Declaration where
  toOcaml (ValDecls ds) = "let " ++ intercalate " and " (map toOcaml ds)
  toOcaml (FunDecls ds) = "let " ++ intercalate " and " (map toOcaml ds)

declaration :: Parser Declaration
declaration =
      liftM ValDecls valDeclarations
  <|> liftM FunDecls funDeclarations
  <?> "declaration"

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


funDeclarations :: Parser [FunDeclaration]
funDeclarations = reserved "fun" >> funDeclaration `sepBy1` reserved "and"

newtype FunDeclaration = FunDeclaration [FunDeclarationCase] deriving Show

instance ToOcaml FunDeclaration where
  toOcaml (FunDeclaration cs) = intercalate "|" $ map toOcaml cs

funDeclaration :: Parser FunDeclaration
funDeclaration = liftM FunDeclaration $ funDeclarationCase `sepBy1` reservedOp "|"

data FunDeclarationCase = FunDeclarationCase FunHeading (Maybe Type) Expression
  deriving Show

instance ToOcaml FunDeclarationCase where
  toOcaml (FunDeclarationCase h m e) = unwords $
    toOcaml h : maybe [] (\ t -> [":", toOcaml t]) m ++ ["=", toOcaml e]

funDeclarationCase :: Parser FunDeclarationCase
funDeclarationCase =
  funHeading >>= \ h ->
  optionMaybe (reservedOp ":" >> typ) >>= \ t ->
  reservedOp "=" >>
  expression >>= return . FunDeclarationCase h t

instance ToOcaml FunHeading where
  toOcaml (NameFunHd n ps) = unwords $ toOcaml n : map toOcaml ps

data FunHeading = NameFunHd Name [AtomicPattern]
  deriving Show

funHeading :: Parser FunHeading
funHeading = name >>= \ n -> many1 atomicPattern >>= return . NameFunHd n


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
    <?> "expression"

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
    CoNameExp CompoundName
  |  ConstExp Constant
  |  TupleExp [Expression]
  |   ListExp [Expression]
  | RecordExp [(Label, Expression)]
  |    LetExp Declaration [Expression]
  deriving Show


instance ToOcaml AtomicExpression where
  toOcaml (CoNameExp n) = toOcaml n
  toOcaml ( ConstExp c) = toOcaml c
  toOcaml ( TupleExp es) = "(" ++ intercalate ", " (map toOcaml es) ++ ")"
  toOcaml (  ListExp es) = "[" ++ intercalate "; " (map toOcaml es) ++ "]"
  toOcaml (RecordExp le) = "{" ++ binariesSepBy "," "=" le ++ "}"
  toOcaml (LetExp d e) = unwords [toOcaml d, "in", intercalate "; " (map toOcaml e)]

atomicExpression :: Parser AtomicExpression
atomicExpression =
      liftM CoNameExp compoundName
  <|> liftM  ConstExp constant
  <|> liftM  TupleExp (parens   $ commaSep expression)
  <|> liftM   ListExp (brackets $ commaSep expression)
  <|> liftM RecordExp (braces   $ commaSep labelExpression)
  <|> letExpression
  <?> "atomic expression"

  where
    labelExpression =
      labl >>= \ l -> reservedOp "=" >> expression >>= \ e -> return (l, e)

    letExpression =
      reserved "let" >> declaration >>= \ d ->
      reserved "in"  >> semiSep1 expression >>= \ e ->
      reserved "end" >> return (LetExp d e)


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

data AtomicPattern = AnyPat | CompoundPat CompoundName | ConstantPat Constant
  deriving Show

instance ToOcaml AtomicPattern where
  toOcaml AnyPat = "_"
  toOcaml (CompoundPat c) = toOcaml c
  toOcaml (ConstantPat c) = toOcaml c

atomicPattern :: Parser AtomicPattern
atomicPattern =
      (reservedOp "_" >> return AnyPat)
  <|> liftM CompoundPat compoundName
  <|> liftM ConstantPat constant
  <?> "atomic pattern"


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
  <?> "type"

  where labelType = labl >>= \ l -> reservedOp ":" >> typ >>= \ t -> return (l, t)


-- -----------------------------------------------------------------------------
-- Lexical Matters: Identifiers, Constants, Comments

compoundIdent :: Parser [String]
compoundIdent = ident `sepBy1` char '.'


data CompoundName = CoIdentNm [String] | CoOpNm String deriving Show

instance ToOcaml CompoundName where
  toOcaml (CoIdentNm i) = intercalate "." i
  toOcaml (CoOpNm o) = o

compoundName :: Parser CompoundName
compoundName =
      liftM CoIdentNm compoundIdent
  <|> liftM CoOpNm (reserved "op" >> infixOperator)
  <?> "compound name"
  

data Name = IdentNm String | OpNm String deriving Show

instance ToOcaml Name where
  toOcaml (IdentNm i) = i
  toOcaml (OpNm o) = o

name :: Parser Name
name =
      liftM IdentNm ident
  <|> liftM OpNm (reserved "op" >> infixOperator)
  <?> "name"

infixOperator :: Parser String
infixOperator = ident


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
semiSep1    = T.semiSep1 lexer

