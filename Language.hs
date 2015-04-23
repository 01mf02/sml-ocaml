module Language where

import Text.Parsec
import Text.Parsec.Token

mlStyle :: LanguageDef st
mlStyle = LanguageDef
  { commentStart    = "(*"
  , commentEnd      = "*)"
  , commentLine     = ""
  , nestedComments  = True
  , identStart      = letter
  , identLetter	    = alphaNum <|> oneOf "_'"
  , opStart	    = opLetter mlStyle
  , opLetter	    = oneOf "!%&$#+-/:<=>?@\\~^|*'"
  , reservedOpNames = []
  , reservedNames   = []
  , caseSensitive   = True
  }

smlDef :: LanguageDef st
smlDef = mlStyle
  { reservedNames   = [ "and", "andalso", "case", "do", "else", "if", "of", "orelse", "rec", "then", "val", "while" ]
  , reservedOpNames = [ "=", "_", "~", ":", "=>", "|" ]
  }
