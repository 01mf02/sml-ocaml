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
  { reservedNames   = [ "and", "andalso", "do", "else", "if", "orelse", "rec", "then", "val", "while" ]
  , reservedOpNames = [ "=", "_", "~", ":" ]
  }
