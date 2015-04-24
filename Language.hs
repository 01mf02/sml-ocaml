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
  { reservedNames   = [ "and", "andalso", "case", "do", "else", "end", "fn", "fun", "function", "handle", "if", "in", "let", "of", "op", "orelse", "raise", "rec", "then", "try", "val", "while", "with" ]
  , reservedOpNames = [ "=", "_", "~", ":", "=>", "|" ]
  }
