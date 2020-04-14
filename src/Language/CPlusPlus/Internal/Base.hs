module Language.CPlusPlus.Internal.Base where

import           Language.CPlusPlus.Internal.Lexer
                                         hiding ( pos )

import           Language.CPlusPlus.Internal.Types.Lexer

import           Text.Parsec

import           Control.Applicative            ( (<$>) )

data BToken =
  BToken
    { _bTokenPos :: SourcePos
    , _bTokenValue :: Token
    }
  deriving (Show, Eq)

data Literal =
  Literal
    { _literalPos   :: SourcePos
    , _literalValue :: InputT
    }
  deriving (Show, Eq)

data Identifier =
  Identifier
    { _identifierPos   :: SourcePos
    , _identifierValue :: InputT
    }
  deriving (Show, Eq)

literal :: P Literal
literal = Literal <$> getPosition <*> choice
  [ integerLiteral
  , characterLiteral
  , floatingLiteral
  , stringLiteral
  , booleanLiteral
  , pointerLiteral
  , userDefinedLiteral
  ]

stringLiteral' :: P Literal
stringLiteral' = Literal <$> getPosition <*> stringLiteral <?> "string literal"

identifier :: P Identifier
identifier = Identifier <$> getPosition <*> ident

integerLiteral :: P InputT
integerLiteral = intLiteral

characterLiteral :: P InputT
characterLiteral = charLiteral

floatingLiteral :: P InputT
floatingLiteral = flLiteral

stringLiteral :: P InputT
stringLiteral = strLiteral

booleanLiteral :: P InputT
booleanLiteral = boolLiteral

pointerLiteral :: P InputT
pointerLiteral = ptrLiteral

userDefinedLiteral :: P InputT
userDefinedLiteral = uDefLiteral

bToken :: P BToken
bToken = BToken <$> pos <*> bcppToken

lexemeFinal = lexeme "final"
lexemeOverride = lexeme "override"

parens :: P a -> P a
parens = between leftParen rightParen

braces :: P a -> P a
braces = between leftBrace rightBrace

brackets :: P a -> P a
brackets = between leftBracket rightBracket

angles :: P a -> P a
angles = between opLess opGreater

-- | Tries apply parser 'p'. Return True if it succeed.
optionBool :: P a -> P Bool
optionBool p = option False (p >> pure True)

optionEither :: P a -> P b -> P (Either a b)
optionEither pa pb = try (Left <$> pa) <|> (Right <$> pb)

pos :: P SourcePos
pos = getPosition
