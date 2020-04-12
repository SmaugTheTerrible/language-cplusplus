module Language.CPlusPlus.Internal.Base where

import qualified Language.CPlusPlus.Internal.Lexer
                                               as Lexer

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
identifier = Identifier <$> getPosition <*> Lexer.ident

integerLiteral :: P InputT
integerLiteral = Lexer.integerLiteral

characterLiteral :: P InputT
characterLiteral = Lexer.characterLiteral

floatingLiteral :: P InputT
floatingLiteral = Lexer.floatingLiteral

stringLiteral :: P InputT
stringLiteral = Lexer.stringLiteral

booleanLiteral :: P InputT
booleanLiteral = Lexer.booleanLiteral

pointerLiteral :: P InputT
pointerLiteral = Lexer.pointerLiteral

userDefinedLiteral :: P InputT
userDefinedLiteral = Lexer.userDefinedLiteral

bToken :: P BToken
bToken = BToken <$> pos <*> Lexer.bcppToken

parens :: P a -> P a
parens = between Lexer.leftParen Lexer.rightParen

braces :: P a -> P a
braces = between Lexer.leftBrace Lexer.rightBrace

brackets :: P a -> P a
brackets = between Lexer.leftBracket Lexer.rightBracket

angles :: P a -> P a
angles = between Lexer.opLess Lexer.opGreater

-- | Tries apply parser 'p'. Return True if it succeed.
optionBool :: P a -> P Bool
optionBool p = option False (p >> pure True)

optionEither :: P a -> P b -> P (Either a b)
optionEither pa pb = try (Left <$> pa) <|> (Right <$> pb)

pos :: P SourcePos
pos = getPosition
