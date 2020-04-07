module Language.CPlusPlus.Internal.Types.Lexer where

import           Language.CPlusPlus.Internal.Types.Common

data Token
  = Identifier InputT

  | KWAlignas
  | KWAlignof
  | KWAsm
  | KWAuto
  | KWBool
  | KWBreak
  | KWCase
  | KWCatch
  | KWChar
  | KWChar16T
  | KWChar32T
  | KWClass
  | KWConst
  | KWConstexpr
  | KWConstCast
  | KWContinue
  | KWDecltype
  | KWDefault
  | KWDelete
  | KWDo
  | KWDouble
  | KWDynamicCast
  | KWElse
  | KWEnum
  | KWExplicit
  | KWExport
  | KWExtern
  | KWFalse
  | KWFloat
  | KWFor
  | KWFriend
  | KWGoto
  | KWIf
  | KWInline
  | KWInt
  | KWLong
  | KWMutable
  | KWNamespace
  | KWNew
  | KWNoexcept
  | KWNullptr
  | KWOperator
  | KWPrivate
  | KWProtected
  | KWPublic
  | KWRegister
  | KWReinterpretCast
  | KWReturn
  | KWShort
  | KWSigned
  | KWSizeof
  | KWStatic
  | KWStaticAssert
  | KWStaticCast
  | KWStruct
  | KWSwitch
  | KWTemplate
  | KWThis
  | KWThreadLocal
  | KWThrow
  | KWTrue
  | KWTry
  | KWTypedef
  | KWTypeid
  | KWTypename
  | KWUnion
  | KWUnsigned
  | KWUsing
  | KWVirtual
  | KWVoid
  | KWVolatile
  | KWWcharT
  | KWWhile

  | LiteralToken Literal

  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Hash
  | Hash2
  | LeftParen
  | RightParen
  | Semicolon
  | Colon
  | Dots
  | QuestionMark
  | Colons
  | Dot
  | OpGetPtr
  | OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpRem
  | OpBitXor
  | OpRef
  | OpBitOr
  | Tilda
  | OpNot
  | OpSet
  | OpLess
  | OpGreater
  | OpSetPlus
  | OpSetMinus
  | OpSetMultiply
  | OpSetDivide
  | OpSetRem
  | OpSetBitXor
  | OpSetBitAnd
  | OpSetBitOr
  | OpLeftShift
  | OpRightShift
  | OpSetLeftShift
  | OpSetRightShift
  | OpEq
  | OpNotEq
  | OpLessEq
  | OpGreaterEq
  | OpAnd
  | OpOr
  | OpIncrement
  | OpDecrement
  | Comma
  | OpPtrGetPtr
  | OpPtrGet

  | PPHHeaderName InputT
  | PPQHeaderName InputT
  deriving (Show)

data Literal
  = IntegerLiteral InputT
  | CharacterLiteral InputT
  | FloatingLiteral InputT
  | StringLiteral InputT
  | BooleanLiteral InputT
  | PointerLiteral InputT
  | UserDefinedLiteral InputT
  deriving (Show)

