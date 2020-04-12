module Language.CPlusPlus.Internal.Types.Lexer where

import           Text.Parsec 

type InputT = String

-- (line, column)
type Pos = (Int, Int)

data L a = L Pos a
  deriving (Show, Eq)


type P = Parsec [CppToken] ()
type CppToken = L Token

data Token
    = KW_Alignas
    | KW_Alignof
    | KW_Asm
    | KW_Auto
    | KW_Bool
    | KW_Break
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Char16T
    | KW_Char32T
    | KW_Class
    | KW_Const
    | KW_Constexpr
    | KW_ConstCast
    | KW_Continue
    | KW_Decltype
    | KW_Default
    | KW_Delete
    | KW_Do
    | KW_Double
    | KW_DynamicCast
    | KW_Else
    | KW_Enum
    | KW_Explicit
    | KW_Export
    | KW_Extern
    | KW_False
    | KW_Final
    | KW_Float
    | KW_For
    | KW_Friend
    | KW_Goto
    | KW_If
    | KW_Inline
    | KW_Int
    | KW_Long
    | KW_Mutable
    | KW_Namespace
    | KW_New
    | KW_Noexcept
    | KW_Nullptr
    | KW_Operator
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Register
    | KW_ReinterpretCast
    | KW_Return
    | KW_Short
    | KW_Signed
    | KW_Sizeof
    | KW_Static
    | KW_StaticAssert
    | KW_StaticCast
    | KW_Struct
    | KW_Switch
    | KW_Template
    | KW_This
    | KW_ThreadLocal
    | KW_Throw
    | KW_True
    | KW_Try
    | KW_Typedef
    | KW_Typeid
    | KW_Typename
    | KW_Union
    | KW_Unsigned
    | KW_Using
    | KW_Virtual
    | KW_Void
    | KW_Volatile
    | KW_WCharT
    | KW_While

    | Punc_LeftBrace
    | Punc_RightBrace
    | Punc_LeftBracket
    | Punc_RightBracket
    | Punc_Hash
    | Punc_DoubleHash
    | Punc_LeftParen
    | Punc_RightParen
    | Punc_Semi
    | Punc_Colon
    | Punc_ThreeDot
    | Punc_QuestionMark
    | Punc_DoubleColon
    | Punc_Dot
    | Punc_Comma
    | Op_DotPtr
    | Op_Plus
    | Op_Minus
    | Op_Mul
    | Op_Div
    | Op_Rem
    | Op_Xor
    | Op_And
    | Op_Or
    | Op_Tilda
    | Op_Not
    | Op_Assign
    | Op_Less
    | Op_Greater
    | Op_AssignPlus
    | Op_AssignMinus
    | Op_AssignMul
    | Op_AssignDiv
    | Op_AssignRem
    | Op_AssignXor
    | Op_AssignAnd
    | Op_AssignOr
    | Op_LeftShift
    | Op_RightShift
    | Op_AssignLeftShift
    | Op_AssignRightShift
    | Op_Eq
    | Op_NotEq
    | Op_LessEq
    | Op_GreaterEq
    | Op_LogicalAnd
    | Op_LogicalOr
    | Op_Increment
    | Op_Decrement
    | Op_ArrowPtr
    | Op_Arrow

{-    | Op_LiterAnd
    | Op_LiterAndEq
    | Op_LiterBitAnd
    | Op_LiterBitOr
    | Op_LiterCompl
    | Op_LiterNot
    | Op_LiterNotEq
    | Op_LiterOr
    | Op_LiterOrEq
    | Op_LiterXor
    | Op_LiterXorEq -}

    | Literal_Integer InputT
    | Literal_Char InputT
    | Literal_String InputT
    | Literal_Float InputT
    | Literal_Boolean InputT
    | Literal_NullPtr InputT
    | Literal_UserDefined InputT
    | Id InputT
    | PP_If
    | PP_Ifdef
    | PP_Ifndef
    | PP_Elif
    | PP_Else
    | PP_Endif
    | PP_Include
    | PP_Define
    | PP_Undef
    | PP_Line
    | PP_Error
    | PP_Pragma
    | TComment InputT
    | TLineComment InputT
    | EOL
    deriving (Show, Eq)
