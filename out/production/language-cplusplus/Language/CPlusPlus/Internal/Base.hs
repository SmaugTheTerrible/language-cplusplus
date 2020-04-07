module Language.CPlusPlus.Internal.Base where

import           Language.CPlusPlus.Internal.Types.Common
import           Language.CPlusPlus.Internal.Types.Lexer

import           Text.Parsec                              hiding (digit)

import           Control.Applicative                      ((<$>))

-- BNF Grammar Rules

-- lex.charset
-- hex-quad:
--  	hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit
-- universal-character-name:
--  	\u hex-quad
--  	\U hex-quad hex-quad
hexQuad :: P InputT
hexQuad = count 4 hexadecimalDigit

universalCharacterName :: P InputT
universalCharacterName = do
  prefix <- try $ string "\\u" <|> string "\\U"
  quad <- hexQuad
  return $ prefix ++ quad

sourceSetChar :: P Char
sourceSetChar = oneOf "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_!@#$%^&*()-=_+{};':\"\\|,./<>?`~"

-- lex.pptoken
-- preprocessing-token:
--  	header-name
--  	identifier
--  	pp-number
--  	character-literal
--  	user-defined-character-literal     C++0x
--  	string-literal
--  	user-defined-string-literal     C++0x
--  	preprocessing-op-or-punc
--  	each non-white-space character that cannot be one of the above
preprocessingToken :: P Token
preprocessingToken = choice
  [ headerName
  , identifier
  , ppNumber
  , characterLiteral
  , stringLiteral
  , userDefinedLiteral
  , preprocessingOpOrPunc
  ]

-- lex.token
-- token:
--  	identifier
--  	keyword
--  	literal
--  	operator-token See C++ Standard Core Language Issue n. 189
--  	punctuator See C++ Standard Core Language Issue n. 189
token :: P Token
token = choice
  [ identifier
  , keyword'
  , literal
  , preprocessingOpOrPunc
  ]

-- lex.header
-- header-name:
--  	< h-char-sequence >
--  	" q-char-sequence "
-- h-char-sequence:
--  	h-char
--  	h-char-sequence h-char
-- h-char:
--  	any member of the source character set except new-line and >
-- q-char-sequence:
--  	q-char
--  	q-char-sequence q-char
-- q-char:
--  	any member of the source character set except new-line and "
headerName :: P Token
headerName =
  hHeader <|> qHeader
  where
    hHeader = PPHHeaderName <$> angles hCharSequence
    qHeader = PPQHeaderName <$> between (char '"') (char '"') qCharSequence

hCharSequence :: P InputT
hCharSequence = many1 hChar

hChar :: P Char
hChar = anyChar

qCharSequence :: P InputT
qCharSequence = many1 qChar

qChar :: P Char
qChar = anyChar

-- lex.ppnumber
-- pp-number:
--  	digit
--  	. digit
--  	pp-number digit
--  	pp-number identifier-nondigit
--  	pp-number e sign
--  	pp-number E sign
--  	pp-number .
ppNumber :: P Token
ppNumber = literal

-- lex.name
-- identifier:
--  	identifier-nondigit     C++0x
--  	identifier identifier-nondigit     C++0x
--  	identifier digit
-- identifier-nondigit:
--  	nondigit     C++0x
--  	universal-character-name     C++0x
--  	other implementation-defined characters     C++0x
-- nondigit:
--  	universal-character-name     Removed in C++0x
--  	a
--  	b
--  	c
--  	d
--  	e
--  	f
--  	g
--  	h
--  	i
--  	j
--  	k
--  	l
--  	m
--  	n
--  	o
--  	p
--  	q
--  	r
--  	s
--  	t
--  	u
--  	v
--  	w
--  	x
--  	y
--  	z
--  	A
--  	B
--  	C
--  	D
--  	E
--  	F
--  	G
--  	H
--  	I
--  	J
--  	K
--  	L
--  	M
--  	N
--  	O
--  	P
--  	Q
--  	R
--  	S
--  	T
--  	U
--  	V
--  	W
--  	X
--  	Y
--  	Z
--  	_     (underscore)
-- digit:
--  	0
--  	1
--  	2
--  	3
--  	4
--  	5
--  	6
--  	7
--  	8
--  	9
identifier :: P Token
identifier = do
  start <- identifierNondigit
  rest <- many $ nondigit <|> digit
  return $ Identifier $ start ++ rest

identifierNondigit :: P InputT
identifierNondigit =
  (do
    c <- nondigit
    return [c]) <|>
  universalCharacterName

nondigit :: P Char
nondigit = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

digit :: P Char
digit = oneOf "0123456789"

-- lex.key
-- keyword:
--  	alignas     C++0x
--  	alignof     C++0x
--  	asm
--  	auto
--  	bool
--  	break
--  	case
--  	catch
--  	char
--  	char16_t     C++0x
--  	char32_t     C++0x
--  	class
--  	const
--  	constexpr     C++0x
--  	const_cast
--  	continue
--  	decltype     C++0x
--  	default
--  	delete
--  	do
--  	double
--  	dynamic_cast
--  	else
--  	enum
--  	explicit
--  	export     C++0x - Reserved for future use
--  	extern
--  	false
--  	float
--  	for
--  	friend
--  	goto
--  	if
--  	inline
--  	int
--  	long
--  	mutable
--  	namespace
--  	new
--  	noexcept     C++0x
--  	nullptr     C++0x
--  	operator
--  	private
--  	protected
--  	public
--  	register
--  	reinterpret_cast
--  	return
--  	short
--  	signed
--  	sizeof
--  	static
--  	static_assert     C++0x
--  	static_cast
--  	struct
--  	switch
--  	template
--  	this
--  	thread_local     C++0x
--  	throw
--  	true
--  	try
--  	typedef
--  	typeid
--  	typename
--  	union
--  	unsigned
--  	using
--  	virtual
--  	void
--  	volatile
--  	wchar_t
--  	while
keyword' :: P Token
keyword' = choice
  [ kwAlignas
  , kwAlignof
  , kwAsm
  , kwAuto
  , kwBool
  , kwBreak
  , kwCase
  , kwCatch
  , kwChar
  , kwChar16_t
  , kwChar32_t
  , kwClass
  , kwConst
  , kwConstexpr
  , kwConstCast
  , kwContinue
  , kwDecltype
  , kwDefault
  , kwDelete
  , kwDo
  , kwDouble
  , kwDynamicCast
  , kwElse
  , kwEnum
  , kwExplicit
  , kwExport
  , kwExtern
  , kwFalse
  , kwFloat
  , kwFor
  , kwFriend
  , kwGoto
  , kwIf
  , kwInline
  , kwInt
  , kwLong
  , kwMutable
  , kwNamespace
  , kwNew
  , kwNoexcept
  , kwNullptr
  , kwOperator
  , kwPrivate
  , kwProtected
  , kwPublic
  , kwRegister
  , kwReinterpretCast
  , kwReturn
  , kwShort
  , kwSigned
  , kwSizeof
  , kwStatic
  , kwStaticAssert
  , kwStaticCast
  , kwStruct
  , kwSwitch
  , kwTemplate
  , kwThis
  , kwThreadLocal
  , kwThrow
  , kwTrue
  , kwTry
  , kwTypedef
  , kwTypeid
  , kwTypename
  , kwUnion
  , kwUnsigned
  , kwUsing
  , kwVirtual
  , kwVoid
  , kwVolatile
  , kwWcharT
  , kwWhile
  ]

kwAlignas :: P Token
kwAlignas = keyword "alighnas" >>  return KWAlignas

kwAlignof :: P Token
kwAlignof = keyword "alignof" >>  return KWAlignof

kwAsm :: P Token
kwAsm = keyword "asm" >>  return KWAsm

kwAuto :: P Token
kwAuto = keyword "auto" >>  return KWAuto

kwBool :: P Token
kwBool = keyword "bool" >>  return KWBool

kwBreak :: P Token
kwBreak = keyword "break" >>  return KWBreak

kwCase :: P Token
kwCase = keyword "case" >>  return KWCase

kwCatch :: P Token
kwCatch = keyword "catch" >>  return KWCatch

kwChar :: P Token
kwChar = keyword "char" >>  return KWChar

kwChar16_t :: P Token
kwChar16_t = keyword "char16_t" >>  return KWChar16T

kwChar32_t :: P Token
kwChar32_t = keyword "char32_t" >>  return KWChar32T

kwClass :: P Token
kwClass = keyword "class" >>  return KWClass

kwConst :: P Token
kwConst = keyword "const" >>  return KWConst

kwConstexpr :: P Token
kwConstexpr = keyword "constexpr" >>  return KWConstexpr

kwConstCast :: P Token
kwConstCast = keyword "const_cast" >>  return KWConstCast

kwContinue :: P Token
kwContinue = keyword "continue" >>  return KWContinue

kwDecltype :: P Token
kwDecltype = keyword "decltype" >>  return KWDecltype

kwDefault :: P Token
kwDefault = keyword "default" >>  return KWDefault

kwDelete :: P Token
kwDelete = keyword "delete" >>  return KWDelete

kwDo :: P Token
kwDo = keyword "do" >>  return KWDo

kwDouble :: P Token
kwDouble = keyword "double" >>  return KWDouble

kwDynamicCast :: P Token
kwDynamicCast = keyword "dynamic_cast" >>  return KWDynamicCast

kwElse :: P Token
kwElse = keyword "else" >>  return KWElse

kwEnum :: P Token
kwEnum = keyword "enum" >>  return KWEnum

kwExplicit :: P Token
kwExplicit = keyword "explicit" >>  return KWExplicit

kwExport :: P Token
kwExport = keyword "export" >>  return KWExport

kwExtern :: P Token
kwExtern = keyword "extern" >>  return KWExtern

kwFalse :: P Token
kwFalse = keyword "false" >>  return KWFalse

kwFloat :: P Token
kwFloat = keyword "float" >>  return KWFloat

kwFor :: P Token
kwFor = keyword "for" >>  return KWFor

kwFriend :: P Token
kwFriend = keyword "friend" >>  return KWFriend

kwGoto :: P Token
kwGoto = keyword "goto" >>  return KWGoto

kwIf :: P Token
kwIf = keyword "if" >>  return KWIf

kwInline :: P Token
kwInline = keyword "inline" >>  return KWInline

kwInt :: P Token
kwInt = keyword "int" >>  return KWInt

kwLong :: P Token
kwLong = keyword "long" >>  return KWLong

kwMutable :: P Token
kwMutable = keyword "mutable" >>  return KWMutable

kwNamespace :: P Token
kwNamespace = keyword "namespace" >>  return KWNamespace

kwNew :: P Token
kwNew = keyword "new" >>  return KWNew

kwNoexcept :: P Token
kwNoexcept = keyword "noexcept" >>  return KWNoexcept

kwNullptr :: P Token
kwNullptr = keyword "nullptr" >>  return KWNullptr

kwOperator :: P Token
kwOperator = keyword "operator" >>  return KWOperator

kwPrivate :: P Token
kwPrivate = keyword "private" >>  return KWPrivate

kwProtected :: P Token
kwProtected = keyword "protected" >>  return KWProtected

kwPublic :: P Token
kwPublic = keyword "public" >>  return KWPublic

kwRegister :: P Token
kwRegister = keyword "register" >>  return KWRegister

kwReinterpretCast :: P Token
kwReinterpretCast = keyword "reinterpret_cast"  >>  return KWReinterpretCast

kwReturn :: P Token
kwReturn = keyword "return" >> return KWReturn

kwShort :: P Token
kwShort = keyword "short" >> return KWShort

kwSigned :: P Token
kwSigned = keyword "signed" >> return KWSigned

kwSizeof :: P Token
kwSizeof = keyword "sizeof" >> return KWSizeof

kwStatic :: P Token
kwStatic = keyword "static" >> return KWStatic

kwStaticAssert :: P Token
kwStaticAssert = keyword "static_assert" >> return KWStaticAssert

kwStaticCast :: P Token
kwStaticCast = keyword "static_cast" >> return KWStaticCast

kwStruct :: P Token
kwStruct = keyword "struct" >> return KWStruct

kwSwitch :: P Token
kwSwitch = keyword "switch" >> return KWSwitch

kwTemplate :: P Token
kwTemplate = keyword "template" >> return KWTemplate

kwThis :: P Token
kwThis = keyword "this" >> return KWThis

kwThreadLocal :: P Token
kwThreadLocal = keyword "thread_local" >> return KWThreadLocal

kwThrow :: P Token
kwThrow = keyword "throw" >> return KWThrow

kwTrue :: P Token
kwTrue = keyword "true" >> return KWTrue

kwTry :: P Token
kwTry = keyword "try" >> return KWTry

kwTypedef :: P Token
kwTypedef = keyword "typedef" >> return KWTypedef

kwTypeid :: P Token
kwTypeid = keyword "typeid" >> return KWTypeid

kwTypename :: P Token
kwTypename = keyword "typename" >> return KWTypename

kwUnion :: P Token
kwUnion = keyword "union" >> return KWUnion

kwUnsigned :: P Token
kwUnsigned = keyword "unsigned" >> return KWUnsigned

kwUsing :: P Token
kwUsing = keyword "using" >> return KWUsing

kwVirtual :: P Token
kwVirtual = keyword "virtual" >> return KWVirtual

kwVoid :: P Token
kwVoid = keyword "void" >> return KWVoid

kwVolatile :: P Token
kwVolatile = keyword "volatile" >> return KWVolatile

kwWcharT :: P Token
kwWcharT = keyword "wchar_t" >> return KWWcharT

kwWhile :: P Token
kwWhile = keyword "while" >> return KWWhile

-- lex.operators
-- operator-token:
--  	Look at preprocessing-op-or-punc below See C++ Standard Core Language Issue n. 189
-- punctuator:
--  	Look at preprocessing-op-or-punc below See C++ Standard Core Language Issue n. 189
-- preprocessing-op-or-punc:
--  	{
--  	}
--  	[
--  	]
--  	#
--  	##
--  	(
--  	)
--  	<:
--  	:>
--  	<%
--  	%>
--  	%:
--  	%:%:
--  	;
--  	:
--  	...
--  	new
--  	delete
--  	?
--  	::
--  	.
--  	.*
--  	+
--  	-
--  	*
--  	/
--  	%
--  	^
--  	&
--  	|
--  	~
--  	!
--  	=
--  	<
--  	>
--  	+=
--  	-=
--  	*=
--  	/=
--  	%=
--  	^=
--  	&=
--  	|=
--  	<<
--  	>>
--  	<<=
--  	>>=
--  	==
--  	!=
--  	<=
--  	>=
--  	&&
--  	||
--  	++
--  	--
--  	,
--  	->*
--  	->
--  	and
--  	and_eq
--  	bitand
--  	bitor
--  	compl
--  	not
--  	not_eq
--  	or
--  	or_eq
--  	xor
--  	xor_eq
preprocessingOpOrPunc :: P Token
preprocessingOpOrPunc = choice
  [ leftBrace
  , rightBrace
  , leftBracket
  , rightBracket
  , hash
  , hash2
  , leftParen
  , rightParen
  , semicolon
  , colon
  , dots
  , questionMark
  , colons
  , dot
  , opGet
  , opGetPtr
  , opPlus
  , opMinus
  , opMultiply
  , opDivide
  , opRem
  , opBitXor
  , opRef
  , opBitAnd
  , opBitOr
  , tilda
  , opNot
  , opSet
  , opLess
  , opGreater
  , opSetPlus
  , opSetMinus
  , opSetMultiply
  , opSetDivide
  , opSetRem
  , opSetBitXor
  , opSetBitAnd
  , opSetBitOr
  , opLeftShift
  , opRightShift
  , opSetLeftShift
  , opSetRightShift
  , opEq
  , opNotEq
  , opLessEq
  , opGreaterEq
  , opAnd
  , opOr
  , opIncrement
  , opDecrement
  , comma
  , opPtrGetPtr
  , opPtrGet
  ]

leftBrace :: P Token
leftBrace = string "{" >> return LeftBrace

rightBrace :: P Token
rightBrace = string "}" >> return RightBrace

leftBracket :: P Token
leftBracket = string "[" >> return LeftBracket

rightBracket :: P Token
rightBracket = string "]" >> return RightBracket

hash :: P Token
hash = string "#" >> return Hash

hash2 :: P Token
hash2 = string "##" >> return Hash2

leftParen :: P Token
leftParen = string "(" >> return LeftParen

rightParen :: P Token
rightParen = string ")" >> return RightParen

-- TODO parse triplets
-- "<:"
-- ":>"
-- "<%"
-- "%>"
-- "%:"
-- "%:%:"
semicolon :: P Token
semicolon = string ";" >> return Semicolon

colon :: P Token
colon = string ":" >> return Colon

dots :: P Token
dots = string "..." >> return Dots

-- "new"
-- "delete"
questionMark :: P Token
questionMark = string "?" >> return QuestionMark

colons :: P Token
colons = string "::" >> return Colons

dot :: P Token
dot = string "." >> return Dot

opGet :: P Token
opGet = dot

opGetPtr :: P Token
opGetPtr = string ".*" >> return OpGetPtr

opPlus :: P Token
opPlus = string "+" >> return OpPlus

opMinus :: P Token
opMinus = string "-" >> return OpMinus

opMultiply :: P Token
opMultiply = string "*" >> return OpMultiply

opDivide :: P Token
opDivide = string "/" >> return OpDivide

opRem :: P Token
opRem = string "%" >> return OpRem

opBitXor :: P Token
opBitXor = string "^" >> return OpBitXor

opRef :: P Token
opRef = string "&" >> return OpRef

opBitAnd :: P Token
opBitAnd = opRef

opBitOr :: P Token
opBitOr = string "|" >> return OpBitOr

tilda :: P Token
tilda = string "~" >> return Tilda

opNot :: P Token
opNot = string "!" >> return OpNot

opSet :: P Token
opSet = string "=" >> return OpSet

opLess :: P Token
opLess = string "<" >> return OpLess

opGreater :: P Token
opGreater = string ">" >> return OpGreater

opSetPlus :: P Token
opSetPlus = string "+=" >> return OpSetPlus

opSetMinus :: P Token
opSetMinus = string "-=" >> return OpSetMinus

opSetMultiply :: P Token
opSetMultiply = string "*=" >> return OpSetMultiply

opSetDivide :: P Token
opSetDivide = string "/=" >> return OpSetDivide

opSetRem :: P Token
opSetRem = string "%=" >> return OpSetRem

opSetBitXor :: P Token
opSetBitXor = string "^=" >> return OpSetBitXor

opSetBitAnd :: P Token
opSetBitAnd = string "&=" >> return OpSetBitAnd

opSetBitOr :: P Token
opSetBitOr = string "|=" >> return OpSetBitOr

opLeftShift :: P Token
opLeftShift = string "<<" >> return OpLeftShift

opRightShift :: P Token
opRightShift = string ">>" >> return OpRightShift

opSetLeftShift :: P Token
opSetLeftShift = string "<<=" >> return OpSetLeftShift

opSetRightShift :: P Token
opSetRightShift = string ">>=" >> return OpSetRightShift

opEq :: P Token
opEq = string "==" >> return OpEq

opNotEq :: P Token
opNotEq = string "!=" >> return OpNotEq

opLessEq :: P Token
opLessEq = string "<=" >> return OpLessEq

opGreaterEq :: P Token
opGreaterEq = string ">=" >> return OpGreaterEq

opAnd :: P Token
opAnd = string "&&" >> return OpAnd

opOr :: P Token
opOr = string "||" >> return OpOr

opIncrement :: P Token
opIncrement = string "++" >> return OpIncrement

opDecrement :: P Token
opDecrement = string "--" >> return OpDecrement

comma :: P Token
comma = string "," >> return Comma

opPtrGetPtr :: P Token
opPtrGetPtr = string "->*" >> return OpPtrGetPtr

opPtrGet :: P Token
opPtrGet = string "->" >> return OpPtrGet
--TODO parse literal operators
-- = string "and"
-- = string "and_eq"
-- = string "bitand"
-- = string "bitor"
-- = string "compl"
-- = string "not"
-- = string "not_eq"
-- = string "or"
-- = string "or_eq"
-- = string "xor"
-- = string "xor_eq"

-- lex.literal.kinds
-- literal:
--  	integer-literal
--  	character-literal
--  	floating-literal
--  	string-literal
--  	boolean-literal
--  	pointer-literal     C++0x
--  	user-defined-literal     C++0x
literal :: P Token
literal = choice
  [ integerLiteral
  , characterLiteral
  , floatingLiteral
  , stringLiteral
  , booleanLiteral
  , pointerLiteral
  , userDefinedLiteral
  ]

-- lex.icon
-- integer-literal:
--  	decimal-literal integer-suffix[opt]
--  	octal-literal integer-suffix[opt]
--  	hexadecimal-literal integer-suffix[opt]
-- decimal-literal:
--  	nonzero-digit
--  	decimal-literal digit
-- octal-literal:
--  	0
--  	octal-literal octal-digit
-- hexadecimal-literal:
--  	0x hexadecimal-digit
--  	0X hexadecimal-digit
--  	hexadecimal-literal hexadecimal-digit
-- nonzero-digit:
--  	1
--  	2
--  	3
--  	4
--  	5
--  	6
--  	7
--  	8
--  	9
-- octal-digit:
--  	0
--  	1
--  	2
--  	3
--  	4
--  	5
--  	6
--  	7
-- hexadecimal-digit:
--  	0
--  	1
--  	2
--  	3
--  	4
--  	5
--  	6
--  	7
--  	8
--  	9
--  	a
--  	b
--  	c
--  	d
--  	e
--  	f
--  	A
--  	B
--  	C
--  	D
--  	E
--  	F
-- integer-suffix:
--  	unsigned-suffix long-suffix[opt]
--  	unsigned-suffix long-long-suffix[opt]     C++0x
--  	long-suffix unsigned-suffix[opt]
--  	long-long-suffix unsigned-suffix[opt]     C++0x
-- unsigned-suffix:
--  	u
--  	U
-- long-suffix:
--  	l
--  	L
-- long-long-suffix:
--  	ll     C++0x
--  	LL     C++0x
integerLiteral :: P Token
integerLiteral = do
  lit <- try hexadecimalLiteral <|> try octalLiteral <|> decimalLiteral
  suffix <- integerSuffix
  return $ LiteralToken $ IntegerLiteral $ lit ++ suffix

decimalLiteral :: P InputT
decimalLiteral = do
  start <- nonzeroDigit
  rest <- many digit
  return $ start : rest

octalLiteral :: P InputT
octalLiteral = do
  prefix <- string "0"
  rest <- many octalDigit
  return $ prefix ++ rest

hexadecimalLiteral :: P InputT
hexadecimalLiteral = do
  prefix <- try (string "0x") <|> string "0X"
  body <- many1 hexadecimalDigit
  return $ prefix ++ body

nonzeroDigit :: P Char
nonzeroDigit = oneOf "123456789"

octalDigit :: P Char
octalDigit = oneOf "01234567"

hexadecimalDigit :: P Char
hexadecimalDigit = oneOf "0123456789abcdefABCDEF"

integerSuffix :: P InputT
integerSuffix =
  ul <|> lu
  where
    ul = do
      u <- unsignedSuffix
      l <- option [] $ try longLongSuffix <|> longSuffix
      return $ u ++ l
    lu = do
      l <- try longLongSuffix <|> longSuffix
      u <- option [] unsignedSuffix
      return $ l ++ u

unsignedSuffix :: P InputT
unsignedSuffix = string "u" <|> string "U"

longSuffix :: P InputT
longSuffix = string "l" <|> string "L"

longLongSuffix :: P InputT
longLongSuffix = string "ll" <|> string "LL"

-- lex.ccon
-- character-literal:
--  	' c-char-sequence '
--  	u ' c-char-sequence '     C++0x
--  	U ' c-char-sequence '     C++0x
--  	L ' c-char-sequence '
-- c-char-sequence:
--  	c-char
--  	c-char-sequence c-char
-- c-char:
--  	any member of the source character set except the single quote ', backslash \, or new-line character
--  	escape-sequence
--  	universal-character-name
-- escape-sequence:
--  	simple-escape-sequence
--  	octal-escape-sequence
--  	hexadecimal-escape-sequence
-- simple-escape-sequence:
--  	\'
--  	\"
--  	\?
--  	\\
--  	\a
--  	\b
--  	\f
--  	\n
--  	\r
--  	\t
--  	\v
-- octal-escape-sequence:
--  	\ octal-digit
--  	\ octal-digit octal-digit
--  	\ octal-digit octal-digit octal-digit
-- hexadecimal-escape-sequence:
--  	\x hexadecimal-digit
--  	hexadecimal-escape-sequence hexadecimal-digit

characterLiteral :: P Token
characterLiteral = undefined

cCharSequence :: P InputT
cCharSequence = do
  seq <- many1 cChar
  return $ concat seq

cChar :: P InputT
cChar = try escapeSequence <|> universalCharacterName
  <|> (do
  ch <- sourceSetChar
  return [ch])

escapeSequence :: P InputT
escapeSequence = simpleEscapeSequence <|> octalEscapeSequence <|> hexadecimalEscapeSequence

simpleEscapeSequence :: P InputT
simpleEscapeSequence = choice
  [ string "\\'"
  , string "\\\""
  , string "\\?"
  , string "\\\\"
  , string "\\a"
  , string "\\b"
  , string "\\f"
  , string "\\n"
  , string "\\r"
  , string "\\t"
  , string "\\v"
  ]

octalEscapeSequence :: P InputT
octalEscapeSequence = do
  prefix <- string "\\"
  first <- octalDigit
  s <- optionMaybe octalDigit
  case s of
    Nothing -> return $ prefix ++ [first]
    Just second -> do
      t <- optionMaybe octalDigit
      case t of
        Nothing    -> return $ prefix ++ [first, second]
        Just third -> return $ prefix ++ [first, second, third]

hexadecimalEscapeSequence :: P InputT
hexadecimalEscapeSequence = do
  prefix <- string "\\x"
  rest <- many1 hexadecimalDigit
  return $ prefix ++ rest

-- lex.fcon
-- floating-literal:
--  	fractional-constant exponent-part[opt] floating-suffix[opt]
--  	digit-sequence exponent-part floating-suffix[opt]
-- fractional-constant:
--  	digit-sequence[opt] . digit-sequence
--  	digit-sequence .
-- exponent-part:
--  	e sign[opt] digit-sequence
--  	E sign[opt] digit-sequence
-- sign:
--  	+
--  	-
-- digit-sequence:
--  	digit
--  	digit-sequence digit
-- floating-suffix:
--  	f
--  	l
--  	F
--  	L

floatingLiteral :: P Token
floatingLiteral = try fractional <|> digital
  where
    fractional = undefined
    digital = undefined

fractionalConstant :: P InputT
fractionalConstant = dotDigits <|> digitsDot
  where
    dotDigits = do
      bef <- option [] digitSequence
      dot <- string "."
      after <- digitSequence
      return $ bef ++ dot ++ after
    digitsDot = do
      ds <- digitSequence
      dot <- string "."
      return $ ds ++ dot

exponentPart :: P InputT
exponentPart = do
  e <- string "e" <|> string "E"
  s <- option [] sign
  rest <- digitSequence
  return $ e ++ s ++ rest

sign :: P InputT
sign = string "+" <|> string "-"

digitSequence :: P InputT
digitSequence = many1 digit

floatingSuffix :: P InputT
floatingSuffix = string "f" <|> string "l" <|> string "F" <|> string "L"

-- lex.string
-- string-literal:
--  	encoding-prefix[opt] " s-char-sequence[opt] "     C++0x
--  	encoding-prefix[opt] R raw-string     C++0x
-- encoding-prefix:
--  	u8     C++0x
--  	u     C++0x
--  	U     C++0x
--  	L     C++0x
-- s-char-sequence:
--  	s-char
--  	s-char-sequence s-char
-- s-char:
--  	any member of the source character set except the double-quote ", backslash \, or new-line character
--  	escape-sequence
--  	universal-character-name
-- raw-string:
--  	" d-char-sequence[opt] ( r-char-sequence[opt] ) d-char-sequence[opt] "     C++0x
-- r-char-sequence:
--  	r-char     C++0x
--  	r-char-sequence r-char     C++0x
-- r-char:
--  	any member of the source character set, except a right parenthesis ) followed by the initial d-char-sequence (which may be empty) followed by a double quote ".     C++0x
-- d-char-sequence:
--  	d-char     C++0x
--  	d-char-sequence d-char     C++0x
-- d-char:
--  	any member of the basic source character set, except: space, the left parenthesis (, the right parenthesis ), the backslash \, and the control characters representing horizontal tab, vertical tab, form feed, and newline.     C++0x
stringLiteral :: P Token
stringLiteral = undefined

encodingPrefix :: P InputT
encodingPrefix = undefined

sCharSequence :: P InputT
sCharSequence = undefined

sChar :: P InputT
sChar = undefined

rawString :: P InputT
rawString = undefined

rCharSequence :: P InputT
rCharSequence = undefined

rChar :: P InputT
rChar = undefined

dCharSequence :: P InputT
dCharSequence = undefined

dChar :: P InputT
dChar = undefined

-- lex.bool
-- boolean-literal:
--  	false
--  	true

booleanLiteral :: P Token
booleanLiteral = undefined

-- lex.nullptr
-- pointer-literal:
--  	nullptr     C++0x

pointerLiteral :: P Token
pointerLiteral = undefined

-- lex.ext
-- user-defined-literal:
--  	user-defined-integer-literal     C++0x
--  	user-defined-floating-literal     C++0x
--  	user-defined-string-literal     C++0x
--  	user-defined-character-literal     C++0x
-- user-defined-integer-literal:
--  	decimal-literal ud-suffix     C++0x
--  	octal-literal ud-suffix     C++0x
--  	hexadecimal-literal ud-suffix     C++0x
-- user-defined-floating-literal:
--  	fractional-constant exponent-part[opt] ud-suffix     C++0x
--  	digit-sequence exponent-part ud-suffix     C++0x
-- user-defined-string-literal:
--  	string-literal ud-suffix     C++0x
-- user-defined-character-literal:
--  	character-literal ud-suffix     C++0x
-- ud-suffix:
--  	identifier     C++0x
userDefinedLiteral :: P Token
userDefinedLiteral = undefined

userDefinedIntegerLiteral :: P InputT
userDefinedIntegerLiteral = undefined

userDefinedFloatingLiteral :: P InputT
userDefinedFloatingLiteral = undefined

userDefinedStringLiteral :: P InputT
userDefinedStringLiteral = undefined

userDefinedCharacterLiteral :: P InputT
userDefinedCharacterLiteral = undefined

udSuffix :: P InputT
udSuffix = undefined

---------------------------------------

angles :: P a -> P a
angles = between opLess opGreater

brackets :: P a -> P a
brackets = between leftBracket rightBracket

braces :: P a -> P a
braces = between leftBrace rightBrace

parens :: P a -> P a
parens = between leftParen rightParen

keyword :: InputT -> P ()
keyword s = do
  string s
  notFollowedBy alphaNum
