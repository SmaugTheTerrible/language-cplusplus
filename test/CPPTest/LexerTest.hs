module CPPTest.LexerTest where

import           Test.HUnit
import           Language.CPlusPlus.Internal.Lexer

tests =
  TestList [common, comments, newlines, keywords, punctuators, preprocessor]

common = TestList
  [trimSpacesTest, trimTabsTest, kwPostfixTest, kwPrefixTest, innerSpaceTest]

trimSpacesTest =
  "trim spaces test" ~: "[L (1,5) KW_Asm]" ~=? show (lexer "    asm   ")
trimTabsTest = "trim tabs test" ~: "[L (1,13) KW_Asm]" ~=? show
  (lexer "            asm     ")
kwPostfixTest = "asms /= asm"
  ~: assertBool "asms /= asm" ("[L (1,1) KW_Asm]" /= show (lexer "asms"))
kwPrefixTest = "sasm /= asm"
  ~: assertBool "sasm /= asm" ("[L (1,1) KW_Asm]" /= show (lexer "sasm"))
innerSpaceTest = "a sm /= asm"
  ~: assertBool "a sm /= asm" ("[L (1,1) KW_Asm]" /= show (lexer "a sm"))

comments = TestList [lineCommentTest, cstyleComment]

lineCommentTest =
  "line comment test" ~: "[L (1,1) (TLineComment \"//aaa\\n\")]" ~=? show
    (lexer "//aaa\n")
cstyleComment =
  "C-style comment test" ~: "[L (1,1) (TComment \"/*bbb*/\")]" ~=? show
    (lexer "/*bbb*/")

newlines = TestList [unixEOLTest, unixEOLTest, winEOLTest]

unixEOLTest = "\\n test" ~: "[L (1,1) EOL]" ~=? show (lexer "\n")
macEOLTest = "\\r test" ~: "[L (1,1) EOL]" ~=? show (lexer "\r")
winEOLTest = "\\r\\n test" ~: "[L (1,1) EOL]" ~=? show (lexer "\r\n")

keywords = TestList
  [ alignasTest
  , alignofTest
  , asmTest
  , autoTest
  , boolTest
  , breakTest
  , caseTest
  , catchTest
  , charTest
  , char16TTest
  , char32TTest
  , classTest
  , constTest
  , constexprTest
  , constCastTest
  , continueTest
  , decltypeTest
  , defaultTest
  , deleteTest
  , doTest
  , doubleTest
  , dynamicCastTest
  , elseTest
  , enumTest
  , explicitTest
  , exportTest
  , externTest
  , falseTest
  , floatTest
  , forTest
  , friendTest
  , gotoTest
  , ifTest
  , inlineTest
  , intTest
  , longTest
  , mutableTest
  , namespaceTest
  , newTest
  , noexceptTest
  , nullptrTest
  , operatorTest
  , privateTest
  , protectedTest
  , publicTest
  , registerTest
  , reinterpretCastTest
  , returnTest
  , shortTest
  , signedTest
  , sizeofTest
  , staticTest
  , staticAssertTest
  , staticCastTest
  , structTest
  , switchTest
  , templateTest
  , thisTest
  , threadLocalTest
  , throwTest
  , trueTest
  , tryTest
  , typedefTest
  , typeidTest
  , typenameTest
  , unionTest
  , unsignedTest
  , usingTest
  , virtualTest
  , voidTest
  , volatileTest
  , wcharTTest
  , whileTest
  ]

alignasTest =
  "alignas test" ~: "[L (1,1) KW_Alignas]" ~=? show (lexer "alignas")
alignofTest =
  "alignof test" ~: "[L (1,1) KW_Alignof]" ~=? show (lexer "alignof")
asmTest = "asm test" ~: "[L (1,1) KW_Asm]" ~=? show (lexer "asm")
autoTest = "auto test" ~: "[L (1,1) KW_Auto]" ~=? show (lexer "auto")
boolTest = "bool test" ~: "[L (1,1) KW_Bool]" ~=? show (lexer "bool")
breakTest = "break test" ~: "[L (1,1) KW_Break]" ~=? show (lexer "break")
caseTest = "case test" ~: "[L (1,1) KW_Case]" ~=? show (lexer "case")
catchTest = "catch test" ~: "[L (1,1) KW_Catch]" ~=? show (lexer "catch")
charTest = "char test" ~: "[L (1,1) KW_Char]" ~=? show (lexer "char")
char16TTest =
  "char16_t test" ~: "[L (1,1) KW_Char16T]" ~=? show (lexer "char16_t  ")
char32TTest =
  "char32_t test" ~: "[L (1,1) KW_Char32T]" ~=? show (lexer "char32_t  ")
classTest = "class test" ~: "[L (1,1) KW_Class]" ~=? show (lexer "class")
constTest = "const test" ~: "[L (1,1) KW_Const]" ~=? show (lexer "const")
constexprTest =
  "constexpr test" ~: "[L (1,1) KW_Constexpr]" ~=? show (lexer "constexpr     ")
constCastTest =
  "const_cast test" ~: "[L (1,1) KW_ConstCast]" ~=? show (lexer "const_cast")
continueTest =
  "continue test" ~: "[L (1,1) KW_Continue]" ~=? show (lexer "continue")
decltypeTest =
  "decltype test" ~: "[L (1,1) KW_Decltype]" ~=? show (lexer "decltype     ")
defaultTest =
  "default test" ~: "[L (1,1) KW_Default]" ~=? show (lexer "default")
deleteTest = "delete test" ~: "[L (1,1) KW_Delete]" ~=? show (lexer "delete")
doTest = "do test" ~: "[L (1,1) KW_Do]" ~=? show (lexer "do")
doubleTest = "double test" ~: "[L (1,1) KW_Double]" ~=? show (lexer "double")
dynamicCastTest = "dynamic_cast test" ~: "[L (1,1) KW_DynamicCast]" ~=? show
  (lexer "dynamic_cast")
elseTest = "else test" ~: "[L (1,1) KW_Else]" ~=? show (lexer "else")
enumTest = "enum test" ~: "[L (1,1) KW_Enum]" ~=? show (lexer "enum")
explicitTest =
  "explicit test" ~: "[L (1,1) KW_Explicit]" ~=? show (lexer "explicit")
exportTest = "export test" ~: "[L (1,1) KW_Export]" ~=? show (lexer "export")
externTest = "extern test" ~: "[L (1,1) KW_Extern]" ~=? show (lexer "extern")
falseTest = "false test" ~: "[L (1,1) (Literal_Boolean \"false\")]" ~=? show
  (lexer "false")
floatTest = "float test" ~: "[L (1,1) KW_Float]" ~=? show (lexer "float")
forTest = "for test" ~: "[L (1,1) KW_For]" ~=? show (lexer "for")
friendTest = "friend test" ~: "[L (1,1) KW_Friend]" ~=? show (lexer "friend")
gotoTest = " test" ~: "[L (1,1) KW_Goto]" ~=? show (lexer "goto")
ifTest = "if test" ~: "[L (1,1) KW_If]" ~=? show (lexer "if")
inlineTest = "inline test" ~: "[L (1,1) KW_Inline]" ~=? show (lexer "inline")
intTest = "int test" ~: "[L (1,1) KW_Int]" ~=? show (lexer "int")
longTest = "long test" ~: "[L (1,1) KW_Long]" ~=? show (lexer "long")
mutableTest =
  "mutable test" ~: "[L (1,1) KW_Mutable]" ~=? show (lexer "mutable")
namespaceTest =
  "namespace test" ~: "[L (1,1) KW_Namespace]" ~=? show (lexer "namespace")
newTest = "new test" ~: "[L (1,1) KW_New]" ~=? show (lexer "new")
noexceptTest =
  "noexcept test" ~: "[L (1,1) KW_Noexcept]" ~=? show (lexer "noexcept   ")
nullptrTest =
  "nullptr test" ~: "[L (1,1) (Literal_NullPtr \"nullptr\")]" ~=? show
    (lexer "nullptr     ")
operatorTest =
  "operator test" ~: "[L (1,1) KW_Operator]" ~=? show (lexer "operator")
privateTest =
  "private test" ~: "[L (1,1) KW_Private]" ~=? show (lexer "private")
protectedTest =
  "protected test" ~: "[L (1,1) KW_Protected]" ~=? show (lexer "protected")
publicTest = "public test" ~: "[L (1,1) KW_Public]" ~=? show (lexer "public")
registerTest =
  "register test" ~: "[L (1,1) KW_Register]" ~=? show (lexer "register")
reinterpretCastTest =
  "reinterpret_cast test" ~: "[L (1,1) KW_ReinterpretCast]" ~=? show
    (lexer "reinterpret_cast")
returnTest = "return test" ~: "[L (1,1) KW_Return]" ~=? show (lexer "return")
shortTest = "short test" ~: "[L (1,1) KW_Short]" ~=? show (lexer "short")
signedTest = "signed test" ~: "[L (1,1) KW_Signed]" ~=? show (lexer "signed")
sizeofTest = "sizeof test" ~: "[L (1,1) KW_Sizeof]" ~=? show (lexer "sizeof")
staticTest = "static test" ~: "[L (1,1) KW_Static]" ~=? show (lexer "static")
staticAssertTest = "static_assert test" ~: "[L (1,1) KW_StaticAssert]" ~=? show
  (lexer "static_assert     ")
staticCastTest =
  "static_cast test" ~: "[L (1,1) KW_StaticCast]" ~=? show (lexer "static_cast")
structTest = "struct test" ~: "[L (1,1) KW_Struct]" ~=? show (lexer "struct")
switchTest = "switch test" ~: "[L (1,1) KW_Switch]" ~=? show (lexer "switch")
templateTest =
  "template test" ~: "[L (1,1) KW_Template]" ~=? show (lexer "template")
thisTest = "this test" ~: "[L (1,1) KW_This]" ~=? show (lexer "this")
threadLocalTest = "thread_local test" ~: "[L (1,1) KW_ThreadLocal]" ~=? show
  (lexer "thread_local     ")
throwTest = "throw test" ~: "[L (1,1) KW_Throw]" ~=? show (lexer "throw")
trueTest =
  "true test" ~: "[L (1,1) (Literal_Boolean \"true\")]" ~=? show (lexer "true")
tryTest = "try test" ~: "[L (1,1) KW_Try]" ~=? show (lexer "try")
typedefTest =
  "typedef test" ~: "[L (1,1) KW_Typedef]" ~=? show (lexer "typedef")
typeidTest = "typeid test" ~: "[L (1,1) KW_Typeid]" ~=? show (lexer "typeid")
typenameTest =
  "typename test" ~: "[L (1,1) KW_Typename]" ~=? show (lexer "typename")
unionTest = "union test" ~: "[L (1,1) KW_Union]" ~=? show (lexer "union")
unsignedTest =
  "unsigned test" ~: "[L (1,1) KW_Unsigned]" ~=? show (lexer "unsigned")
usingTest = "using test" ~: "[L (1,1) KW_Using]" ~=? show (lexer "using")
virtualTest =
  "virtual test" ~: "[L (1,1) KW_Virtual]" ~=? show (lexer "virtual")
voidTest = "void test" ~: "[L (1,1) KW_Void]" ~=? show (lexer "void")
volatileTest =
  "volatile test" ~: "[L (1,1) KW_Volatile]" ~=? show (lexer "volatile")
wcharTTest = "wchar_t test" ~: "[L (1,1) KW_WCharT]" ~=? show (lexer "wchar_t")
whileTest = "while test" ~: "[L (1,1) KW_While]" ~=? show (lexer "while")

punctuators = TestList
  [ leftBraceTest
  , rightBraceTest
  , leftBracketTest
  , rightBracketTest
  , hashTest
  , doubleHashTest
  , leftParenTest
  , rightParenTest
  , semiTest
  , colonTest
  , threeDotTest
  , questionMarkTest
  , doubleColonTest
  , dotTest
  , dotPtrTest
  , plusTest
  , minusTest
  , mulTest
  , divTest
  , remTest
  , xorTest
  , andTest
  , orTest
  , tildaTest
  , notTest
  , assignTest
  , lessTest
  , greaterTest
  , assignPlusTest
  , assignMinusTest
  , assignMulTest
  , assignDivTest
  , assignRemTest
  , assignXorTest
  , assignAndTest
  , assignOrTest
  , leftShiftTest
  , rightShiftTest
  , assignLeftShiftTest
  , assignRightShiftTest
  , eqTest
  , notEqTest
  , lessEqTest
  , greaterEqTest
  , logicalAndTest
  , logicalOrTest
  , incrementTest
  , decrementTest
  , commaTest
  , arrowPtrTest
  , arrowTest
  ]

leftBraceTest =
  "Left brace test" ~: "[L (1,1) Punc_LeftBrace]" ~=? show (lexer "{")
rightBraceTest =
  "Right brace test" ~: "[L (1,1) Punc_RightBrace]" ~=? show (lexer "}")
leftBracketTest =
  "Left Bracket test" ~: "[L (1,1) Punc_LeftBracket]" ~=? show (lexer "[")
rightBracketTest =
  "Right Bracket test" ~: "[L (1,1) Punc_RightBracket]" ~=? show (lexer "]")
hashTest = "Hash test" ~: "[L (1,1) Punc_Hash]" ~=? show (lexer "#")
doubleHashTest =
  "Double Hash test" ~: "[L (1,1) Punc_DoubleHash]" ~=? show (lexer "##")
leftParenTest =
  "Left Paren test" ~: "[L (1,1) Punc_LeftParen]" ~=? show (lexer "(")
rightParenTest =
  "Right Paren test" ~: "[L (1,1) Punc_RightParen]" ~=? show (lexer ")")

--Test = " test" ~: "[L (1,1) Punc_LeftBracket]" ~=? show (lexer "<:"  )
--Test = " test" ~: "[L (1,1) Punc_RightBracket]" ~=? show (lexer ":>"  )
--Test = " test" ~: "[L (1,1) Punc_LeftBrace]" ~=? show (lexer "<%"  )
--Test = " test" ~: "[L (1,1) Punc_RightBrace]" ~=? show (lexer "%>"  )
--Test = " test" ~: "[L (1,1) Punc_Hash]" ~=? show (lexer "%:"  )
--Test = " test" ~: "[L (1,1) Punc_DoubleHash]" ~=? show (lexer "%:%:")

semiTest = "semi test" ~: "[L (1,1) Punc_Semi]" ~=? show (lexer ";")
colonTest = "colon test" ~: "[L (1,1) Punc_Colon]" ~=? show (lexer ":")
threeDotTest =
  "Three Dot test" ~: "[L (1,1) Punc_ThreeDot]" ~=? show (lexer "...")
questionMarkTest =
  "Question Mark test" ~: "[L (1,1) Punc_QuestionMark]" ~=? show (lexer "?")
doubleColonTest =
  "Double Colon test" ~: "[L (1,1) Punc_DoubleColon]" ~=? show (lexer "::")
dotTest = "Dot test" ~: "[L (1,1) Punc_Dot]" ~=? show (lexer ".")
dotPtrTest = "DotPtr test" ~: "[L (1,1) Op_DotPtr]" ~=? show (lexer ".*")
plusTest = "Plus test" ~: "[L (1,1) Op_Plus]" ~=? show (lexer "+")
minusTest = "Minus test" ~: "[L (1,1) Op_Minus]" ~=? show (lexer "-")
mulTest = "Mul test" ~: "[L (1,1) Op_Mul]" ~=? show (lexer "*")
divTest = "Div test" ~: "[L (1,1) Op_Div]" ~=? show (lexer "/")
remTest = "Rem test" ~: "[L (1,1) Op_Rem]" ~=? show (lexer "%")
xorTest = "Xor test" ~: "[L (1,1) Op_Xor]" ~=? show (lexer "^")
andTest = "And test" ~: "[L (1,1) Op_And]" ~=? show (lexer "&")
orTest = "Or test" ~: "[L (1,1) Op_Or]" ~=? show (lexer "|")
tildaTest = "Tilda test" ~: "[L (1,1) Op_Tilda]" ~=? show (lexer "~")
notTest = "Not test" ~: "[L (1,1) Op_Not]" ~=? show (lexer "!")
assignTest = "Assign test" ~: "[L (1,1) Op_Assign]" ~=? show (lexer "=")
lessTest = "Less test" ~: "[L (1,1) Op_Less]" ~=? show (lexer "<")
greaterTest = "Greater test" ~: "[L (1,1) Op_Greater]" ~=? show (lexer ">")
assignPlusTest =
  "AssignPlus test" ~: "[L (1,1) Op_AssignPlus]" ~=? show (lexer "+=")
assignMinusTest =
  "AssignMinus test" ~: "[L (1,1) Op_AssignMinus]" ~=? show (lexer "-=")
assignMulTest =
  "AssignMul test" ~: "[L (1,1) Op_AssignMul]" ~=? show (lexer "*=")
assignDivTest =
  "AssignDiv test" ~: "[L (1,1) Op_AssignDiv]" ~=? show (lexer "/=")
assignRemTest =
  "AssignRem test" ~: "[L (1,1) Op_AssignRem]" ~=? show (lexer "%=")
assignXorTest =
  "AssignXor test" ~: "[L (1,1) Op_AssignXor]" ~=? show (lexer "^=")
assignAndTest =
  "AssignAnd test" ~: "[L (1,1) Op_AssignAnd]" ~=? show (lexer "&=")
assignOrTest = "AssignOr test" ~: "[L (1,1) Op_AssignOr]" ~=? show (lexer "|=")
leftShiftTest =
  "LeftShift test" ~: "[L (1,1) Op_LeftShift]" ~=? show (lexer "<<")
rightShiftTest =
  "RightShift test" ~: "[L (1,1) Op_RightShift]" ~=? show (lexer ">>")
assignLeftShiftTest =
  "AssignLeftShift test" ~: "[L (1,1) Op_AssignLeftShift]" ~=? show
    (lexer "<<=")
assignRightShiftTest =
  "AssignRightShift test" ~: "[L (1,1) Op_AssignRightShift]" ~=? show
    (lexer ">>=")
eqTest = "Eq test" ~: "[L (1,1) Op_Eq]" ~=? show (lexer "==")
notEqTest = "NotEq test" ~: "[L (1,1) Op_NotEq]" ~=? show (lexer "!=")
lessEqTest = "LessEq test" ~: "[L (1,1) Op_LessEq]" ~=? show (lexer "<=")
greaterEqTest =
  "GreaterEq test" ~: "[L (1,1) Op_GreaterEq]" ~=? show (lexer ">=")
logicalAndTest =
  "LogicalAnd test" ~: "[L (1,1) Op_LogicalAnd]" ~=? show (lexer "&&")
logicalOrTest =
  "LogicalOr test" ~: "[L (1,1) Op_LogicalOr]" ~=? show (lexer "||")
incrementTest =
  "Increment test" ~: "[L (1,1) Op_Increment]" ~=? show (lexer "++")
decrementTest =
  "Decrement test" ~: "[L (1,1) Op_Decrement]" ~=? show (lexer "--")
commaTest = "Comma test" ~: "[L (1,1) Punc_Comma]" ~=? show (lexer ",")
arrowPtrTest =
  "ArrowPtr test" ~: "[L (1,1) Op_ArrowPtr]" ~=? show (lexer "->*")
arrowTest = "Arrow test" ~: "[L (1,1) Op_Arrow]" ~=? show (lexer "->")

--Test = " test" ~: "[L (1,1) Op_LogicalAnd]" ~=? show (lexer "and"   )
--Test = " test" ~: "[L (1,1) Op_AssignAnd]" ~=? show (lexer "and_eq")
--Test = " test" ~: "[L (1,1) Op_And]" ~=? show (lexer "bitand")
--Test = " test" ~: "[L (1,1) Op_Or]" ~=? show (lexer "bitor" )
--Test = " test" ~: "[L (1,1) Op_Tilda]" ~=? show (lexer "compl") 
--Test = " test" ~: "[L (1,1) Op_Not]" ~=? show (lexer "not")   
--Test = " test" ~: "[L (1,1) Op_NotEq]" ~=? show (lexer "not_eq")
--Test = " test" ~: "[L (1,1) Op_LogicalOr]" ~=? show (lexer "or")    
--Test = " test" ~: "[L (1,1) Op_AssignOr]" ~=? show (lexer "or_eq") 
--Test = " test" ~: "[L (1,1) Op_Xor]" ~=? show (lexer "xor")   
--Test = " test" ~: "[L (1,1) Op_AssignXor]" ~=? show (lexer "xor_eq")

preprocessor = TestList
  [ ppIfTest
  , ppIfdefTest
  , ppIfndefTest
  , ppElifTest
  , ppElseTest
  , ppEndifTest
  , ppIncludeTest
  , ppDefineTest
  , ppUndefTest
  , ppLineTest
  , ppErrorTest
  , ppPragmaTest
  ]

ppIfTest = "PP_If test" ~: "[L (1,1) PP_If]" ~=? show (lexer "#if")
ppIfdefTest = "PP_Ifdef test" ~: "[L (1,1) PP_Ifdef]" ~=? show (lexer "#ifdef")
ppIfndefTest =
  "PP_Ifndef test" ~: "[L (1,1) PP_Ifndef]" ~=? show (lexer "#ifndef")
ppElifTest = "PP_Elif test" ~: "[L (1,1) PP_Elif]" ~=? show (lexer "#elif")
ppElseTest = "PP_Else test" ~: "[L (1,1) PP_Else]" ~=? show (lexer "#else")
ppEndifTest = "PP_Endif test" ~: "[L (1,1) PP_Endif]" ~=? show (lexer "#endif")
ppIncludeTest =
  "PP_Include test" ~: "[L (1,1) PP_Include]" ~=? show (lexer "#include")
ppDefineTest =
  "PP_Define test" ~: "[L (1,1) PP_Define]" ~=? show (lexer "#define")
ppUndefTest = "PP_Undef test" ~: "[L (1,1) PP_Undef]" ~=? show (lexer "#undef")
ppLineTest = "PP_Line test" ~: "[L (1,1) PP_Line]" ~=? show (lexer "#line")
ppErrorTest = "PP_Error test" ~: "[L (1,1) PP_Error]" ~=? show (lexer "#error")
ppPragmaTest =
  "PP_Pragma test" ~: "[L (1,1) PP_Pragma]" ~=? show (lexer "#pragma")

literals = TestList
  [ integerLiterals
  , charLiterals
  , floatingLiterals
  , stringLiterals
  , userDefinedLiterals
  ]

integerLiterals = TestList []
charLiterals = TestList []
floatingLiterals = TestList []
stringLiterals = TestList []
userDefinedLiterals = TestList []

identifiers = TestList []
