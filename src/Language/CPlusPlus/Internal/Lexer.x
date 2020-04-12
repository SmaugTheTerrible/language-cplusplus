{
module Language.CPlusPlus.Internal.Lexer where

import Text.Parsec
import Text.Parsec.Pos (newPos)

import Language.CPlusPlus.Internal.Types.Lexer

}

%wrapper "posn"

$cppWhite = [\ \t\f\v]

$hexDigit = [0-9a-fA-F]
$octDigit = [0-7]
@hexQuad = $hexDigit{4}

$nondigit = [a-zA-Z_]
$digit = [0-9]
$nonzero = [1-9]
$opPuncSymbol = [\~\!\@\#\$\%\^\&\*\(\)\[\]\{\}\-\=\+\\\|\;\'\:\"\,\.\/\<\>\?]

@sourceChar = $digit | $nondigit
@sourceCharSeq = (@sourceChar)+

@universalCharacterName = \\ [uU] @hexQuad

@lineterm = [\n\r] | \r\n

@lineComment = "//" .* @lineterm
@cstyleComment = "/*" .* "*/"
@comment = @lineComment | @cstyleComment

@identifier = $nondigit ($digit | $nondigit)*

$sign = [\+\-]
@digits = $digit+
@number = $digit* . $digit*
@expNumber = @number [eE] $sign @digits
@ppNumber = @expNumber | @number | @digits

@decimalLiteral = $nonzero $digit*
@octalLiteral = 0 $octDigit+
@hexadecimalLiteral = 0 [xX] $hexDigit+
$unsignedSuffix = [uU]
$longSuffix = [lL]
@longLongSuffix = ll | LL
@integerSuffix = ($unsignedSuffix @longLongSuffix?)
               | ($unsignedSuffix $longSuffix?)
               | (@longLongSuffix $unsignedSuffix?)
               | ($longSuffix $unsignedSuffix?)
@integerLiteral = (@decimalLiteral | @octalLiteral | @hexadecimalLiteral | 0) @integerSuffix?

@octEscape = [0123]? $octDigit{1, 2}
@hexEscape = \\x $hexDigit{1,2}
@escapeSequence = \\ (@octEscape | @hexEscape | [abfnrtv\'\"\?\\])
@cChar = @universalCharacterName | @escapeSequence | [^\'\\\n]
@charLiteral = [uUL]?\' @cChar \'

$floatingSuffix = [fFlL]
@exponentPart = [eE] $sign? @digits
@fractionalConstant = ($digit* "." @digits)|(@digits ".")
@floatingLiteral = (@digits @exponentPart $floatingSuffix?)
                 | (@fractionalConstant @exponentPart? $floatingSuffix?)

@encodingPrefix = [uUL]|u8
@sChar = @universalCharacterName | @escapeSequence | [^\"\\\n]
$dChar = [^\(\)\ \\\n\r\t\v\f]
$rChar = [^\)]
@rawString = \" $dChar* \( $rChar* \) $dChar* \"
@stringLiteral = (@encodingPrefix? \" @sChar* \")
               | (@encodingPrefix? "R" @rawString)

@userDefinedIntegerLiteral = (@decimalLiteral @identifier)
                           | (@octalLiteral @identifier)
                           | (@hexadecimalLiteral @identifier)
@userDefinedFloatingLiteral = (@fractionalConstant @exponentPart? @identifier)
                            | (@digits @exponentPart @identifier)
@userDefinedCharLiteral = @charLiteral @identifier
@userDefinedStringLiteral = @stringLiteral @identifier
@userDefinedLiteral = @userDefinedIntegerLiteral
                    | @userDefinedFloatingLiteral
                    | @userDefinedCharLiteral
                    | @userDefinedStringLiteral

tokens :-
    $cppWhite+          ;
    \\@lineterm         ;
    @lineComment        { \p s -> L (pos p) $ TLineComment s        }
    @cstyleComment      { \p s -> L (pos p) $ TComment s            }
    @lineterm           { \p s -> L (pos p) $ EOL                   }
    alignas             { \p _ -> L (pos p) $ KW_Alignas            }
    alignof             { \p _ -> L (pos p) $ KW_Alignof            }
    asm                 { \p _ -> L (pos p) $ KW_Asm                }
    auto                { \p _ -> L (pos p) $ KW_Auto               }
    bool                { \p _ -> L (pos p) $ KW_Bool               }
    break               { \p _ -> L (pos p) $ KW_Break              }
    case                { \p _ -> L (pos p) $ KW_Case               }
    catch               { \p _ -> L (pos p) $ KW_Catch              }
    char                { \p _ -> L (pos p) $ KW_Char               }
    char16_t            { \p _ -> L (pos p) $ KW_Char16T            }
    char32_t            { \p _ -> L (pos p) $ KW_Char32T            }
    class               { \p _ -> L (pos p) $ KW_Class              }
    const               { \p _ -> L (pos p) $ KW_Const              }
    constexpr           { \p _ -> L (pos p) $ KW_Constexpr          }
    const_cast          { \p _ -> L (pos p) $ KW_ConstCast          }
    continue            { \p _ -> L (pos p) $ KW_Continue           }
    decltype            { \p _ -> L (pos p) $ KW_Decltype           }
    default             { \p _ -> L (pos p) $ KW_Default            }
    delete              { \p _ -> L (pos p) $ KW_Delete             }
    do                  { \p _ -> L (pos p) $ KW_Do                 }
    double              { \p _ -> L (pos p) $ KW_Double             }
    dynamic_cast        { \p _ -> L (pos p) $ KW_DynamicCast        }
    else                { \p _ -> L (pos p) $ KW_Else               }
    enum                { \p _ -> L (pos p) $ KW_Enum               }
    explicit            { \p _ -> L (pos p) $ KW_Explicit           }
    export              { \p _ -> L (pos p) $ KW_Export             }
    extern              { \p _ -> L (pos p) $ KW_Extern             }
    float               { \p _ -> L (pos p) $ KW_Float              }
    final               { \p _ -> L (pos p) $ KW_Final              }
    for                 { \p _ -> L (pos p) $ KW_For                }
    friend              { \p _ -> L (pos p) $ KW_Friend             }
    goto                { \p _ -> L (pos p) $ KW_Goto               }
    if                  { \p _ -> L (pos p) $ KW_If                 }
    inline              { \p _ -> L (pos p) $ KW_Inline             }
    int                 { \p _ -> L (pos p) $ KW_Int                }
    long                { \p _ -> L (pos p) $ KW_Long               }
    mutable             { \p _ -> L (pos p) $ KW_Mutable            }
    namespace           { \p _ -> L (pos p) $ KW_Namespace          }
    new                 { \p _ -> L (pos p) $ KW_New                }
    noexcept            { \p _ -> L (pos p) $ KW_Noexcept           }
    operator            { \p _ -> L (pos p) $ KW_Operator           }
    private             { \p _ -> L (pos p) $ KW_Private            }
    protected           { \p _ -> L (pos p) $ KW_Protected          }
    public              { \p _ -> L (pos p) $ KW_Public             }
    register            { \p _ -> L (pos p) $ KW_Register           }
    reinterpret_cast    { \p _ -> L (pos p) $ KW_ReinterpretCast    }
    return              { \p _ -> L (pos p) $ KW_Return             }
    short               { \p _ -> L (pos p) $ KW_Short              }
    signed              { \p _ -> L (pos p) $ KW_Signed             }
    sizeof              { \p _ -> L (pos p) $ KW_Sizeof             }
    static              { \p _ -> L (pos p) $ KW_Static             }
    static_assert       { \p _ -> L (pos p) $ KW_StaticAssert       }
    static_cast         { \p _ -> L (pos p) $ KW_StaticCast         }
    struct              { \p _ -> L (pos p) $ KW_Struct             }
    switch              { \p _ -> L (pos p) $ KW_Switch             }
    template            { \p _ -> L (pos p) $ KW_Template           }
    this                { \p _ -> L (pos p) $ KW_This               }
    thread_local        { \p _ -> L (pos p) $ KW_ThreadLocal        }
    throw               { \p _ -> L (pos p) $ KW_Throw              }
    try                 { \p _ -> L (pos p) $ KW_Try                }
    typedef             { \p _ -> L (pos p) $ KW_Typedef            }
    typeid              { \p _ -> L (pos p) $ KW_Typeid             }
    typename            { \p _ -> L (pos p) $ KW_Typename           }
    union               { \p _ -> L (pos p) $ KW_Union              }
    unsigned            { \p _ -> L (pos p) $ KW_Unsigned           }
    using               { \p _ -> L (pos p) $ KW_Using              }
    virtual             { \p _ -> L (pos p) $ KW_Virtual            }
    void                { \p _ -> L (pos p) $ KW_Void               }
    volatile            { \p _ -> L (pos p) $ KW_Volatile           }
    wchar_t             { \p _ -> L (pos p) $ KW_WCharT             }
    while               { \p _ -> L (pos p) $ KW_While              }
    true                { \p s -> L (pos p) $ Literal_Boolean s     }
    false               { \p s -> L (pos p) $ Literal_Boolean s     }
    nullptr             { \p s -> L (pos p) $ Literal_NullPtr s     }
    "{"                 { \p _ -> L (pos p) $ Punc_LeftBrace        }
    "}"                 { \p _ -> L (pos p) $ Punc_RightBrace       }
    "["                 { \p _ -> L (pos p) $ Punc_LeftBracket      }
    "]"                 { \p _ -> L (pos p) $ Punc_RightBracket     }
    "#"                 { \p _ -> L (pos p) $ Punc_Hash             }
    "##"                { \p _ -> L (pos p) $ Punc_DoubleHash       }
    "("                 { \p _ -> L (pos p) $ Punc_LeftParen        }
    ")"                 { \p _ -> L (pos p) $ Punc_RightParen       }
    "<:"                { \p _ -> L (pos p) $ Punc_LeftBracket      }
    ":>"                { \p _ -> L (pos p) $ Punc_RightBracket     }
    "<%"                { \p _ -> L (pos p) $ Punc_LeftBrace        }
    "%>"                { \p _ -> L (pos p) $ Punc_RightBrace       }
    "%:"                { \p _ -> L (pos p) $ Punc_Hash             }
    "%:%:"              { \p _ -> L (pos p) $ Punc_DoubleHash       }
    ";"                 { \p _ -> L (pos p) $ Punc_Semi             }
    ":"                 { \p _ -> L (pos p) $ Punc_Colon            }
    "..."               { \p _ -> L (pos p) $ Punc_ThreeDot         }
    "?"                 { \p _ -> L (pos p) $ Punc_QuestionMark     }
    "::"                { \p _ -> L (pos p) $ Punc_DoubleColon      }
    "."                 { \p _ -> L (pos p) $ Punc_Dot              }
    ".*"                { \p _ -> L (pos p) $ Op_DotPtr             }
    "+"                 { \p _ -> L (pos p) $ Op_Plus               }
    "-"                 { \p _ -> L (pos p) $ Op_Minus              }
    "*"                 { \p _ -> L (pos p) $ Op_Mul                }
    "/"                 { \p _ -> L (pos p) $ Op_Div                }
    "%"                 { \p _ -> L (pos p) $ Op_Rem                }
    "^"                 { \p _ -> L (pos p) $ Op_Xor                }
    "&"                 { \p _ -> L (pos p) $ Op_And                }
    "|"                 { \p _ -> L (pos p) $ Op_Or                 }
    "~"                 { \p _ -> L (pos p) $ Op_Tilda              }
    "!"                 { \p _ -> L (pos p) $ Op_Not                }
    "="                 { \p _ -> L (pos p) $ Op_Assign             }
    "<"                 { \p _ -> L (pos p) $ Op_Less               }
    ">"                 { \p _ -> L (pos p) $ Op_Greater            }
    "+="                { \p _ -> L (pos p) $ Op_AssignPlus         }
    "-="                { \p _ -> L (pos p) $ Op_AssignMinus        }
    "*="                { \p _ -> L (pos p) $ Op_AssignMul          }
    "/="                { \p _ -> L (pos p) $ Op_AssignDiv          }
    "%="                { \p _ -> L (pos p) $ Op_AssignRem          }
    "^="                { \p _ -> L (pos p) $ Op_AssignXor          }
    "&="                { \p _ -> L (pos p) $ Op_AssignAnd          }
    "|="                { \p _ -> L (pos p) $ Op_AssignOr           }
    "<<"                { \p _ -> L (pos p) $ Op_LeftShift          }
    ">>"                { \p _ -> L (pos p) $ Op_RightShift         }
    "<<="               { \p _ -> L (pos p) $ Op_AssignLeftShift    }
    ">>="               { \p _ -> L (pos p) $ Op_AssignRightShift   }
    "=="                { \p _ -> L (pos p) $ Op_Eq                 }
    "!="                { \p _ -> L (pos p) $ Op_NotEq              }
    "<="                { \p _ -> L (pos p) $ Op_LessEq             }
    ">="                { \p _ -> L (pos p) $ Op_GreaterEq          }
    "&&"                { \p _ -> L (pos p) $ Op_LogicalAnd         }
    "||"                { \p _ -> L (pos p) $ Op_LogicalOr          }
    "++"                { \p _ -> L (pos p) $ Op_Increment          }
    "--"                { \p _ -> L (pos p) $ Op_Decrement          }
    ","                 { \p _ -> L (pos p) $ Punc_Comma            }
    "->*"               { \p _ -> L (pos p) $ Op_ArrowPtr           }
    "->"                { \p _ -> L (pos p) $ Op_Arrow              }
    and                 { \p _ -> L (pos p) $ Op_LogicalAnd         }
    and_eq              { \p _ -> L (pos p) $ Op_AssignAnd          }
    bitand              { \p _ -> L (pos p) $ Op_And                }
    bitor               { \p _ -> L (pos p) $ Op_Or                 }
    compl               { \p _ -> L (pos p) $ Op_Tilda              }
    not                 { \p _ -> L (pos p) $ Op_Not                }
    not_eq              { \p _ -> L (pos p) $ Op_NotEq              }
    or                  { \p _ -> L (pos p) $ Op_LogicalOr          }
    or_eq               { \p _ -> L (pos p) $ Op_AssignOr           }
    xor                 { \p _ -> L (pos p) $ Op_Xor                }
    xor_eq              { \p _ -> L (pos p) $ Op_AssignXor          }
    "#if"               { \p _ -> L (pos p) $ PP_If                 }
    "#ifdef"            { \p _ -> L (pos p) $ PP_Ifdef              }
    "#ifndef"           { \p _ -> L (pos p) $ PP_Ifndef             }
    "#elif"             { \p _ -> L (pos p) $ PP_Elif               }
    "#else"             { \p _ -> L (pos p) $ PP_Else               }
    "#endif"            { \p _ -> L (pos p) $ PP_Endif              }
    "#include"          { \p _ -> L (pos p) $ PP_Include            }
    "#define"           { \p _ -> L (pos p) $ PP_Define             }
    "#undef"            { \p _ -> L (pos p) $ PP_Undef              }
    "#line"             { \p _ -> L (pos p) $ PP_Line               }
    "#error"            { \p _ -> L (pos p) $ PP_Error              }
    "#pragma"           { \p _ -> L (pos p) $ PP_Pragma             }
    @integerLiteral     { \p s -> L (pos p) $ Literal_Integer s     }
    @charLiteral        { \p s -> L (pos p) $ Literal_Char s        }
    @floatingLiteral    { \p s -> L (pos p) $ Literal_Float s       }
    @stringLiteral      { \p s -> L (pos p) $ Literal_String s      }
    @userDefinedLiteral { \p s -> L (pos p) $ Literal_UserDefined s }
    @identifier         { \p s -> L (pos p) $ Id s                  }
{

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

lexer = alexScanTokens

cppToken :: Token -> P ()
cppToken t' = cppToken' (\t -> if t' == t then Just () else Nothing)

cppToken' :: (Token -> Maybe a) -> P a
cppToken' test = token showTok posFromTok testTok
  where
    showTok    (L _ t)      = show t
    posFromTok (L (l, c) _) = newPos "" l c
    testTok    (L _ t)      = test t

kwAlignas         = cppToken KW_Alignas
kwAlignof         = cppToken KW_Alignof
kwAsm             = cppToken KW_Asm
kwAuto            = cppToken KW_Auto
kwBool            = cppToken KW_Bool
kwBreak           = cppToken KW_Break
kwCase            = cppToken KW_Case
kwCatch           = cppToken KW_Catch
kwChar            = cppToken KW_Char
kwChar16T         = cppToken KW_Char16T
kwChar32T         = cppToken KW_Char32T
kwClass           = cppToken KW_Class
kwConst           = cppToken KW_Const
kwConstexpr       = cppToken KW_Constexpr
kwConstCast       = cppToken KW_ConstCast
kwContinue        = cppToken KW_Continue
kwDecltype        = cppToken KW_Decltype
kwDefault         = cppToken KW_Default
kwDelete          = cppToken KW_Delete
kwDo              = cppToken KW_Do
kwDouble          = cppToken KW_Double
kwDynamicCast     = cppToken KW_DynamicCast
kwElse            = cppToken KW_Else
kwEnum            = cppToken KW_Enum
kwExplicit        = cppToken KW_Explicit
kwExport          = cppToken KW_Export
kwExtern          = cppToken KW_Extern
kwFalse           = cppToken KW_False
kwFloat           = cppToken KW_Float
kwFinal           = cppToken KW_Final
kwFor             = cppToken KW_For
kwFriend          = cppToken KW_Friend
kwGoto            = cppToken KW_Goto
kwIf              = cppToken KW_If
kwInline          = cppToken KW_Int
kwInt             = cppToken KW_Inline
kwLong            = cppToken KW_Long
kwMutable         = cppToken KW_Mutable
kwNamespace       = cppToken KW_Namespace
kwNew             = cppToken KW_New
kwNoexcept        = cppToken KW_Noexcept
kwNullptr         = cppToken KW_Nullptr
kwOperator        = cppToken KW_Operator
kwPrivate         = cppToken KW_Private
kwProtected       = cppToken KW_Protected
kwPublic          = cppToken KW_Public
kwRegister        = cppToken KW_Register
kwReinterpretCast = cppToken KW_ReinterpretCast
kwReturn          = cppToken KW_Return
kwShort           = cppToken KW_Short
kwSigned          = cppToken KW_Signed
kwSizeof          = cppToken KW_Sizeof
kwStatic          = cppToken KW_Static
kwStaticAssert    = cppToken KW_StaticAssert
kwStaticCast      = cppToken KW_StaticCast
kwStruct          = cppToken KW_Struct
kwSwitch          = cppToken KW_Switch
kwTemplate        = cppToken KW_Template
kwThis            = cppToken KW_This
kwThreadLocal     = cppToken KW_ThreadLocal
kwThrow           = cppToken KW_Throw
kwTrue            = cppToken KW_True
kwTry             = cppToken KW_Try
kwTypedef         = cppToken KW_Typedef
kwTypeid          = cppToken KW_Typeid
kwTypename        = cppToken KW_Typename
kwUnion           = cppToken KW_Union
kwUnsigned        = cppToken KW_Unsigned
kwUsing           = cppToken KW_Using
kwVirtual         = cppToken KW_Virtual
kwVoid            = cppToken KW_Void
kwVolatile        = cppToken KW_Volatile
kwWCharT          = cppToken KW_WCharT
kwWhile           = cppToken KW_While

leftBrace           = cppToken Punc_LeftBrace
rightBrace          = cppToken Punc_RightBrace
leftBracket         = cppToken Punc_LeftBracket
rightBracket        = cppToken Punc_RightBracket
hash                = cppToken Punc_Hash
doubleHash          = cppToken Punc_DoubleHash
leftParen           = cppToken Punc_LeftParen
rightParen          = cppToken Punc_RightParen
semi                = cppToken Punc_Semi
colon               = cppToken Punc_Colon
questionMark        = cppToken Punc_QuestionMark
doubleColon         = cppToken Punc_DoubleColon
dot                 = cppToken Punc_Dot
threeDot            = cppToken Punc_ThreeDot
--opNew               = cppToken Op_
--opDelete            = cppToken Op_
opDotPtr            = cppToken Op_DotPtr
opPlus              = cppToken Op_Plus
opMinus             = cppToken Op_Minus
opMul               = cppToken Op_Mul
opDiv               = cppToken Op_Div
opRem               = cppToken Op_Rem
opXor               = cppToken Op_Xor
opAnd               = cppToken Op_And
opOr                = cppToken Op_Or
opTilda             = cppToken Op_Tilda
opNot               = cppToken Op_Not
opAssign            = cppToken Op_Assign
opLess              = cppToken Op_Less
opGreater           = cppToken Op_Greater
opAssignPlus        = cppToken Op_AssignPlus
opAssignMinus       = cppToken Op_AssignMinus
opAssignMul         = cppToken Op_AssignMul
opAssignDiv         = cppToken Op_AssignDiv
opAssignRem         = cppToken Op_AssignRem
opAssignXor         = cppToken Op_AssignXor
opAssignAnd         = cppToken Op_AssignAnd
opAssignOr          = cppToken Op_AssignOr
opLeftShift         = cppToken Op_LeftShift
opRightShift        = cppToken Op_RightShift
opAssignLeftShift   = cppToken Op_AssignLeftShift
opAssignRightShift  = cppToken Op_AssignRightShift
opEq                = cppToken Op_Eq
opNotEq             = cppToken Op_NotEq
opLessEq            = cppToken Op_LessEq
opGreaterEq         = cppToken Op_GreaterEq
opLogicalAnd        = cppToken Op_LogicalAnd
opLogicalOr         = cppToken Op_LogicalOr
opIncrement         = cppToken Op_Increment
opDecrement         = cppToken Op_Decrement
comma               = cppToken Punc_Comma
opArrowPtr          = cppToken Op_Arrow
opArrow             = cppToken Op_ArrowPtr

ppIf                = cppToken PP_If
ppIfdef             = cppToken PP_Ifdef
ppIfndef            = cppToken PP_Ifndef
ppElif              = cppToken PP_Elif
ppElse              = cppToken PP_Else
ppEndif             = cppToken PP_Endif
ppInclude           = cppToken PP_Include
ppDefine            = cppToken PP_Define
ppUndef             = cppToken PP_Undef
ppLine              = cppToken PP_Line
ppError             = cppToken PP_Error
ppPragma            = cppToken PP_Pragma

eol                 = cppToken EOL

integerLiteral      = cppToken' (\t -> case t of
                                        Literal_Integer s     -> Just s
                                        _                     -> Nothing)
characterLiteral    = cppToken' (\t -> case t of
                                        Literal_Char s        -> Just s
                                        _                     -> Nothing)
floatingLiteral     = cppToken' (\t -> case t of
                                        Literal_Float s       -> Just s
                                        _                     -> Nothing)
stringLiteral       = cppToken' (\t -> case t of
                                        Literal_String s      -> Just s
                                        _                     -> Nothing)
booleanLiteral      = cppToken' (\t -> case t of
                                        Literal_Boolean s     -> Just s
                                        _                     -> Nothing)
pointerLiteral      = cppToken' (\t -> case t of
                                        Literal_NullPtr s     -> Just s
                                        _                     -> Nothing)
userDefinedLiteral  = cppToken' (\t -> case t of
                                        Literal_UserDefined s -> Just s
                                        _                     -> Nothing)

ident = cppToken' (\t -> case t of
                            Id s -> Just s
                            _    -> Nothing)

bcppToken = cppToken' (\t -> case t of
                                Punc_LeftBrace    -> Nothing
                                Punc_RightBrace   -> Nothing
                                Punc_LeftBracket  -> Nothing
                                Punc_RightBracket -> Nothing
                                Punc_LeftParen    -> Nothing
                                Punc_RightParen   -> Nothing
                                _                 -> Just t
                      )

}