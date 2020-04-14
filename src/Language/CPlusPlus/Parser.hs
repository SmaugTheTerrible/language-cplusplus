module Language.CPlusPlus.Parser where

import           Language.CPlusPlus.AST
import           Language.CPlusPlus.Internal.Base
import           Language.CPlusPlus.Internal.Lexer
                                         hiding ( pos )
import           Language.CPlusPlus.Internal.Types.Lexer

import           Text.Parsec             hiding ( parse )

-- https://www.nongnu.org/hcb/
-- Hyperlinked C++ BNF Grammar
-- By Alessio Marchetti
--
-- Version 3.2
--
-- Last updated: 12-Feb-2016
-- BNF Grammar Rules
--
-- basic.link
-- translation-unit:
--  	declaration-seq[opt]
translationUnit :: P TranslationUnit
translationUnit = TU <$> option [] declarationSeq <?> "translation unit"

-- expr.prim.general
-- primary-expression:
--  	literal
--  	this
--  	( expression )
--  	id-expression
--  	lambda-expression     C++0x
-- id-expression:
--  	unqualified-id
--  	qualified-id

primaryExpression :: P Expression
primaryExpression =
  do
      let literalExpr  = LiteralExpression <$> pos <*> literal
      let thisExpr     = kwThis >> ThisExpression <$> pos
      let parensedExpr = parens expression
      choice
        [literalExpr, thisExpr, parensedExpr, idExpression, lambdaExpression]
    <?> "primary expression"

idExpression :: P Expression
idExpression =
  IdExpression
    <$> pos
    <*> (Left <$> unqualifiedId <|> Right <$> qualifiedId)
    <?> "id expression"

-- unqualified-id:
--  	identifier
--  	operator-function-id
--  	conversion-function-id
--  	literal-operator-id     C++0x
--  	~ class-name
--  	~ decltype-specifier     C++0x
--  	template-id
unqualifiedId :: P UnqualifiedId
unqualifiedId =
  do
      let idIdentifier = UnqualifiedIdIdentifier <$> pos <*> identifier
      let operatorFunctionId' =
            UnqualifiedIdOperatorFunctionId <$> pos <*> operatorFunctionId
      let conversionFunctionId' =
            UnqualifiedIdConversionFunctionId <$> pos <*> conversionFunctionId
      let literalOperatorId' =
            UnqualifiedIdLiteralOperatorId <$> pos <*> literalOperatorId
      let destructorClass =
            UnqualifiedIdDestructorClass <$> pos <*> try (opTilda >> className)
      let destructorDecltype =
            UnqualifiedIdDestructorDecltype
              <$> pos
              <*> (opTilda >> decltypeSpecifier)
      let templateId' = UnqualifiedIdTemplateId <$> pos <*> templateId
      choice
        [ idIdentifier
        , operatorFunctionId'
        , conversionFunctionId'
        , literalOperatorId'
        , destructorClass
        , destructorDecltype
        , templateId'
        ]
    <?> "unqualified id"

-- qualified-id:
--  	::[opt] nested-name-specifier template[opt] unqualified-id
--  	:: identifier
--  	:: operator-function-id
--  	:: literal-operator-id     C++0x
--  	:: template-id
qualifiedId :: P QualifiedId
qualifiedId =
  QualifiedIdNestedId
    <$> pos
    <*> optionBool doubleColon
    <*> nestedNameSpecifier
    <*> optionBool kwTemplate
    <*> unqualifiedId
    <?> "qualified id"

-- nested-name-specifier:
--  	type-name ::
--  	namespace-name ::
--  	decltype-specifier ::     C++0x
--  	nested-name-specifier identifier ::
--  	nested-name-specifier template[opt] simple-template-id ::
nestedNameSpecifier :: P NestedNameSpecifier
nestedNameSpecifier =
  do
      let nameSpecifier = try $ do
            pos <- pos
            n   <- typeName
            doubleColon
            pure $ NestedNameSpecifierType pos n
      let namespaceSpecifier = try $ do
            pos <- pos
            n   <- namespaceName
            doubleColon
            pure $ NestedNameSpecifierNamespace pos n
      let decltypeSpecifier' = try $ do
            pos <- pos
            n   <- decltypeSpecifier
            doubleColon
            pure $ NestedNameSpecifierDecltype pos n
      let idSpecifier = try $ do
            pos <- pos
            n   <- nestedNameSpecifier
            id  <- identifier
            doubleColon
            pure $ NestedNameSpecifierIdentifier pos n id
      let templateSpecifier = do
            pos <- pos
            n   <- nestedNameSpecifier
            b   <- optionBool kwTemplate
            t   <- simpleTemplateId
            doubleColon
            pure $ NestedNameSpecifierTemplate pos n b t
      choice
        [ nameSpecifier
        , namespaceSpecifier
        , decltypeSpecifier'
        , idSpecifier
        , templateSpecifier
        ]
    <?> "nested name specifier"

-- expr.prim.lambda
-- lambda-expression:
--  	lambda-introducer lambda-declarator[opt] compound-statement     C++0x
-- lambda-introducer:
--  	[ lambda-capture[opt] ]     C++0x
-- lambda-capture:
--  	capture-default     C++0x
--  	capture-list     C++0x
--  	capture-default , capture-list     C++0x
-- capture-default:
--  	&     C++0x
--  	=     C++0x
-- capture-list:
--  	capture ...[opt]     C++0x
--  	capture-list , capture ...[opt]     C++0x
-- capture:
--  	identifier     C++0x
--  	& identifier     C++0x
--  	this     C++0x
-- lambda-declarator:
--  	( parameter-declaration-clause ) mutable[opt] exception-specification[opt] attribute-specifier-seq[opt] trailing-return-type[opt]     C++0x
lambdaExpression :: P Expression
lambdaExpression =
  LambdaExpression
    <$> pos
    <*> lambdaIntroducer
    <*> optionMaybe lambdaDeclarator
    <*> compoundStatement
    <?> "lambda expression"

lambdaIntroducer :: P LambdaIntroducer
lambdaIntroducer =
  LambdaIntroducer <$> pos <*> optionMaybe lambdaCapture <?> "lambda introducer"

lambdaCapture :: P LambdaCapture
lambdaCapture = do
  let captureDefault' = LambdaCaptureDefault <$> pos <*> captureDefault
  let captureList'    = LambdaCaptureList <$> pos <*> captureList
  let captureDefaultAndList = do
        pos <- pos
        d   <- captureDefault
        comma
        LambdaCaptureDefaultAndList pos d <$> captureList
  choice [captureDefaultAndList, captureDefault', captureList']

captureDefault :: P CaptureDefault
captureDefault =
  CaptureDefault
    <$> pos
    <*> ((opAnd >> pure CaptureByRef) <|> (opAssign >> pure CaptureByValue))
    <?> "capture default"

captureList :: P CaptureList
captureList =
  CaptureList
    <$> pos
    <*> sepBy1 capture comma
    <*> optionBool threeDot
    <?> "capture list"

capture :: P Capture
capture = do
  let idCapture   = undefined
  let refCapture  = undefined
  let thisCapture = undefined
  choice [idCapture, refCapture, thisCapture] <?> "capture"

lambdaDeclarator :: P LambdaDeclarator
lambdaDeclarator =
  LambdaDeclarator
    <$> pos
    <*> parens parameterDeclarationClause
    <*> optionBool kwMutable
    <*> optionMaybe exceptionSpecification
    <*> option [] attributeSpecifierSeq
    <*> optionMaybe trailingReturnType
    <?> "lambda declarator"

-- expr.post
-- postfix-expression:
--  	primary-expression
--  	postfix-expression [ expression ]
--  	postfix-expression [ braced-init-list[opt] ]     C++0x
--  	postfix-expression ( expression-list[opt] )
--  	simple-type-specifier ( expression-list[opt] )
--  	typename-specifier ( expression-list[opt] )
--  	simple-type-specifier braced-init-list     C++0x
--  	typename-specifier braced-init-list     C++0x
--  	postfix-expression . template[opt] id-expression
--  	postfix-expression -> template[opt] id-expression
--  	postfix-expression . pseudo-destructor-name
--  	postfix-expression -> pseudo-destructor-name
--  	postfix-expression ++
--  	postfix-expression --
--  	dynamic_cast < type-id > ( expression )
--  	static_cast < type-id > ( expression )
--  	reinterpret_cast < type-id > ( expression )
--  	const_cast < type-id > ( expression )
--  	typeid ( expression )
--  	typeid ( type-id )
-- expression-list:
--  	initializer-list
postfixExpression :: P Expression
postfixExpression = do
--  	postfix-expression [ expression ]
  let getByIndex =
        GetByIndexExpression
          <$> pos
          <*> postfixExpression
          <*> brackets expression
--  	postfix-expression [ braced-init-list[opt] ]     C++0x
  let getByBraced =
        GetByBracedExpression <$> pos <*> postfixExpression <*> brackets
          (optionMaybe bracedInitList)
--  	postfix-expression ( expression-list[opt] )
  let callExpr = CallExpression <$> pos <*> postfixExpression <*> parens
        (optionMaybe expressionList)
--  	simple-type-specifier ( expression-list[opt] )
  let simpleTypeCall =
        SimpleTypeCallExpression <$> pos <*> simpleTypeSpecifier <*> parens
          (optionMaybe expressionList)
--  	typename-specifier ( expression-list[opt] )
  let typenameCall =
        TypenameCallExpression <$> pos <*> typeSpecifier <*> parens
          (optionMaybe expressionList)
--  	simple-type-specifier braced-init-list     C++0x
  let simpleTypeBraced =
        SimpleTypeWithBracedExpression
          <$> pos
          <*> simpleTypeSpecifier
          <*> bracedInitList
--  	typename-specifier braced-init-list     C++0x
  let typenameBraced =
        TypenameWithBracedExpression
          <$> pos
          <*> typenameSpecifier
          <*> bracedInitList
--  	postfix-expression . template[opt] id-expression
  let refField =
        GetFromRefExpression
          <$> pos
          <*> postfixExpression
          <*> (dot >> optionBool kwTemplate)
          <*> idExpression
--  	postfix-expression -> template[opt] id-expression
  let ptrField =
        GetFromPtrExpression
          <$> pos
          <*> postfixExpression
          <*> (opArrow >> optionBool kwTemplate)
          <*> idExpression
--  	postfix-expression . pseudo-destructor-name
  let refDestrExpr =
        GetDestructorFromRefExpression
          <$> pos
          <*> postfixExpression
          <*> (dot >> pseudoDestructorName)
--  	postfix-expression -> pseudo-destructor-name
  let ptrDestrExpr =
        GetDestructorFromPtrExpression
          <$> pos
          <*> postfixExpression
          <*> (opArrow >> pseudoDestructorName)
--  	postfix-expression ++
  let postfixIncrExpr = do
        pos  <- pos
        expr <- postfixExpression
        opIncrement
        pure $ PostIncrementExpression pos expr
--  	postfix-expression --
  let postfixDecrExpr = do
        pos  <- pos
        expr <- postfixExpression
        opDecrement
        pure $ PostDecrementExpression pos expr
--  	dynamic_cast < type-id > ( expression )
  let dynamicCast = do
        pos <- pos
        kwDynamicCast
        t    <- angles typeId
        expr <- parens expression
        pure $ DynamicCastExpression pos t expr
--  	static_cast < type-id > ( expression )
  let staticCast = do
        pos <- pos
        kwStaticCast
        t    <- angles typeId
        expr <- parens expression
        pure $ StaticCastExpression pos t expr
--  	reinterpret_cast < type-id > ( expression )
  let reinterCast = do
        pos <- pos
        kwReinterpretCast
        t    <- angles typeId
        expr <- parens expression
        pure $ ReinterpretCastExpression pos t expr
--  	const_cast < type-id > ( expression )
  let constCast = do
        pos <- pos
        kwConstCast
        t    <- angles typeId
        expr <- parens expression
        pure $ ConstCastExpression pos t expr
--  	typeid ( expression )
--  	typeid ( type-id )
  let typeIdExpr = do
        pos <- pos
        t   <- typeId
        arg <- parens ((Left <$> expression) <|> (Right <$> typeId))
        pure $ TypeIdExpression pos t arg
  choice
      [ primaryExpression
      , getByIndex
      , getByBraced
      , callExpr
      , simpleTypeCall
      , typenameCall
      , simpleTypeBraced
      , typenameBraced
      , refField
      , ptrField
      , refDestrExpr
      , ptrDestrExpr
      , postfixIncrExpr
      , postfixDecrExpr
      , dynamicCast
      , staticCast
      , reinterCast
      , constCast
      , typeIdExpr
      ]
    <?> "postfix expression"

expressionList :: P ExpressionList
expressionList =
  ExpressionList <$> pos <*> initializerList <?> "expression list"

-- pseudo-destructor-name:
--  	::opt nested-name-specifier[opt] type-name :: ~ type-name
--  	::opt nested-name-specifier template simple-template-id :: ~ type-name     C++0x
--  	::opt nested-name-specifier[opt] ~ type-name
--  	~ decltype-specifier     C++0x
pseudoDestructorName :: P PseudoDestructorName
pseudoDestructorName = do
  let nested =
        PseudoDestructorNameNested
          <$> pos
          <*> optionBool doubleColon
          <*> optionMaybe nestedNameSpecifier
          <*> typeName
          <*> (doubleColon >> opTilda >> typeName)
  let temp =
        PseudoDestructorNameTemplate
          <$> pos
          <*> optionBool doubleColon
          <*> nestedNameSpecifier
          <*> simpleTemplateId
          <*> (doubleColon >> opTilda >> typeName)
  let typeName' =
        PseudoDestructorNameTypeName
          <$> pos
          <*> optionBool doubleColon
          <*> optionMaybe nestedNameSpecifier
          <*> (opTilda >> typeName)
  let decltype =
        PseudoDestructorNameDecltype <$> pos <*> (opTilda >> decltypeSpecifier)
  choice [nested, temp, typeName', decltype] <?> "pseudo destructor name"

-- expr.unary
-- unary-expression:
--  	postfix-expression
--  	++ cast-expression
--  	-- cast-expression
--  	unary-operator cast-expression
--  	sizeof unary-expression
--  	sizeof ( type-id )
--  	sizeof ... ( identifier )     C++0x
--  	alignof ( type-id )     C++0x
--  	noexcept-expression     C++0x
--  	new-expression
--  	delete-expression
-- unary-operator:
--  	*
--  	&
--  	+
--  	-
--  	!
--  	~
unaryExpression :: P Expression
unaryExpression = do
  let preIncr =
        PrefIncrementExpression <$> pos <*> (opIncrement >> castExpression)
  let preDecr =
        PrefDecrementExpression <$> pos <*> (opDecrement >> castExpression)
  let unary =
        UnaryOperationExpression <$> pos <*> unaryOperator <*> castExpression
  let sizeofExpr = SizeOfExpression <$> pos <*> (kwSizeof >> unaryExpression)
  let sizeofType = SizeOfTypeExpression <$> pos <*> (kwSizeof >> parens typeId)
  let sizeofThreeDot =
        SizeOfThreeDottedExpression
          <$> pos
          <*> (kwSizeof >> threeDot >> parens identifier)
  choice
      [ postfixExpression
      , preIncr
      , preDecr
      , unary
      , sizeofExpr
      , sizeofType
      , sizeofThreeDot
      , alignmentExpression
      , noexceptExpression
      , newExpression
      , deleteExpression
      ]
    <?> "unary expression"

unaryOperator :: P UnaryOperator
unaryOperator =
  UnaryOperator
    <$> pos
    <*> choice
          [ (opMul >> pure Ptr)
          , (opAnd >> pure Ref)
          , (opPlus >> pure UnaryPlus)
          , (opMinus >> pure UnaryMinus)
          , (opNot >> pure Not)
          , (opTilda >> pure Tilda)
          ]
    <?> "unary operator"

alignmentExpression :: P Expression
alignmentExpression =
  AlignOfExpression
    <$> pos
    <*> (kwAlignof >> parens typeId)
    <?> "alignment expression"

-- expr.new
-- new-expression:
--  	::opt new new-placement[opt] new-type-id new-initializer[opt]
--  	::opt new new-placement[opt] ( type-id ) new-initializer[opt]
-- new-placement:
--  	( expression-list )
-- new-type-id:
--  	type-specifier-seq new-declarator[opt]
-- new-declarator:
--  	ptr-operator new-declarator[opt]
--  	noptr-new-declarator     C++0x
-- noptr-new-declarator:
--  	[ expression ] attribute-specifier-seq[opt]     C++0x
--  	noptr-new-declarator [ constant-expression ] attribute-specifier-seq[opt]     C++0x
-- new-initializer:
--  	( expression-list[opt] )
--  	braced-init-list     C++0x
newExpression :: P Expression
newExpression = do
  let new =
        NewExpression
          <$> pos
          <*> optionBool doubleColon
          <*> (kwNew >> optionMaybe newPlacement)
          <*> newTypeId
          <*> optionMaybe newInitializer
  let newParensed =
        NewParensedExpression
          <$> pos
          <*> optionBool doubleColon
          <*> (kwNew >> optionMaybe newPlacement)
          <*> parens typeId
          <*> optionMaybe newInitializer
  try new <|> newParensed <?> "new expression"

newPlacement :: P ExpressionList
newPlacement = parens expressionList <?> "new placement"

newTypeId :: P NewTypeId
newTypeId =
  NewTypeId
    <$> pos
    <*> typeSpecifierSeq
    <*> optionMaybe newDeclarator
    <?> "new type id"

newDeclarator :: P NewDeclarator
newDeclarator = do
  let newPtr =
        NewDeclaratorPtr <$> pos <*> ptrOperator <*> optionMaybe newDeclarator
  let newNoPtr = NewDeclaratorNoptr <$> pos <*> noptrNewDeclarator
  newPtr <|> newNoPtr <?> "new declarator"

noptrNewDeclarator :: P NoptrNewDeclarator
noptrNewDeclarator = do
  let common =
        NoptrNewDeclarator
          <$> pos
          <*> brackets expression
          <*> option [] attributeSpecifierSeq
  let prefixed =
        NoptrNewDeclaratorPrefixed
          <$> pos
          <*> noptrNewDeclarator
          <*> brackets constantExpression
          <*> option [] attributeSpecifierSeq
  common <|> prefixed <?> "noptr new declarator"

newInitializer :: P NewInitializer
newInitializer = do
  let exprList = NewInitializerExpressionList <$> pos <*> parens
        (optionMaybe expressionList)
  let bracedList = NewInitializerBracedList <$> pos <*> bracedInitList
  exprList <|> bracedList <?> "new initializer"

-- expr.delete
-- delete-expression:
--  	::opt delete cast-expression
--  	::opt delete [ ] cast-expression
deleteExpression :: P Expression
deleteExpression = do
  let common =
        DeleteExpression
          <$> pos
          <*> optionBool doubleColon
          <*> (kwDelete >> castExpression)
  let delArray =
        DeleteArrayExpression
          <$> pos
          <*> optionBool doubleColon
          <*> (kwDelete >> leftBracket >> rightBracket >> castExpression)
  try common <|> delArray <?> "delete expression"

-- expr.unary.noexcept
-- noexcept-expression:
--  	noexcept ( expression )     C++0x
noexceptExpression :: P Expression
noexceptExpression =
  NoexceptExpression <$> pos <*> (kwNoexcept >> parens expression)

-- expr.cast
-- cast-expression:
--  	unary-expression
--  	( type-id ) cast-expression
castExpression :: P Expression
castExpression = do
  let cast = CastExpression <$> pos <*> parens typeId <*> castExpression
  unaryExpression <|> cast <?> "cast expression"

-- expr.mptr.oper
-- pm-expression:
--  	cast-expression
--  	pm-expression .* cast-expression
--  	pm-expression ->* cast-expression
pmExpression :: P Expression
pmExpression = do
  let fromRef =
        GetPtrFromRefExpression <$> pos <*> pmExpression <*> castExpression
  let fromPtr =
        GetPtrFromPtrExpression <$> pos <*> pmExpression <*> castExpression
  choice [castExpression, try fromRef, fromPtr] <?> "pm expression"

-- expr.mul
-- multiplicative-expression:
--  	pm-expression
--  	multiplicative-expression * pm-expression
--  	multiplicative-expression / pm-expression
--  	multiplicative-expression % pm-expression
multiplicativeExpression :: P Expression
multiplicativeExpression = do
  let mul = binaryOperationExpression (opMul >> pure Multiply)
                                      multiplicativeExpression
                                      pmExpression
  let div = binaryOperationExpression (opDiv >> pure Divide)
                                      multiplicativeExpression
                                      pmExpression
  let rem = binaryOperationExpression (opRem >> pure Remain)
                                      multiplicativeExpression
                                      pmExpression
  choice [pmExpression, try mul, try div, rem] <?> "multiplicative expression"

binaryOperationExpression
  :: P BinaryOperatorType -> P Expression -> P Expression -> P Expression
binaryOperationExpression op left right = do
  pos <- pos
  l   <- left
  o   <- BinaryOperator <$> getPosition <*> op
  BinaryOperationExpression pos o l <$> right

-- expr.add
-- additive-expression:
--  	multiplicative-expression
--  	additive-expression + multiplicative-expression
--  	additive-expression - multiplicative-expression
additiveExpression :: P Expression
additiveExpression = do
  let plus = binaryOperationExpression (opPlus >> pure Plus)
                                       additiveExpression
                                       multiplicativeExpression
  let minus = binaryOperationExpression (opMinus >> pure Minus)
                                        additiveExpression
                                        multiplicativeExpression
  choice [multiplicativeExpression, try plus, minus] <?> "additive expression"

-- expr.shift
-- shift-expression:
--  	additive-expression
--  	shift-expression << additive-expression
--  	shift-expression >> additive-expression
shiftExpression :: P Expression
shiftExpression = do
  let leftSh = binaryOperationExpression (opLeftShift >> pure LeftShift)
                                         shiftExpression
                                         additiveExpression
  let rightSh = binaryOperationExpression (opRightShift >> pure RightShift)
                                          shiftExpression
                                          additiveExpression
  choice [additiveExpression, try leftSh, rightSh] <?> "shift expression"

-- expr.rel
-- relational-expression:
--  	shift-expression
--  	relational-expression < shift-expression
--  	relational-expression > shift-expression
--  	relational-expression <= shift-expression
--  	relational-expression >= shift-expression
relationalExpression :: P Expression
relationalExpression = do
  let rel p = binaryOperationExpression p relationalExpression shiftExpression
  let less    = rel (opLess >> pure Lesser)
  let great   = rel (opGreater >> pure Greater)
  let lessEq  = rel (opLessEq >> pure LesserEqual)
  let greatEq = rel (opGreaterEq >> pure GreaterEqual)
  choice [shiftExpression, try less, try great, try lessEq, greatEq]
    <?> "relational expression"

-- expr.eq
-- equality-expression:
--  	relational-expression
--  	equality-expression == relational-expression
--  	equality-expression != relational-expression
equalityExpression :: P Expression
equalityExpression = do
  let eq = binaryOperationExpression (opEq >> pure Equal)
                                     equalityExpression
                                     relationalExpression
  let notEq = binaryOperationExpression (opNotEq >> pure NotEqual)
                                        equalityExpression
                                        relationalExpression
  choice [relationalExpression, try eq, notEq] <?> "equality expression"

-- expr.bit.and
-- and-expression:
--  	equality-expression
--  	and-expression & equality-expression
andExpression :: P Expression
andExpression = do
  let bitand = binaryOperationExpression (opAnd >> pure BitAnd)
                                         andExpression
                                         equalityExpression
  choice [equalityExpression, bitand] <?> "bit and expression"

-- expr.xor
-- exclusive-or-expression:
--  	and-expression
--  	exclusive-or-expression ^ and-expression
exclusiveOrExpression :: P Expression
exclusiveOrExpression = do
  let xor = binaryOperationExpression (opOr >> pure BitXor)
                                      exclusiveOrExpression
                                      andExpression
  choice [andExpression, xor] <?> "xor expression"

-- expr.or
-- inclusive-or-expression:
--  	exclusive-or-expression
--  	inclusive-or-expression | exclusive-or-expression
inclusiveOrExpression :: P Expression
inclusiveOrExpression = do
  let orExpr = binaryOperationExpression (opOr >> pure BitOr)
                                         inclusiveOrExpression
                                         exclusiveOrExpression
  choice [exclusiveOrExpression, orExpr] <?> "bit or expression"

-- expr.log.and
-- logical-and-expression:
--  	inclusive-or-expression
--  	logical-and-expression && inclusive-or-expression
logicalAndExpression :: P Expression
logicalAndExpression = do
  let andExpr = binaryOperationExpression (opLogicalAnd >> pure LogicalAnd)
                                          logicalAndExpression
                                          inclusiveOrExpression
  choice [inclusiveOrExpression, andExpr] <?> "logical and expression"

-- expr.log.or
-- logical-or-expression:
--  	logical-and-expression
--  	logical-or-expression || logical-and-expression
logicalOrExpression :: P Expression
logicalOrExpression = do
  let orExpr = binaryOperationExpression (opLogicalOr >> pure LogicalOr)
                                         logicalOrExpression
                                         logicalAndExpression
  choice [logicalAndExpression, orExpr] <?> "logical or expression"

-- expr.cond
-- conditional-expression:
--  	logical-or-expression
--  	logical-or-expression ? expression : assignment-expression
conditionalExpression :: P Expression
conditionalExpression = do
  let cond =
        ConditionalExpression
          <$> pos
          <*> logicalOrExpression
          <*> (questionMark >> expression)
          <*> (colon >> assignmentExpression)
  choice [logicalOrExpression, cond] <?> "conditional expression"

-- expr.ass
-- assignment-expression:
--  	conditional-expression
--  	logical-or-expression assignment-operator initializer-clause     C++0x
--  	throw-expression
-- assignment-operator:
--  	=
--  	*=
--  	/=
--  	%=
--  	+=
--  	-=
--  	>>=
--  	<<=
--  	&=
--  	^=
---  	|=
assignmentExpression :: P Expression
assignmentExpression = do
  let assign =
        AssignmentExpression
          <$> pos
          <*> logicalOrExpression
          <*> assignmentOperator
          <*> initializerClause
  choice [conditionalExpression, assign, throwExpression]
    <?> "assignment expression"

assignmentOperator :: P AssignmentOperator
assignmentOperator = do
  let operator p = AssignmentOperator <$> pos <*> p
  let assign           = operator $ opAssign >> pure Assign
  let assignMul        = operator $ opAssignMul >> pure AssignMultiply
  let assignDiv        = operator $ opAssignDiv >> pure AssignDivision
  let assignRem        = operator $ opAssignRem >> pure AssignRemain
  let assignPlus       = operator $ opAssignPlus >> pure AssignAdd
  let assignMinus      = operator $ opAssignMinus >> pure AssignSubstract
  let assignRightShift = operator $ opAssignRightShift >> pure AssignRightShift
  let assignLeftShift  = operator $ opAssignLeftShift >> pure AssignLeftShift
  let assignAnd        = operator $ opAssignAnd >> pure AssignAnd
  let assignXor        = operator $ opAssignXor >> pure AssignXor
  let assignOr         = operator $ opAssignOr >> pure AssignOr
  choice
      [ assign
      , assignMul
      , assignDiv
      , assignRem
      , assignPlus
      , assignMinus
      , assignRightShift
      , assignLeftShift
      , assignAnd
      , assignXor
      , assignOr
      ]
    <?> "assignment operator"

-- expr.comma
-- expression:
--  	assignment-expression
--  	expression , assignment-expression
expression :: P Expression
expression = do
  let commaExpr =
        CommaExpression
          <$> pos
          <*> expression
          <*> (comma >> assignmentExpression)
  choice [try assignmentExpression, commaExpr] <?> "comma expression"

-- expr.const
-- constant-expression:
--  	conditional-expression
constantExpression :: P Expression
constantExpression = conditionalExpression <?> "constant expression"

-- stmt.stmt
-- statement:
--  	labeled-statement
--  	attribute-specifier-seq[opt] expression-statement     C++0x
--  	attribute-specifier-seq[opt] compound-statement     C++0x
--  	attribute-specifier-seq[opt] selection-statement     C++0x
--  	attribute-specifier-seq[opt] iteration-statement     C++0x
--  	attribute-specifier-seq[opt] jump-statement     C++0x
--  	declaration-statement
--  	attribute-specifier-seq[opt] try-block
statement :: P Statement
statement =
  choice
      [ labeledStatement
      , expressionStatement
      , compoundStatement
      , selectionStatement
      , iterationStatement
      , jumpStatement
      , declarationStatement
--  , tryStatement
      ]
    <?> "statement"

-- stmt.label
-- labeled-statement:
--  	attribute-specifier-seq[opt] identifier : statement
--  	attribute-specifier-seq[opt] case constant-expression : statement
--  	attribute-specifier-seq[opt] default : statement
labeledStatement :: P Statement
labeledStatement = do
  let labeled =
        LabeledStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> identifier
          <*> (colon >> statement)
  let caseSt =
        LabeledCaseStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwCase >> expression)
          <*> (colon >> statement)
  let defaultSt =
        LabeledDefaultStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwDefault >> colon >> statement)
  choice [labeled, caseSt, defaultSt] <?> "labeled statement"

-- stmt.expr
-- expression-statement:
--  	expression[opt] ;
expressionStatement :: P Statement
expressionStatement =
  ExpressionStatement
    <$> pos
    <*> option [] attributeSpecifierSeq
    <*> do
          e <- optionMaybe expression
          semi
          pure e
    <?> "expression statement"


-- stmt.block
-- compound-statement:
--  	{ statement-seq[opt] }
-- statement-seq:
--  	statement
--  	statement-seq statement
compoundStatement :: P Statement
compoundStatement =
  CompoundStatement
    <$> pos
    <*> option [] attributeSpecifierSeq
    <*> braces (option [] statementSeq)
    <?> "compound statement"

statementSeq :: P [Statement]
statementSeq = many1 statement

-- stmt.select
-- selection-statement:
--  	if ( condition ) statement
--  	if ( condition ) statement else statement
--  	switch ( condition ) statement
-- condition:
--  	expression
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator braced-init-list     C++0x
selectionStatement :: P Statement
selectionStatement = do
  let ifStat =
        IfStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwIf >> parens condition)
          <*> statement
  let ifElseStat =
        IfElseStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwIf >> parens condition)
          <*> statement
          <*> (kwElse >> statement)
  let switchStat =
        SwitchStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> parens condition
          <*> statement
  choice [try ifElseStat, ifStat, switchStat] <?> "selection statement"

condition :: P Condition
condition = do
  let exprCond = ExpressionCondition <$> pos <*> expression
  let assignCond =
        InitializerCondition
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> declSpecifierSeq
          <*> declarator
          <*> (opAssign >> initializerClause)
  let bracedInitListCond =
        InitBracedCondition
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> declSpecifierSeq
          <*> declarator
          <*> bracedInitList
  choice [try exprCond, try assignCond, try bracedInitListCond] <?> "condition"

-- stmt.iter
-- iteration-statement:
--  	while ( condition ) statement
--  	do statement while ( expression ) ;
--  	for ( for-init-statement condition[opt] ; expression[opt] ) statement
--  	for ( for-range-declaration : for-range-initializer ) statement     C++0x
iterationStatement :: P Statement
iterationStatement = do
  let whileStat =
        WhileStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwWhile >> parens condition)
          <*> statement
  let doWhileStat =
        DoWhileStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwDo >> statement)
          <*> do
                kwWhile
                e <- parens expression
                semi
                pure e
  let forStat =
        ForStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwFor >> leftParen >> forInitStatement)
          <*> optionMaybe condition
          <*> (semi >> optionMaybe expression)
          <*> (rightParen >> statement)
  let forRange =
        ForRangeStatement
          <$> pos
          <*> option [] attributeSpecifierSeq
          <*> (kwFor >> leftParen >> forRangeDeclaration)
          <*> (colon >> forRangeInitializer)
          <*> (rightParen >> statement)
  choice [whileStat, doWhileStat, forStat, forRange] <?> "iteration statement"

-- for-init-statement:
--  	expression-statement
--  	simple-declaration
-- for-range-declaration:
--  	attribute-specifier-seq[opt] type-specifier-seq declarator     C++0x
-- for-range-initializer:
--  	expression
--    braced-init-list     C++0x
forInitStatement :: P ForInitStatement
forInitStatement =
  ForInitStatement
    <$> pos
    <*> (Left <$> expressionStatement <|> Right <$> simpleDeclaration)
    <?> "for init statement"

forRangeDeclaration :: P ForRangeDeclaration
forRangeDeclaration =
  ForRangeDeclaration
    <$> pos
    <*> option [] attributeSpecifierSeq
    <*> typeSpecifierSeq
    <*> declarator
    <?> "for range declaration"

forRangeInitializer :: P ForRangeInitializer
forRangeInitializer =
  ForRangeInitializer
    <$> pos
    <*> (Left <$> expression <|> Right <$> bracedInitList)
    <?> "for range initializer"

-- stmt.jump
-- jump-statement:
--  	break ;
--  	continue ;
--  	return expression[opt] ;
--  	return braced-init-list[opt] ;     C++0x
--  	goto identifier ;
jumpStatement :: P Statement
jumpStatement = do
  let break = do
        pos   <- pos
        attrs <- option [] attributeSpecifierSeq
        kwBreak
        semi
        pure $ BreakStatement pos attrs
  let continue = do
        pos   <- pos
        attrs <- option [] attributeSpecifierSeq
        kwContinue
        semi
        pure $ ContinueStatement pos attrs
  let returnExpr = do
        pos   <- pos
        attrs <- option [] attributeSpecifierSeq
        kwReturn
        e <- optionMaybe expression
        semi
        pure $ ReturnStatement pos attrs e
  let returnBraced = do
        pos   <- pos
        attrs <- option [] attributeSpecifierSeq
        kwReturn
        b <- optionMaybe bracedInitList
        semi
        pure $ ReturnBracedStatement pos attrs b
  let goto = do
        pos   <- pos
        attrs <- option [] attributeSpecifierSeq
        kwGoto
        i <- identifier
        semi
        pure $ GotoStatement pos attrs i
  choice [try break, try continue, try returnExpr, try returnBraced, goto]
    <?> "jump statement"

-- stmt.dcl
-- declaration-statement:
--  	block-declaration
declarationStatement :: P Statement
declarationStatement =
  DeclarationStatement <$> pos <*> blockDeclaration <?> "declaration statement"

-- dcl.dcl
-- declaration-seq:
--  	declaration
--  	declaration-seq declaration
-- declaration:
--  	block-declaration
--  	function-definition
--  	template-declaration
--  	explicit-instantiation
--  	explicit-specialization
--  	linkage-specification
--  	namespace-definition
--  	empty-declaration     C++0x
--  	attribute-declaration     C++0x
declarationSeq :: P [Declaration]
declarationSeq = many1 declaration <?> "declaration seq"

declaration :: P Declaration
declaration =
  choice
      [ blockDeclaration
      , functionDefinition
      , templateDeclaration
      , explicitInstantiation
      , explicitSpecialization
      , linkageSpecification
      , namespaceDefinition
      , emptyDeclaration
      , attributeDeclaration
      ]
    <?> "declaration"

-- block-declaration:
--  	simple-declaration
--  	asm-definition
--  	namespace-alias-definition
--  	using-declaration
--  	using-directive
--  	static_assert-declaration     C++0x
--  	alias-declaration     C++0x
--  	opaque-enum-declaration     C++0x
blockDeclaration :: P Declaration
blockDeclaration =
  choice
      [ simpleDeclaration
      , asmDefinition
      , namespaceAliasDefinition
      , usingDeclaration
      , usingDirective
      , staticAssertDeclaration
      , aliasDeclaration
      , opaqueEnumDeclaration
      ]
    <?> "block declaration"

-- alias-declaration:
--  	using identifier = type-id ;     C++0x
-- simple-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] init-declarator-list[opt] ;     C++0x
aliasDeclaration :: P Declaration
aliasDeclaration =
  do
      pos <- pos
      kwUsing
      i <- identifier
      opAssign
      t <- typeId
      semi
      pure $ AliasDeclaration pos i t
    <?> "alias declaration"

simpleDeclaration :: P Declaration
simpleDeclaration =
  do
      pos <- pos
      as  <- option [] attributeSpecifierSeq
      ds  <- optionMaybe declSpecifierSeq
      is  <- option [] initDeclaratorList
      semi
      pure $ SimpleDeclaration pos as ds is
    <?> "simple declaration"

-- static_assert-declaration:
--  	static_assert ( constant-expression , string-literal ) ;     C++0x
-- empty-declaration:
--  	;     C++0x
-- attribute-declaration:
--  	attribute-specifier-seq ;     C++0x
staticAssertDeclaration :: P Declaration
staticAssertDeclaration =
  do
      pos <- pos
      kwStaticAssert
      leftParen
      e <- constantExpression
      comma
      s <- stringLiteral'
      rightParen
      semi
      pure $ StaticAssertDeclaration pos e s
    <?> "static assert declaration"

emptyDeclaration :: P Declaration
emptyDeclaration =
  do
      pos <- pos
      semi
      pure $ EmptyDeclaration pos
    <?> "empty declaration"


attributeDeclaration :: P Declaration
attributeDeclaration =
  do
      pos <- pos
      as  <- attributeSpecifierSeq
      semi
      pure $ AttributeDeclaration pos as
    <?> "attribute declaration"

-- dcl.spec
-- decl-specifier:
--  	storage-class-specifier
--  	type-specifier
--  	function-specifier
--  	friend
--  	typedef
--  	constexpr     C++0x
-- decl-specifier-seq:
--  	decl-specifier attribute-specifier-seq[opt]     C++0x
--  	decl-specifier decl-specifier-seq     C++0x
declSpecifier :: P DeclSpecifier
declSpecifier =
  do
      let storage = StorageDeclSpecifier <$> pos <*> storageClassSpecifier
      let typeS     = undefined
      let funcS     = undefined
      let friend    = undefined
      let typedef   = undefined
      let constexpr = undefined
      choice [storage, typeS, funcS, friend, typedef, constexpr]
    <?> "decl specifier"

declSpecifierSeq :: P DeclSpecifierSeq
declSpecifierSeq =
  DeclSpecifierSeq
    <$> pos
    <*> many1 declSpecifier
    <*> option [] attributeSpecifierSeq
    <?> "decl specifier seq"

-- dcl.stc
-- storage-class-specifier:
--  	auto     Removed in C++0x
--  	register
--  	static
--  	thread_local     C++0x
--  	extern
--  	mutable
storageClassSpecifier :: P StorageClassSpecifier
storageClassSpecifier = do
  let auto        = undefined
  let register    = undefined
  let static_     = undefined
  let threadLocal = undefined
  let extern      = undefined
  let mutable     = undefined
  choice [auto, register, static_, threadLocal, extern, mutable]
    <?> "storage class specifier"

-- dcl.fct.spec
-- function-specifier:
--  	inline
--  	virtual
--  	explicit
functionSpecifier :: P FunctionSpecifier
functionSpecifier = do
  let inline   = undefined
  let virtual  = undefined
  let explicit = undefined
  choice [inline, virtual, explicit] <?> "function specifier"

-- dcl.typedef
-- typedef-name:
--  	identifier
typedefName :: P TypedefName
typedefName = undefined

-- dcl.type
-- type-specifier:
--  	trailing-type-specifier
--  	class-specifier
--  	enum-specifier
-- trailing-type-specifier:
--  	simple-type-specifier
--  	elaborated-type-specifier
--  	typename-specifier
--  	cv-qualifier
-- type-specifier-seq:
--  	type-specifier attribute-specifier-seq[opt]     C++0x
--  	type-specifier type-specifier-seq
-- trailing-type-specifier-seq:
--  	trailing-type-specifier attribute-specifier-seq[opt]     C++0x
--  	trailing-type-specifier trailing-type-specifier-seq     C++0x
typeSpecifier :: P TypeSpecifier
typeSpecifier = do
  let trailing = undefined
  let class_   = undefined
  let enum     = undefined
  choice [trailing, class_, enum] <?> "type specifier"

trailingTypeSpecifier :: P TrailingTypeSpecifier
trailingTypeSpecifier = do
  let simple   = undefined
  let elab     = undefined
  let typename = undefined
  let cv       = undefined
  choice [simple, elab, typename, cv] <?> "trailing type specifier"

typeSpecifierSeq :: P TypeSpecifierSeq
typeSpecifierSeq = do
  pos <- pos
  ts  <- many1 typeSpecifier
  as  <- option [] attributeSpecifierSeq
  pure $ TypeSpecifierSeq pos ts as

trailingTypeSpecifierSeq :: P TrailingTypeSpecifierSeq
trailingTypeSpecifierSeq = do
  pos <- pos
  ts  <- many1 trailingTypeSpecifier
  as  <- option [] attributeSpecifierSeq
  pure $ TrailingTypeSpecifierSeq pos ts as

-- dct.type.simple
-- simple-type-specifier:
--  	::opt nested-name-specifier[opt] type-name
--  	::opt nested-name-specifier template simple-template-id
--  	char
--  	char16_t     C++0x
--  	char32_t     C++0x
--  	wchar_t
--  	bool
--  	short
--  	int
--  	long
--  	signed
--  	unsigned
--  	float
--  	double
--  	void
--  	auto     C++0x
--  	decltype-specifier     C++0x
simpleTypeSpecifier :: P SimpleTypeSpecifier
simpleTypeSpecifier = do
  let typeName = undefined
  let template = undefined
  let char     = undefined
  let char16   = undefined
  let char32   = undefined
  let wchar    = undefined
  let bool     = undefined
  let short    = undefined
  let int      = undefined
  let long     = undefined
  let signed   = undefined
  let unsigned = undefined
  let float    = undefined
  let double   = undefined
  let void     = undefined
  let auto     = undefined
  let decltype = undefined
  choice
      [ typeName
      , template
      , char
      , char16
      , char32
      , wchar
      , bool
      , short
      , int
      , long
      , signed
      , unsigned
      , float
      , double
      , void
      , auto
      , decltype
      ]
    <?> "simple type specifier"

-- type-name:
--  	class-name
--  	enum-name
--  	typedef-name
--  	simple-template-id     C++0x
-- decltype-specifier:
--  	decltype ( expression )     C++0x
typeName :: P TypeName
typeName = do
  let class_   = undefined
  let enum     = undefined
  let typedef  = undefined
  let template = undefined
  choice [class_, enum, typedef, template] <?> "type name"

decltypeSpecifier :: P DecltypeSpecifier
decltypeSpecifier = do
  pos <- pos
  kwDecltype
  e <- parens expression
  pure $ DecltypeSpecifier pos e

-- dcl.type.elab
-- elaborated-type-specifier:
--  	class-key attribute-specifier-seq[opt] ::opt nested-name-specifier[opt] identifier
--  	class-key ::opt nested-name-specifier[opt] template[opt] simple-template-id
--  	enum ::opt nested-name-specifier[opt] identifier
elaboratedTypeSpecifier :: P ElaboratedTypeSpecifier
elaboratedTypeSpecifier = do
  let id       = undefined
  let template = undefined
  let enum     = undefined
  choice [id, template, enum] <?> "elaborated type specifier"

-- dcl.enum
-- enum-name:
--  	identifier
-- enum-specifier:
--  	enum-head { enumerator-list[opt] }     C++0x
--  	enum-head { enumerator-list , }     C++0x
-- enum-head:
--  	enum-key attribute-specifier-seq[opt] identifier[opt] enum-base[opt]     C++0x
--  	enum-key attribute-specifier-seq[opt] nested-name-specifier identifier enum-base[opt]     CD0x
-- opaque-enum-declaration:
--  	enum-key attribute-specifier-seq[opt] identifier enum-base[opt] ;     C++0x
-- enum-key:
--  	enum     C++0x
--  	enum class     C++0x
--  	enum struct     C++0x
-- enum-base:
--  	: type-specifier-seq     C++0x
-- enumerator-list:
--  	enumerator-definition     C++0x
--  	enumerator-list , enumerator-definition     C++0x
-- enumerator-definition:
--  	enumerator
--  	enumerator = constant-expression
-- enumerator:
--  	identifier
enumName :: P EnumName
enumName = EnumName <$> pos <*> identifier <?> "enum name"

enumSpecifier :: P EnumSpecifier
enumSpecifier =
  EnumSpecifier <$> pos <*> enumHead <*> enumeratorList <*> optionBool comma

enumHead :: P EnumHead
enumHead = do
  let simple = undefined
  let nested = undefined
  try simple <|> nested <?> "enum head"

opaqueEnumDeclaration :: P Declaration
opaqueEnumDeclaration =
  OpaqueEnumDeclaration
    <$> pos
    <*> enumKey
    <*> option [] attributeSpecifierSeq
    <*> identifier
    <*> do
          e <- optionMaybe enumBase
          semi
          pure e
    <?> "opaque enum declaration"

enumKey :: P EnumKey
enumKey =
  EnumKey
    <$> pos
    <*> (   try (kwEnum >> kwStruct >> pure EnumStruct)
        <|> try (kwEnum >> kwClass >> pure EnumClass)
        <|> (kwEnum >> pure Enum)
        )
    <?> "enum key"

enumBase :: P EnumBase
enumBase = EnumBase <$> pos <*> (colon >> typeSpecifierSeq) <?> "enum base"

enumeratorList :: P [EnumeratorDefinition]
enumeratorList = sepBy1 enumeratorDefinition comma <?> "enumerator list"

enumeratorDefinition :: P EnumeratorDefinition
enumeratorDefinition = do
  let simple   = undefined
  let assigned = undefined
  try simple <|> assigned <?> "enumerator definition"

enumerator :: P Enumerator
enumerator = Enumerator <$> pos <*> identifier

-- namespace.def
-- namespace-name:
--  	original-namespace-name
--  	namespace-alias
-- original-namespace-name:
--  	identifier
-- namespace-definition:
--  	named-namespace-definition
--  	unnamed-namespace-definition
-- named-namespace-definition:
--  	original-namespace-definition
--  	extension-namespace-definition
namespaceName :: P NamespaceName
namespaceName =
  NamespaceName
    <$> pos
    <*> optionEither originalNamespaceName namespaceAlias
    <?> "namespace name"

originalNamespaceName :: P OriginalNamespaceName
originalNamespaceName =
  OriginalNamespaceName <$> pos <*> identifier <?> "original namespace name"

namespaceDefinition :: P Declaration
namespaceDefinition =
  NamespaceDefinition
    <$> pos
    <*> optionEither namedNamespaceDefinition unnamedNamespaceDefinition
    <?> "namespace definition"

namedNamespaceDefinition :: P NamedNamespaceDefinition
namedNamespaceDefinition =
  NamedNamespaceDefinition
    <$> pos
    <*> optionEither originalNamespaceDefinition extensionNamespaceDefinition
    <?> "named namespace definition"

-- original-namespace-definition:
--  	inline[opt] namespace identifier { namespace-body }     C++0x
-- extension-namespace-definition:
--  	inline[opt] namespace original-namespace-name { namespace-body }     C++0xD
-- unnamed-namespace-definition:
--  	inline[opt] namespace { namespace-body }
-- namespace-body:
--  	declaration-seq[opt]
originalNamespaceDefinition :: P OriginalNamespaceDefinition
originalNamespaceDefinition =
  OriginalNamespaceDefinition
    <$> pos
    <*> optionBool kwInline
    <*> (kwNamespace >> identifier)
    <*> braces namespaceBody
    <?> "original namespace definition"

extensionNamespaceDefinition :: P ExtensionNamespaceDefinition
extensionNamespaceDefinition =
  ExtensionNamespaceDefinition
    <$> pos
    <*> optionBool kwInline
    <*> (kwNamespace >> originalNamespaceName)
    <*> braces namespaceBody
    <?> "extention namespace definition"

unnamedNamespaceDefinition :: P UnnamedNamespaceDefinition
unnamedNamespaceDefinition =
  UnnamedNamespaceDefinition
    <$> pos
    <*> optionBool kwInline
    <*> (kwNamespace >> braces namespaceBody)
    <?> "unnamed namespaces definition"

namespaceBody :: P [Declaration]
namespaceBody = option [] declarationSeq <?> "namespace body"

-- namespace.alias
-- namespace-alias:
--  	identifier
-- namespace-alias-definition:
--  	namespace identifier = qualified-namespace-specifier ;
-- qualified-namespace-specifier:
--  	::opt nested-name-specifier[opt] namespace-name
namespaceAlias :: P NamespaceAlias
namespaceAlias = NamespaceAlias <$> pos <*> identifier <?> "namespace alias"

namespaceAliasDefinition :: P Declaration
namespaceAliasDefinition =
  NamespaceAliasDefinition
    <$> pos
    <*> (kwNamespace >> identifier)
    <*> do
          opAssign
          q <- qualifiedNamespaceSpecifier
          semi
          pure q
    <?> "namespace alias definition"

qualifiedNamespaceSpecifier :: P QualifiedNamespaceSpecifier
qualifiedNamespaceSpecifier =
  QualifiedNamespaceSpecifier
    <$> pos
    <*> optionBool doubleColon
    <*> optionMaybe nestedNameSpecifier
    <*> namespaceName
    <?> "qualified namespace specifier"

-- namespace.udecl
-- using-declaration:
--  	using typename[opt] ::opt nested-name-specifier unqualified-id ;
--  	using :: unqualified-id ;
usingDeclaration :: P Declaration
usingDeclaration = undefined

-- namespace.udir
-- using-directive:
--  	attribute-specifier-seq[opt] using namespace ::opt nested-name-specifier[opt] namespace-name ;
usingDirective :: P Declaration
usingDirective =
  UsingDirective
    <$> pos
    <*> option [] attributeSpecifierSeq
    <*> (kwUsing >> kwNamespace >> optionBool doubleColon)
    <*> optionMaybe nestedNameSpecifier
    <*> do
          n <- namespaceName
          semi
          pure n
    <?> "using directive"

-- dcl.asm
-- asm-definition:
--  	asm ( string-literal ) ;
asmDefinition :: P Declaration
asmDefinition =
  AsmDefinition <$> pos <*> parens stringLiteral' <?> "asm definition"

-- dcl.link
-- linkage-specification:
--  	extern string-literal { declaration-seq[opt] }
--  	extern string-literal declaration
linkageSpecification :: P Declaration
linkageSpecification =
  LinkageSpecification
    <$> pos
    <*> (kwExtern >> stringLiteral')
    <*> optionEither declaration (braces declarationSeq)
    <?> "linkage specification"

-- dcl.attr.grammar
-- attribute-specifier-seq:
--  	attribute-specifier     C++0x
--  	attribute-specifier-seq attribute-specifier     C++0x
-- attribute-specifier:
--  	[ [ attribute-list ] ]     C++0x
--  	alignment-specifier     C++0x
attributeSpecifierSeq :: P [AttributeSpecifier]
attributeSpecifierSeq = many1 attributeSpecifier <?> "attribute specifier seq"

attributeSpecifier :: P AttributeSpecifier
attributeSpecifier = do
  let attr = do
        pos <- pos
        ls  <- braces $ braces attributeList
        pure $ AttributeSpecifier pos ls
  let align = AlignmentAttribute <$> pos <*> alignmentSpecifier
  attr <|> align <?> "attribute specifier"

-- alignment-specifier:
--  	alignas ( type-id ...opt )     C++0x
--  	alignas ( alignment-expression ...opt )     C++0x
alignmentSpecifier :: P AlignmentSpecifier
alignmentSpecifier = do
  let
    p = do
      pos <- getPosition
      kwAlignas
      parens
        $   (AlignAsType pos <$> typeId <*> optionBool threeDot)
        <|> (   AlignAsExpression pos
            <$> alignmentExpression
            <*> optionBool threeDot
            )
  p <?> "alignment specifier"

-- attribute-list:
--  	attribute[opt]     C++0x
--  	attribute-list , attribute[opt]     C++0x
--  	attribute ...     C++0x
--  	attribute-list , attribute ...     C++0x
-- attribute:
--  	attribute-token attribute-argument-clause[opt]     C++0x
-- attribute-token:
--  	identifier     C++0x
--  	attribute-scoped-token     C++0x
-- attribute-scoped-token:
--  	attribute-namespace :: identifier     C++0x
-- attribute-namespace:
--  	identifier     C++0x
-- attribute-argument-clause:
--  	( balanced-token-seq )     C++0x
attributeList :: P AttributeList
attributeList =
  AttributeList
    <$> pos
    <*> (attribute `sepBy` comma)
    <*> optionBool threeDot
    <?> "attribute list"

attribute :: P Attribute
attribute =
  Attribute
    <$> pos
    <*> attributeToken
    <*> optionMaybe attributeArgumentClause
    <?> "attribute"

attributeToken :: P AttributeToken
attributeToken =
  AttributeToken
    <$> pos
    <*> optionEither identifier attributeScopedToken
    <?> "attribute token"

attributeScopedToken :: P AttributeScopedToken
attributeScopedToken =
  AttributeScopedToken
    <$> pos
    <*> attributeNamespace
    <*> (doubleColon >> identifier)
    <?> "attribute scoped token"

attributeNamespace :: P AttributeNamespace
attributeNamespace =
  AttributeNamespace <$> pos <*> identifier <?> "attribute namespace"

attributeArgumentClause :: P AttributeArgumentClause
attributeArgumentClause =
  AttributeArgumentClause
    <$> pos
    <*> balancedTokenSeq
    <?> "attribute argument clause"

-- balanced-token-seq:
--  	balanced-token     C++0x
--  	balanced-token-seq balanced-token     C++0x
-- balanced-token:
--  	( balanced-token-seq )     C++0x
--  	[ balanced-token-seq ]     C++0x
--  	{ balanced-token-seq }     C++0x
--  	token     C++0x - except a parenthesis, a bracket, or a brace
balancedTokenSeq :: P [BalancedToken]
balancedTokenSeq = many1 balancedToken

balancedToken :: P BalancedToken
balancedToken = BalancedToken <$> pos <*> bToken <?> "balanced token"

-- dcl.decl
-- init-declarator-list:
--  	init-declarator
--  	init-declarator-list , init-declarator
-- init-declarator:
--  	declarator initializer[opt]
-- declarator:
--  	ptr-declarator     C++0x
--  	noptr-declarator parameters-and-qualifiers trailing-return-type     C++0x
-- ptr-declarator:
--  	noptr-declarator     C++0x
--  	ptr-operator ptr-declarator     C++0x
-- noptr-declarator:
--  	declarator-id attribute-specifier-seq[opt]     C++0x
--  	noptr-declarator parameters-and-qualifiers     C++0x
--  	noptr-declarator [ constant-expression[opt] ] attribute-specifier-seq[opt]     C++0x
--  	( ptr-declarator )     C++0x
initDeclaratorList :: P [InitDeclarator]
initDeclaratorList = initDeclarator `sepBy1` comma <?> "init declarator list"

initDeclarator :: P InitDeclarator
initDeclarator =
  InitDeclarator
    <$> pos
    <*> declarator
    <*> optionMaybe initializer
    <?> "init declarator"

declarator :: P Declarator
declarator = do
  let ptr   = undefined
  let noptr = undefined
  ptr <|> noptr <?> "declarator"

ptrDeclarator :: P PtrDeclarator
ptrDeclarator = do
  let ptr   = undefined
  let noptr = undefined
  ptr <|> noptr <?> "ptr declarator"

noptrDeclarator :: P NoptrDeclarator
noptrDeclarator = do
  let noptr      = undefined
  let parametred = undefined
  let indexed    = undefined
  let parensed   = undefined
  choice [noptr, parametred, indexed, parensed] <?> "noptr declarator"

-- parameters-and-qualifiers:
--  	( parameter-declaration-clause ) attribute-specifier-seq[opt] cv-qualifier-seq[opt] ref-qualifier[opt] exception-specification[opt]     C++0x
-- trailing-return-type:
--  	-> trailing-type-specifier-seq abstract-declarator[opt]     C++0x
-- ptr-operator:
--  	* attribute-specifier-seq[opt] cv-qualifier-seq[opt]     C++0x
--  	& attribute-specifier-seq[opt]     C++0x
--  	&& attribute-specifier-seq[opt]     C++0x
--  	::opt nested-name-specifier * attribute-specifier-seq[opt] cv-qualifier-seq[opt]     C++0x
-- cv-qualifier-seq:
--  	cv-qualifier
--  	cv-qualifier cv-qualifier-seq
-- cv-qualifier:
--  	const
--  	volatile
-- ref-qualifier:
--  	&     C++0x
--  	&&     C++0x
parametersAndQualifiers :: P ParametersAndQualifiers
parametersAndQualifiers =
  ParametersAndQualifiers
    <$> pos
    <*> parens parameterDeclarationClause
    <*> option [] attributeSpecifierSeq
    <*> option [] cvQualifierSeq
    <*> optionMaybe refQualifier
    <*> optionMaybe exceptionSpecification
    <?> "parameters and qualifiers"

trailingReturnType :: P TrailingReturnType
trailingReturnType =
  TrailingReturnType
    <$> pos
    <*> (opArrow >> trailingTypeSpecifierSeq)
    <*> optionMaybe abstractDeclarator
    <?> "trailing return type"

ptrOperator :: P PtrOperator
ptrOperator = do
  let star  = undefined
  let ref   = undefined
  let dref  = undefined
  let nstar = undefined
  choice [star, ref, dref, nstar] <?> "ptr operator"

cvQualifierSeq :: P [CvQualifier]
cvQualifierSeq = many1 cvQualifier <?> "cv qualifier seq"

cvQualifier :: P CvQualifier
cvQualifier =
  (ConstQualifier <$> pos) <|> (VolatileQualifier <$> pos) <?> "cv qualifier"

refQualifier :: P RefQualifier
refQualifier = do
  let ref  = undefined
  let dref = undefined
  ref <|> dref <?> "ref qualifier"

-- declarator-id:
--  	...opt id-expression     C++0x
--  	::opt nested-name-specifier[opt] class-name     C++0x
declaratorId :: P DeclaratorId
declaratorId = do
  let id = undefined
  let cl = undefined
  id <|> cl <?> "declarator id"

-- dcl.name
-- type-id:
--  	type-specifier-seq abstract-declarator[opt]
-- abstract-declarator:
--  	ptr-abstract-declarator     C++0x
--  	noptr-abstract-declarator[opt] parameters-and-qualifiers trailing-return-type     C++0x
--  	...     C++0x
-- ptr-abstract-declarator:
--  	noptr-abstract-declarator     C++0x
--  	ptr-operator ptr-abstract-declarator[opt]     C++0x
-- noptr-abstract-declarator:
--  	noptr-abstract-declarator[opt] parameters-and-qualifiers     C++0x
--  	noptr-abstract-declarator[opt] [ constant-expression ] attribute-specifier-seq[opt]     C++0x
--  	( ptr-abstract-declarator )     C++0x
typeId :: P TypeId
typeId =
  TypeId
    <$> pos
    <*> typeSpecifierSeq
    <*> optionMaybe abstractDeclarator
    <?> "type id"

abstractDeclarator :: P AbstractDeclarator
abstractDeclarator = do
  let ptr   = undefined
  let noptr = undefined
  let dots  = undefined
  ptr <|> noptr <|> dots <?> "abstract declarator"

ptrAbstractDeclarator :: P PtrAbstractDeclarator
ptrAbstractDeclarator = do
  let ptr   = undefined
  let noptr = undefined
  ptr <|> noptr <?> "ptr abstract declarator"

noptrAbstractDeclarator :: P NoptrAbstractDeclarator
noptrAbstractDeclarator = do
  let noptr    = undefined
  let array    = undefined
  let parensed = undefined
  choice [noptr, array, parensed] <?> "noptr abstract declarator"

-- dcl.fct
-- parameter-declaration-clause:
--  	parameter-declaration-list[opt] ...opt
--  	parameter-declaration-list , ...
-- parameter-declaration-list:
--  	parameter-declaration
--  	parameter-declaration-list , parameter-declaration
-- parameter-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq abstract-declarator[opt]     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq abstract-declarator[opt] = initializer-clause     C++0x
parameterDeclarationClause :: P ParameterDeclarationClause
parameterDeclarationClause = do
  let simple = undefined
  let dotted = undefined
  try simple <|> dotted <?> "parameter declaration list"

parameterDeclarationList :: P [ParameterDeclaration]
parameterDeclarationList =
  parameterDeclaration `sepBy1` comma <?> "parameter declaration list"

parameterDeclaration :: P ParameterDeclaration
parameterDeclaration = do
  let param     = undefined
  let inited    = undefined
  let abstract  = undefined
  let absInited = undefined
  choice [param, inited, abstract, absInited] <?> "parameter declaration"

-- dcl.fct.def.general
-- function-definition:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator function-body     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = default ;     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = delete ;     C++0x
-- function-body:
--  	ctor-initializer[opt] compound-statement     C++0x
--  	function-try-block     C++0x
functionDefinition :: P Declaration
functionDefinition = do
  let simple   = undefined
  let default' = undefined
  let delete   = undefined
  choice [simple, default', delete] <?> "function definition"

functionBody :: P FunctionBody
functionBody = do
  let ctor     = undefined
  let tryBlock = undefined
  try ctor <|> tryBlock <?> "Function body"

-- dcl.init
-- initializer:
--  	brace-or-equal-initializer     C++0x
--  	( expression-list )     C++0x
-- brace-or-equal-initializer:
--  	= initializer-clause     C++0x
--  	braced-init-list     C++0x
-- initializer-clause:
--  	assignment-expression     C++0x
--  	braced-init-list     C++0x
-- initializer-list:
--  	initializer-clause ...opt     C++0x
--  	initializer-list , initializer-clause ...opt     C++0x
-- braced-init-list:
--  	{ initializer-list ,opt }     C++0x
--  	{ }     C++0x
initializer :: P Initializer
initializer = do
  let init = undefined
  let bore = undefined
  init <|> bore <?> "initializer"

braceOrEqualInitializer :: P BraceOrEqualInitializer
braceOrEqualInitializer = do
  let equal  = undefined
  let braced = undefined
  equal <|> braced <?> "brace or equal initializer"

initializerClause :: P InitializerClause
initializerClause = do
  let expr = undefined
  let list = undefined
  expr <|> list <?> "initializer clause"

initializerList :: P InitializerList
initializerList =
  InitializerList
    <$> pos
    <*> (initializerClause `sepBy1` comma)
    <*> optionBool threeDot
    <?> "initializer list"

bracedInitList :: P BracedInitList
bracedInitList = do
  let list  = undefined
  let empty = undefined
  try list <|> empty <?> "braced init list"

-- class
-- class-name:
--  	identifier
--  	simple-template-id     C++0x
-- class-specifier:
--  	class-head { member-specification[opt] }
-- class-head:
--  	class-key attribute-specifier-seq[opt] class-head-name class-virt-specifier-seq[opt] base-clause[opt]     C++0x
--  	class-key attribute-specifier-seq[opt] base-clause[opt]     C++0x
-- class-head-name:
--  	nested-name-specifier[opt] class-name     C++0x
-- class-virt-specifier-seq:
--  	class-virt-specifier     C++0x
--  	class-virt-specifier-seq class-virt-specifier     C++0x
-- class-virt-specifier:
--  	final     C++0x
--  	explicit     C++0x
-- class-key:
--  	class
--  	struct
--  	union
className :: P ClassName
className = do
  let simple   = undefined
  let template = undefined
  simple <|> template <?> "class name"

classSpecifier :: P ClassSpecifier
classSpecifier =
  ClassSpecifier
    <$> pos
    <*> classHead
    <*> braces (optionMaybe memberSpecification)
    <?> "class specifier"

classHead :: P ClassHead
classHead = do
  let simple = undefined
  let opaque = undefined
  try simple <|> opaque <?> "class head"

classHeadName :: P ClassHeadName
classHeadName =
  ClassHeadName
    <$> pos
    <*> optionMaybe nestedNameSpecifier
    <*> className
    <?> "class head name"

classVirtSpecifierSeq :: P [ClassVirtSpecifier]
classVirtSpecifierSeq = many1 classVirtSpecifier

classVirtSpecifier :: P ClassVirtSpecifier
classVirtSpecifier =
  ClassVirtSpecifier
    <$> pos
    <*> ((kwExplicit >> pure ClassExplicit) <|> (lexemeFinal >> pure ClassFinal)
        )
    <?> "class virt specifier"

classKey :: P ClassKey
classKey =
  ClassKey
    <$> pos
    <*> (   (kwClass >> pure Class)
        <|> (kwStruct >> pure Struct)
        <|> (kwUnion >> pure Union)
        )
    <?> "class key"

-- class.mem
-- member-specification:
--  	member-declaration member-specification[opt]
--  	access-specifier : member-specification[opt]
-- member-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] member-declarator-list[opt] ;     C++0x
--  	function-definition ;[opt]
--  	using-declaration
--  	static_assert-declaration     C++0x
--  	template-declaration
--  	alias-declaration     C++0x
-- member-declarator-list:
--  	member-declarator
--  	member-declarator-list , member-declarator
-- member-declarator:
--  	declarator virt-specifier-seq[opt] pure-specifier[opt]
--  	declarator virt-specifier-seq[opt] brace-or-equal-initializer[opt]     C++0x
--  	identifier[opt] attribute-specifier-seq[opt] virt-specifier-seq[opt] : constant-expression
-- virt-specifier-seq:
--  	virt-specifier
--  	virt-specifier-seq virt-specifier
-- virt-specifier:
--  	override
--  	final
--  	new
-- pure-specifier:
--  	= 0
memberSpecification :: P MemberSpecification
memberSpecification = do
  let member = undefined
  let access = undefined
  member <|> access <?> "member specification"

memberDeclaration :: P MemberDeclaration
memberDeclaration = do
  let decList  = undefined
  let func     = undefined
  let using    = undefined
  let stAssert = undefined
  let template = undefined
  let alias    = undefined
  choice [decList, func, using, stAssert, template, alias]
    <?> "member declaration"

memberDeclaratorList :: P [MemberDeclarator]
memberDeclaratorList = memberDeclarator `sepBy1` comma

memberDeclarator :: P MemberDeclarator
memberDeclarator = do
  let pure    = undefined
  let braced  = undefined
  let coloned = undefined
  choice [pure, braced, coloned] <?> "member declarator"

virtSpecifierSeq :: P [VirtSpecifier]
virtSpecifierSeq = many1 virtSpecifier

virtSpecifier :: P VirtSpecifier
virtSpecifier =
  VirtSpecifier
    <$> pos
    <*> (   (lexemeOverride >> pure MemberOverride)
        <|> (lexemeFinal >> pure MemberFinal)
        <|> (kwNew >> pure MemberNew)
        )
    <?> "virt specifier"

pureSpecifier :: P PureSpecifier
pureSpecifier = do
  pos <- pos
  opAssign
  i <- integerLiteral
  if i == "0" then pure $ PureSpecifier pos else unexpected "integer literal"

-- class.derived
-- base-clause:
--  	: base-specifier-list
-- base-specifier-list:
--  	base-specifier ...opt     C++0x
--  	base-specifier-list , base-specifier ...opt     C++0x
-- base-specifier:
--  	attribute-specifier-seq[opt] base-type-specifier     C++0x
--  	attribute-specifier-seq[opt] virtual access-specifier[opt] base-type-specifier     C++0x
--  	attribute-specifier-seq[opt] access-specifier virtual[opt] base-type-specifier     C++0x
-- class-or-decltype:
--  	::opt nested-name-specifier[opt] class-name     C++0x
--  	decltype-specifier     C++0x
-- base-type-specifier:
--  	class-or-decltype     C++0x
-- access-specifier:
--  	private
--  	protected
--  	public
baseClause :: P BaseClause
baseClause =
  BaseClause <$> pos <*> (colon >> baseSpecifierList) <?> "base clause"

baseSpecifierList :: P BaseSpecifierList
baseSpecifierList =
  BaseSpecifierList
    <$> pos
    <*> (baseSpecifier `sepBy1` comma)
    <*> optionBool threeDot
    <?> "base specifier list"

baseSpecifier :: P BaseSpecifier
baseSpecifier = do
  let emptyAccess = undefined
  let virtFirst   = undefined
  let accessFirst = undefined
  choice [emptyAccess, virtFirst, accessFirst] <?> "base specifier"

classOrDecltype :: P ClassOrDecltype
classOrDecltype = do
  let _class   = undefined
  let decltype = undefined
  _class <|> decltype <?> "class or decltype"

baseTypeSpecifier :: P BaseTypeSpecifier
baseTypeSpecifier =
  BaseTypeSpecifier <$> pos <*> classOrDecltype <?> "base type specifier"

accessSpecifier :: P AccessSpecifier
accessSpecifier =
  AccessSpecifier
    <$> pos
    <*> (   (kwPublic >> pure Public)
        <|> (kwProtected >> pure Protected)
        <|> (kwPrivate >> pure Private)
        )
    <?> "access specifier"

-- class.conv.fct
-- conversion-function-id:
--  	operator conversion-type-id
-- conversion-type-id:
--  	type-specifier-seq conversion-declarator[opt]
-- conversion-declarator:
--  	ptr-operator conversion-declarator[opt]
conversionFunctionId :: P ConversionFunctionId
conversionFunctionId =
  ConversionFunctionId
    <$> pos
    <*> (kwOperator >> conversionTypeId)
    <?> "conversion function id"

conversionTypeId :: P ConversionTypeId
conversionTypeId =
  ConversionTypeId
    <$> pos
    <*> typeSpecifierSeq
    <*> optionMaybe conversionDeclarator
    <?> "conversion type id"

conversionDeclarator :: P ConversionDeclarator
conversionDeclarator =
  ConversionDeclarator
    <$> pos
    <*> ptrOperator
    <*> optionMaybe conversionDeclarator
    <?> "conversion declarator"

-- class.base.init
-- ctor-initializer:
--  	: mem-initializer-list
-- mem-initializer-list:
--  	mem-initializer ...opt     C++0x
--  	mem-initializer , mem-initializer-list ...opt     C++0x
-- mem-initializer:
--  	mem-initializer-id ( expression-list[opt] )
--  	mem-initializer-id braced-init-list     C++0x
-- mem-initializer-id:
--  	class-or-decltype
--  	identifier
ctorInitializer :: P CtorInitializer
ctorInitializer =
  CtorInitializer
    <$> pos
    <*> (colon >> memInitializerList)
    <?> "ctor initializer"

memInitializerList :: P MemInitializerList
memInitializerList = do
  pos  <- pos
  ms   <- memInitializer `sepBy1` comma
  dots <- optionBool threeDot
  pure $ MemInitializerList pos ms dots

memInitializer :: P MemInitializer
memInitializer = do
  let parensed = undefined
  let braced   = undefined
  try parensed <|> braced <?> "mem initializer"

memInitializerId :: P MemInitializerId
memInitializerId =
  MemInitializerId
    <$> pos
    <*> optionEither classOrDecltype identifier
    <?> "mem initializer id"

-- over.oper
-- operator-function-id:	See C++ Standard Core Language Issue n. 189
--  	operator overloadable-operator
--  	operator overloadable-operator < template-argument-list[opt] >
-- overloadable-operator:	See C++ Standard Core Language Issue n. 189
--  	new
--  	delete
--  	new [ ]
--  	delete [ ]
--  	+
--  	-
--  	*
--  	/
--  	%
--  	^
--  	&
---  	|
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
---  	|=
--  	<<
--  	>>
--  	>>=
--  	<<=
--  	==
--  	!=
--  	<=
--  	>=
--  	&&
---  	||
--  	++
--  	--
--  	,
--  	->*
--  	->
--  	()
--  	[]
operatorFunctionId :: P OperatorFunctionId
operatorFunctionId = do
  let simple   = undefined
  let template = undefined
  try simple <|> template <?> "operator function id"

overloadableOperator :: P OverloadableOperator
overloadableOperator =
  OverloadableOperator
    <$> pos
    <*> overloadableOperatorType
    <?> "overlodable operator"
  where overloadableOperatorType = undefined

-- over.literal
-- literal-operator-id:
--  	operator "" identifier     C++0x
literalOperatorId :: P LiteralOperatorId
literalOperatorId = do
  pos <- pos
  kwOperator
  str <- stringLiteral'
  let Literal p s = str
  if s == ""
    then LiteralOperatorId pos <$> identifier
    else unexpected $ "string literal on " ++ show p

-- temp
-- template-declaration:
--  	template < template-parameter-list > declaration     
-- template-parameter-list:
--  	template-parameter
--  	template-parameter-list , template-parameter
templateDeclaration :: P Declaration
templateDeclaration =
  TemplateDeclaration
    <$> pos
    <*> (kwTemplate >> angles templateParameterList)
    <*> declaration
    <?> "template declarator"

templateParameterList :: P [TemplateParameter]
templateParameterList = templateParameter `sepBy1` comma

-- temp.param
-- template-parameter:
--  	type-parameter
--  	parameter-declaration
-- type-parameter:
--  	class ...opt identifier[opt]     C++0x
--  	class identifier[opt] = type-id
--  	typename ...opt identifier[opt]     C++0x
--  	typename identifier[opt] = type-id
--  	template < template-parameter-list > class ...opt identifier[opt]     C++0x
--  	template < template-parameter-list > class identifier[opt] = id-expression
templateParameter :: P TemplateParameter
templateParameter = do
  let typeP = undefined
  let param = undefined
  typeP <|> param <?> "template parameter"

typeParameter :: P TypeParameter
typeParameter = do
  let classSimple      = undefined
  let classAssigned    = undefined
  let typeSimple       = undefined
  let typeAssigned     = undefined
  let templateSimple   = undefined
  let templateAssigned = undefined
  choice
      [ try classSimple
      , classAssigned
      , try typeSimple
      , typeAssigned
      , try templateSimple
      , templateAssigned
      ]
    <?> "type parameter"

-- temp.names
-- simple-template-id:
--  	template-name < template-argument-list[opt] >     C++0x
-- template-id:
--  	simple-template-id     C++0x
--  	operator-function-id < template-argument-list[opt] >     C++0x
--  	literal-operator-id < template-argument-list[opt] >     C++0x
-- template-name:
--  	identifier
-- template-argument-list:
--  	template-argument ...opt     C++0x
--  	template-argument-list , template-argument ...opt     C++0x
-- template-argument:
--  	constant-expression     C++0x
--  	type-id     C++0x
--  	id-expression     C++0x
simpleTemplateId :: P SimpleTemplateId
simpleTemplateId =
  SimpleTemplateId
    <$> pos
    <*> templateName
    <*> angles (optionMaybe templateArgumentList)
    <?> "simple template id"

templateId :: P TemplateId
templateId = do
  let simple   = undefined
  let operator = undefined
  let literal  = undefined
  simple <|> operator <|> literal <?> "template id"

templateName :: P TemplateName
templateName = TemplateName <$> pos <*> identifier <?> "template name"

templateArgumentList :: P TemplateArgumentList
templateArgumentList =
  TemplateArgumentList
    <$> pos
    <*> (templateArgument `sepBy1` comma)
    <*> optionBool threeDot
    <?> "template argument list"

templateArgument :: P TemplateArgument
templateArgument = do
  let constExpr = undefined
  let tId       = undefined
  let idExpr    = undefined
  constExpr <|> tId <|> idExpr <?> "template argument"

-- temp.res
-- typename-specifier:
--  	typename ::opt nested-name-specifier identifier     C++0x
--  	typename ::opt nested-name-specifier template[opt] simple-template-id     C++0x
typenameSpecifier :: P TypenameSpecifier
typenameSpecifier = do
  let id       = undefined
  let template = undefined
  try id <|> template <?> "typename specifier"

-- temp.explicit
-- explicit-instantiation:
--  	extern[opt] template declaration     C++0x
explicitInstantiation :: P Declaration
explicitInstantiation =
  ExplicitInstantiation
    <$> pos
    <*> optionBool kwExtern
    <*> (kwTemplate >> declaration)
    <?> "explicit instantiation"

-- temp.expl.spec
-- explicit-specialization:
--  	template < > declaration
explicitSpecialization :: P Declaration
explicitSpecialization =
  ExplicitSpecialization
    <$> pos
    <*> (kwTemplate >> opLess >> opGreater >> declaration)
    <?> "explicit specialization"

-- except
-- try-block:
--  	try compound-statement handler-seq
-- function-try-block:
--  	try ctor-initializer[opt] compound-statement handler-seq     C++0x
-- handler-seq:
--  	handler
--  	handler handler-seq
-- handler:
--  	catch ( exception-declaration ) compound-statement
-- exception-declaration:
--  	attribute-specifier-seq[opt] type-specifier-seq declarator     C++0x
--  	attribute-specifier-seq[opt] type-specifier-seq abstract-declarator[opt]     C++0x
--  	...     C++0x
-- throw-expression:
--  	throw assignment-expression[opt]
tryBlock :: P TryBlock
tryBlock = do
  pos <- pos
  kwTry
  st <- compoundStatement
  TryBlock pos st <$> handlerSeq <?> "try block"

functionTryBlock :: P FunctionTryBlock
functionTryBlock = do
  pos <- pos
  kwTry
  ctor <- optionMaybe ctorInitializer
  st   <- compoundStatement
  FunctionTryBlock pos ctor st <$> handlerSeq <?> "function try block"

handlerSeq :: P [Handler]
handlerSeq = many1 handler

handler :: P Handler
handler = do
  pos <- pos
  kwCatch
  ex <- parens exceptionDeclaration
  Handler pos ex <$> compoundStatement <?> "handler"

exceptionDeclaration :: P ExceptionDeclaration
exceptionDeclaration = do
  let decl    = undefined
  let absDecl = undefined
  let dots    = undefined
  try decl <|> absDecl <|> dots <?> "exception declaration"

throwExpression :: P Expression
throwExpression =
  ThrowExpression
    <$> pos
    <*> (kwThrow >> optionMaybe assignmentExpression)
    <?> "throw expression"

-- except.spec
-- exception-specification:
--  	dynamic-exception-specification     C++0x
--  	noexcept-specification     C++0x
-- dynamic-exception-specification:
--  	throw ( type-id-list[opt] )     C++0x
-- type-id-list:
--  	type-id ...opt     C++0x
--  	type-id-list , type-id ...opt     C++0x
-- noexcept-specification:
--  	noexcept ( constant-expression )     C++0x
--  	noexcept     C++0x
exceptionSpecification :: P ExceptionSpecification
exceptionSpecification =
  ExceptionSpecification
    <$> pos
    <*> optionEither dynamicExceptionSpecification noexceptSpecification
    <?> "exception specification"

dynamicExceptionSpecification :: P DynamicExceptionSpecification
dynamicExceptionSpecification = do
  pos <- pos
  kwThrow
  DynamicExceptionSpecification pos
    <$> parens (optionMaybe typeIdList)
    <?> "dynamic exception specification"

typeIdList :: P TypeIdList
typeIdList =
  TypeIdList
    <$> pos
    <*> (typeId `sepBy1` comma)
    <*> optionBool threeDot
    <?> "type id list"

noexceptSpecification :: P NoexceptSpecification
noexceptSpecification = do
  let expr   = undefined
  let noexpr = undefined
  try expr <|> noexpr <?> "noexcept specification"
