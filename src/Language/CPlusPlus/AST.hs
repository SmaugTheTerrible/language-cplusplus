module Language.CPlusPlus.AST where

import           Text.Parsec                      hiding (parse)

import           Language.CPlusPlus.Internal.Base

newtype TranslationUnit =
  TU [Declaration]
  deriving (Show, Eq)

data Expression
  -- primary expression
  = LiteralExpression
      { _literalExpressionPos   :: SourcePos
      , _literalExpressionValue :: Literal
      }
  | ThisExpression
      { _thisExpressionPos :: SourcePos
      }
  | ParensedExpression
      { _parensedExpressionPos   :: SourcePos
      , _parensedExpressionValue :: Expression
      }
  | IdExpression
      { _idExpressionPos   :: SourcePos
      , _idExpressionValue :: Either UnqualifiedId QualifiedId
      }
  | LambdaExpression
      { _lambdaExpressionPos        :: SourcePos
      , _lambdaExpressionIntroducer :: LambdaIntroducer
      , _lambdaExpressionDeclarator :: Maybe LambdaDeclarator
      , _lambdaExpressionStatement  :: Statement
      }
  -- postfix expression
  --  	primary-expression
  --  	postfix-expression [ expression ]
  | GetByIndexExpression
      { _getByIndexExpressionPos   :: SourcePos
      , _getByIndexExpressionFrom  :: Expression
      , _getByIndexExpressionIndex :: Expression
      }
  --  	postfix-expression [ braced-init-list[opt] ]     C++0x
  | GetByBracedExpression
      { _getByBracedExpressionPos      :: SourcePos
      , _getByBracedExpressionFrom     :: Expression
      , _getByBracedExpressionInitList :: Maybe BracedInitList
      }
  --  	postfix-expression ( expression-list[opt] )
  | CallExpression
      { _callExpressionPos    :: SourcePos
      , _callExpressionCallee :: Expression
      , _callExpressionArgs   :: Maybe ExpressionList
      }
  --  	simple-type-specifier ( expression-list[opt] )
  | SimpleTypeCallExpression
      { _simpleTypeCallExpressionPos  :: SourcePos
      , _simpleTypeCallExpressionType :: SimpleTypeSpecifier
      , _simpleTypeCallExpressionArgs :: Maybe ExpressionList
      }
  --  	typename-specifier ( expression-list[opt] )
  | TypenameCallExpression
      { _typenameCallExpressionPos      :: SourcePos
      , _typenameCallExpressionTypename :: TypeSpecifier
      , _typenameCallExpressionArgs     :: Maybe ExpressionList
      }
  --  	simple-type-specifier braced-init-list     C++0x
  | SimpleTypeWithBracedExpression
      { _simpleTypeWithBracedExpressionPos        :: SourcePos
      , _simpleTypeWithBracedExpressionSimpleType :: SimpleTypeSpecifier
      , _simpleTypeWithBracedExpressionBraced     :: BracedInitList
      }
  --  	typename-specifier braced-init-list     C++0x
  | TypenameWithBracedExpression
      { _typenameWithBracedExpressionPos      :: SourcePos
      , _typenameWithBracedExpressionTypename :: TypenameSpecifier
      , _typenameWithBracedExpressionBraced   :: BracedInitList
      }
  --  	postfix-expression . template[opt] id-expression
  | GetFromRefExpression
      { _getFromRefExpressionPos        :: SourcePos
      , _getFromRefExpressionRef        :: Expression
      , _getFromRefExpressionIsTemplate :: Bool
      , _getFromRefExpressionElem       :: Expression
      }
  --  	postfix-expression -> template[opt] id-expression
  | GetFromPtrExpression
      { _getFromPtrExpressionPos        :: SourcePos
      , _getFromPtrExpressionPtr        :: Expression
      , _getFromPtrExpressionIsTemplate :: Bool
      , _getFromPtrExpressionElem       :: Expression
      }
  --  	postfix-expression . pseudo-destructor-name
  | GetDestructorFromRefExpression
      { _getDestructorFromRefExpressionPos        :: SourcePos
      , _getDestructorFromRefExpressionRef        :: Expression
      , _getDestructorFromRefExpressionDestructor :: PseudoDestructorName
      }
  --  	postfix-expression -> pseudo-destructor-name
  | GetDestructorFromPtrExpression
      { _getDestructorFromPtrExpressionPos        :: SourcePos
      , _getDestructorFromPtrExpressionPtr        :: Expression
      , _getDestructorFromPtrExpressionDestructor :: PseudoDestructorName
      }
  --  	postfix-expression ++
  | PostIncrementExpression
      { _postIncrementExpressionPos   :: SourcePos
      , _postIncrementExpressionValue :: Expression
      }
  --  	postfix-expression --
  | PostDecrementExpression
      { _postDecrementExpressionPos   :: SourcePos
      , _postDecrementExpressionValue :: Expression
      }
  --  	dynamic_cast < type-id > ( expression )
  | DynamicCastExpression
      { _dynamicCastExpressionPos    :: SourcePos
      , _dynamicCastExpressionType   :: TypeId
      , _dynamicCastExpressionCasted :: Expression
      }
  --  	static_cast < type-id > ( expression )
  | StaticCastExpression
      { _staticCastExpressionPos    :: SourcePos
      , _staticCastExpressionType   :: TypeId
      , _staticCastExpressionCasted :: Expression
      }
  --  	reinterpret_cast < type-id > ( expression )
  | ReinterpretCastExpression
      { _reinterpretCastExpressionPos    :: SourcePos
      , _reinterpretCastExpressionType   :: TypeId
      , _reinterpretCastExpressionCasted :: Expression
      }
  --  	const_cast < type-id > ( expression )
  | ConstCastExpression
      { _constCastExpressionPos    :: SourcePos
      , _constCastExpressionType   :: TypeId
      , _constCastExpressionCasted :: Expression
      }
  --  	typeid ( expression )
  --  	typeid ( type-id )
  | TypeIdExpression
      { _typeIdExpressionPos    :: SourcePos
      , _typeIdExpressionType   :: TypeId
      , _typeIdExpressionCalled :: Either Expression TypeId
      }
  --  	++ cast-expression
  | PrefIncrementExpression
      { _prefIncrementExpressionPos   :: SourcePos
      , _prefIncrementExpressionValue :: Expression
      }
  --  	-- cast-expression
  | PrefDecrementExpression
      { _prefDecrementExpressionPos   :: SourcePos
      , _prefDecrementExpressionValue :: Expression
      }
  --  	unary-operator cast-expression
  | UnaryOperationExpression
      { _unaryOperationExpressionPos      :: SourcePos
      , _unaryOperationExpressionOperator :: UnaryOperator
      , _unaryOperationExpressionValue    :: Expression
      }
  --  	sizeof unary-expression
  | SizeOfExpression
      { _sizeOfExpressionPos   :: SourcePos
      , _sizeOfExpressionValue :: Expression
      }
  --  	sizeof ( type-id )
  | SizeOfTypeExpression
      { _sizeOfTypeExpressionPos  :: SourcePos
      , _sizeOfTypeExpressionType :: TypeId
      }
  --  	sizeof ... ( identifier )     C++0x
  | SizeOfThreeDottedExpression
      { _sizeOfThreeDottedExpressionPos :: SourcePos
      , _sizeOfThreeDottedExpressionId  :: Identifier
      }
  --  	alignof ( type-id )     C++0x
  | AlignOfExpression
      { _alignOfExpressionPos  :: SourcePos
      , _alignOfExpressionType :: TypeId
      }
  --  	noexcept ( expression )     C++0x
  | NoexceptExpression
      { _noexceptExpressionPos   :: SourcePos
      , _noexceptExpressionValue :: Expression
      }
  --  	::opt new new-placement[opt] new-type-id new-initializer[opt]
  | NewExpression
      { _newExpressionPos          :: SourcePos
      , _newExpressionHasSquareDot :: Bool
      , _newExpressionPlacement    :: Maybe ExpressionList
      , _newExpressionTypeId       :: NewTypeId
      , _newExpressionInitializer  :: Maybe NewInitializer
      }
  --  	::opt new new-placement[opt] ( type-id ) new-initializer[opt]
  | NewParensedExpression
      { _newParensedExpressionPos          :: SourcePos
      , _newParensedExpressionHasSquareDot :: Bool
      , _newParensedExpressionPlacement    :: Maybe ExpressionList
      , _newParensedExpressionTypeId       :: TypeId
      , _newParensedExpressionInitializer  :: Maybe NewInitializer
      }
  --  	::opt delete cast-expression
  | DeleteExpression
      { _deleteExpressionPos          :: SourcePos
      , _deleteExpressionHasSquareDot :: Bool
      , _deleteExpressionValue        :: Expression
      }
  --  	::opt delete [ ] cast-expression
  | DeleteArrayExpression
      { _deleteArrayExpressionPos          :: SourcePos
      , _deleteArrayExpressionHasSquareDot :: Bool
      , _deleteArrayExpressionValue        :: Expression
      }
  --  	( type-id ) cast-expression
  | CastExpression
      { _castExpressionPos   :: SourcePos
      , _castExpressionType  :: TypeId
      , _castExpressionValue :: Expression
      }
  --  	pm-expression .* cast-expression
  | GetPtrFromRefExpression
      { _getPtrFromRefExpressionPos  :: SourcePos
      , _getPtrFromRefExpressionFrom :: Expression
      , _getPtrFromRefExpressionPtr  :: Expression
      }
  --  	pm-expression ->* cast-expression
  | GetPtrFromPtrExpression
      { _getPtrFromPtrExpressionPos  :: SourcePos
      , _getPtrFromPtrExpressionFrom :: Expression
      , _getPtrFromPtrExpressionPtr  :: Expression
      }
  -- binary expression
  | BinaryOperationExpression
      { _binaryOperationExpressionPos      :: SourcePos
      , _binaryOperationExpressionOperator :: BinaryOperator
      , _binaryOperationExpressionLeft     :: Expression
      , _binaryOperationExpressionRight    :: Expression
      }
  -- conditional expression
  | ConditionalExpression
      { _conditionalExpressionPos       :: SourcePos
      , _conditionalExpressionCondition :: Expression
      , _conditionalExpressionTruePart  :: Expression
      , _conditionalExpressionFalsePart :: Expression
      }
  -- logical-or-expression assignment-operator initializer-clause
  | AssignmentExpression
      { _assignmentExpressionPos         :: SourcePos
      , _assignmentExpressionValue       :: Expression
      , _assignmentExpressionOperator    :: AssignmentOperator
      , _assignmentExpressionInitializer :: InitializerClause
      }
  -- throw assignment-expression[opt]
  | ThrowExpression
      { _throwExpressionPos   :: SourcePos
      , _throwExpressionValue :: Maybe Expression
      }
  -- comma expression
  | CommaExpression
      { _commaExpressionPos   :: SourcePos
      , _commaExpressionLeft  :: Expression
      , _commaExpressionRight :: Expression
      }
  deriving (Show, Eq)

data UnqualifiedId
  = UnqualifiedIdIdentifier
      { _unqualifiedIdIdentifierPos   :: SourcePos
      , _unqualifiedIdIdentifierValue :: Identifier
      }
  | UnqualifiedIdOperatorFunctionId
      { _unqualifiedIdOperatorFunctionIdPos   :: SourcePos
      , _unqualifiedIdOperatorFunctionIdValue :: OperatorFunctionId
      }
  | UnqualifiedIdConversionFunctionId
      { _unqualifiedIdConversionFunctionIdPos   :: SourcePos
      , _unqualifiedIdConversionFunctionIdValue :: ConversionFunctionId
      }
  | UnqualifiedIdLiteralOperatorId
      { _unqualifiedIdLiteralOperatorIdPos   :: SourcePos
      , _unqualifiedIdLiteralOperatorIdValue :: LiteralOperatorId
      }
  | UnqualifiedIdDestructorClass
      { _unqualifiedIdDestructorClassPos   :: SourcePos
      , _unqualifiedIdDestructorClassValue :: ClassName
      }
  | UnqualifiedIdDestructorDecltype
      { _unqualifiedIdDestructorDecltypePos   :: SourcePos
      , _unqualifiedIdDestructorDecltypeValue :: DecltypeSpecifier
      }
  | UnqualifiedIdTemplateId
      { _unqualifiedIdTemplateIdPos   :: SourcePos
      , _unqualifiedIdTemplateIdValue :: TemplateId
      }
  deriving (Show, Eq)

data QualifiedId
  = QualifiedIdNestedId
      { _qualifiedIdNestedIdPos          :: SourcePos
      , _qualifiedIdNestedIdHasSquareDot :: Bool
      , _qualifiedIdNestedNameSpecifier  :: NestedNameSpecifier
      , _qualifiedIdNestedIdIsTemplate   :: Bool
      , _qualifiedIdNestedUnqualifiedId  :: UnqualifiedId
      }
  | QualifiedIdIdentifier
      { _qualifiedIdIdentifierPos   :: SourcePos
      , _qualifiedIdIdentifierValue :: Identifier
      }
  | QualifiedIdOperatorFunctionId
      { _qualifiedIdOperatorFunctionIdPos   :: SourcePos
      , _qualifiedIdOperatorFunctionIdValue :: OperatorFunctionId
      }
  | QualifiedIdLiteralOperatorId
      { _qualifiedIdLiteralOperatorIdPos   :: SourcePos
      , _qualifiedIdLiteralOperatorIdValue :: LiteralOperatorId
      }
  | QualifiedIdTemplateId
      { _qualifiedIdTemplateIdPos   :: SourcePos
      , _qualifiedIdTemplateIdValue :: TemplateId
      }
  deriving (Show, Eq)

data NestedNameSpecifier
  = NestedNameSpecifierType
      { _nestedNameSpecifierTypePos   :: SourcePos
      , _nestedNameSpecifierTypeValue :: TypeName
      }
  | NestedNameSpecifierNamespace
      { _nestedNameSpecifierNamespacePos   :: SourcePos
      , _nestedNameSpecifierNamespaceValue :: NamespaceName
      }
  | NestedNameSpecifierDecltype
      { _nestedNameSpecifierDecltypeSpecifierPos   :: SourcePos
      , _nestedNameSpecifierDecltypeSpecifierValue :: DecltypeSpecifier
      }
  | NestedNameSpecifierIdentifier
      { _nestedNameSpecifierIdentifierPos    :: SourcePos
      , _nestedNameSpecifierIdentifierPrefix :: NestedNameSpecifier
      , _nestedNameSpecifierIdentifierValue  :: Identifier
      }
  | NestedNameSpecifierTemplate
      { _nestedNameSpecifierTemplatePos        :: SourcePos
      , _nestedNameSpecifierTemplatePrefix     :: NestedNameSpecifier
      , _nestedNameSpecifierTemplateIsTemplate :: Bool
      , _nestedNameSpecifierTemplateValue      :: SimpleTemplateId
      }
  deriving (Show, Eq)

data LambdaIntroducer =
  LambdaIntroducer
    { _lambdaIntroducerPos   :: SourcePos
    , _lambdaIntroducerValue :: Maybe LambdaCapture
    }
  deriving (Show, Eq)

data LambdaCapture
  = LambdaCaptureDefault
      { _lambdaCaptureDefaultPos   :: SourcePos
      , _lambdaCaptureDefaultValue :: CaptureDefault
      }
  | LambdaCaptureList
      { _lambdaCaptureListPos   :: SourcePos
      , _lambdaCaptureListValue :: CaptureList
      }
  | LambdaCaptureDefaultAndList
      { _lambdaCaptureDefaultAndListPos    :: SourcePos
      , _lambdaCaptureDefaultAndListFirst  :: CaptureDefault
      , _lambdaCaptureDefaultAndListSecond :: CaptureList
      }
  deriving (Show, Eq)

data CaptureDefault =
  CaptureDefault
    { _captureDefaultPos  :: SourcePos
    , _captureDefaultType :: CaptureDefaultType
    }
  deriving (Show, Eq)

data CaptureDefaultType
  = CaptureByRef
  | CaptureByValue
  deriving (Show, Eq)

data CaptureList =
  CaptureList
    { _captureListPos         :: SourcePos
    , _captureListValue       :: [Capture]
    , _captureListHasThreeDot :: Bool
    }
  deriving (Show, Eq)

data Capture
  = CaptureIdentifier
      { _captureIdentifierRef   :: SourcePos
      , _captureIdentifierValue :: Identifier
      }
  | CaptureRef
      { _captureRefPos   :: SourcePos
      , _captureRefValue :: Identifier
      }
  | CaptureThis
      { _captureThisPos :: SourcePos
      }
  deriving (Show, Eq)

data LambdaDeclarator =
  LambdaDeclarator
    { _lambdaDeclaratorPos :: SourcePos
    , _lambdaDeclaratorParameterDeclarationClause :: ParameterDeclarationClause
    , _lambdaDeclaratorIsMutable :: Bool
    , _lambdaDeclaratorExceptionSpecification :: Maybe ExceptionSpecification
    , _lambdaDeclaratorAttributeSpecifierSeq :: [AttributeSpecifier]
    , _lambdaDeclaratorTrailingReturnType :: Maybe TrailingReturnType
    }
  deriving (Show, Eq)

data ExpressionList =
  ExpressionList
    { _expressionListPos   :: SourcePos
    , _expressionListValue :: InitializerList
    }
  deriving (Show, Eq)

-- | PseudoDestructorName =
--     PseudoDestructorNameNested
--     PseudoDestructorNameTemplate
--     PseudoDestructorNameTypeName
--     PseudoDestructorNameDecltype
data PseudoDestructorName
  --  	::opt nested-name-specifier[opt] type-name :: ~ type-name
  = PseudoDestructorNameNested
      { _pseudoDestructorNameNestedPos            :: SourcePos
      , _pseudoDestructorNameNestedHasSquareDot   :: Bool
      , _pseudoDestructorNameNestedName           :: Maybe NestedNameSpecifier
      , _pseudoDestructorNameNestedType           :: TypeName
      , _pseudoDestructorNameNestedDestructorName :: TypeName
      }
  --  	::opt nested-name-specifier template simple-template-id :: ~ type-name     C++0x
  | PseudoDestructorNameTemplate
      { _pseudoDestructorNameTemplatePos          :: SourcePos
      , _pseudoDestructorNameTemplateHasSquareDot :: Bool
      , _pseudoDestructorNameTemplateNestedName   :: NestedNameSpecifier
      , _pseudoDestructorNameTemplateId           :: SimpleTemplateId
      , _pseudoDestructorNameTemplateType         :: TypeName
      }
  --  	::opt nested-name-specifier[opt] ~ type-name
  | PseudoDestructorNameTypeName
      { _pseudoDestructorNameTypeNamePos          :: SourcePos
      , _pseudoDestructorNameTypeNameHasSquareDot :: Bool
      , _pseudoDestructorNameTypeNameNested       :: Maybe NestedNameSpecifier
      , _pseudoDestructorNameTypeName             :: TypeName
      }
  --  	~ decltype-specifier     C++0x
  | PseudoDestructorNameDecltype
      { _pseudoDestructorNameDecltypePos       :: SourcePos
      , _pseudoDestructorNameDecltypeSpecifier :: DecltypeSpecifier
      }
  deriving (Show, Eq)

data UnaryOperator =
  UnaryOperator
    { _unaryOperatorPos  :: SourcePos
    , _unaryOperatorType :: UnaryOperatorType
    }
  deriving (Show, Eq)

data UnaryOperatorType
  = Ptr
  | Ref
  | UnaryPlus
  | UnaryMinus
  | Not
  | Tilda
  deriving (Show, Eq)

data NewTypeId =
  NewTypeId
    { _newTypeIdPos              :: SourcePos
    , _newTypeIdTypeSpecifierSeq :: TypeSpecifierSeq
    , _newTypeIdNewDeclarator    :: Maybe NewDeclarator
    }
  deriving (Show, Eq)

data NewDeclarator
  = NewDeclaratorPtr
      { _newDeclaratorPtrPos      :: SourcePos
      , _newDeclaratorPtrOperator :: PtrOperator
      , _newDeclaratorPtrSuffix   :: Maybe NewDeclarator
      }
  | NewDeclaratorNoptr
      { _newDeclaratorNoptrPos   :: SourcePos
      , _newDeclaratorNoptrValue :: NoptrNewDeclarator
      }
  deriving (Show, Eq)

data NoptrNewDeclarator
  = NoptrNewDeclarator
      { _noptrNewDeclaratorPos        :: SourcePos
      , _noptrNewDeclaratorExpression :: Expression
      , _noptrNewDeclaratorAttributes :: [AttributeSpecifier]
      }
  | NoptrNewDeclaratorPrefixed
      { _noptrNewDeclaratorPrefixedPos        :: SourcePos
      , _noptrNewDeclaratorPrefix             :: NoptrNewDeclarator
      , _noptrNewDeclaratorPrefixedExpression :: Expression
      , _noptrNewDeclaratorPrefixedAttributes :: [AttributeSpecifier]
      }
  deriving (Show, Eq)

data NewInitializer
  = NewInitializerExpressionList
      { _newInitializerExpressionListPos   :: SourcePos
      , _newInitializerExpressionListValue :: Maybe ExpressionList
      }
  | NewInitializerBracedList
      { _newInitializerBracedListPos   :: SourcePos
      , _newInitializerBracedListValue :: BracedInitList
      }
  deriving (Show, Eq)

data BinaryOperator =
  BinaryOperator
    { _binaryOperatorPos  :: SourcePos
    , _binaryOperatorType :: BinaryOperatorType
    }
  deriving (Show, Eq)

data BinaryOperatorType
  = Multiply
  | Divide
  | Remain
  | Plus
  | Minus
  | LeftShift
  | RightShift
  | Greater
  | Lesser
  | GreaterEqual
  | LesserEqual
  | Equal
  | NotEqual
  | BitAnd
  | BitXor
  | BitOr
  | LogicalAnd
  | LogicalOr
  deriving (Show, Eq)

data AssignmentOperator =
  AssignmentOperator
    { _assignmentOperatorPos  :: SourcePos
    , _assignmentOperatorType :: AssignmentOperatorType
    }
  deriving (Show, Eq)

data AssignmentOperatorType
  = Assign
  | AssignMultiply
  | AssignDivision
  | AssignRemain
  | AssignAdd
  | AssignSubstract
  | AssignRightShift
  | AssignLeftShift
  | AssignAnd
  | AssignXor
  | AssignOr
  deriving (Show, Eq)

data Statement
  --  	attribute-specifier-seq[opt] identifier : statement
  = LabeledStatement
      { _labeledStatementPos        :: SourcePos
      , _labeledStatementAttributes :: [AttributeSpecifier]
      , _labeledStatementId         :: Identifier
      , _labeledStatementBlock      :: Statement
      }
  --  	attribute-specifier-seq[opt] case constant-expression : statement
  | LabeledCaseStatement
      { _labeledCaseStatementPos        :: SourcePos
      , _labeledCaseStatementAttributes :: [AttributeSpecifier]
      , _labeledCaseStatementExpression :: Expression
      , _labeledCaseStatementBlock      :: Statement
      }
  --  	attribute-specifier-seq[opt] default : statement
  | LabeledDefaultStatement
      { _labeledDefaultStatementPos        :: SourcePos
      , _labeledDefaultStatementAttributes :: [AttributeSpecifier]
      , _labeledDefaultStatementBlock      :: Statement
      }
  --  	attribute-specifier-seq[opt] expression-statement     C++0x
  --  	expression[opt] ;
  | ExpressionStatement
      { _expressionStatementPos        :: SourcePos
      , _expressionStatementAttributes :: [AttributeSpecifier]
      , _expressionStatementValue      :: Maybe Expression
      }
  --  	attribute-specifier-seq[opt] compound-statement     C++0x
  --  	{ statement-seq[opt] }
  | CompoundStatement
      { _compoundStatementPos        :: SourcePos
      , _compoundStatementAttributes :: [AttributeSpecifier]
      , _compoundStatementBlock      :: [Statement]
      }
  --  	attribute-specifier-seq[opt] selection-statement     C++0x
  --  	if ( condition ) statement
  | IfStatement
      { _ifStatementPos        :: SourcePos
      , _ifStatementAttributes :: [AttributeSpecifier]
      , _ifStatementCondition  :: Condition
      , _ifStatementBlock      :: Statement
      }
  --  	if ( condition ) statement else statement
  | IfElseStatement
      { _ifElseStatementPos        :: SourcePos
      , _ifElseStatementAttributes :: [AttributeSpecifier]
      , _ifElseStatementCondition  :: Condition
      , _ifElseStatementIfBlock    :: Statement
      , _ifElseStatementElseBlock  :: Statement
      }
  --  	switch ( condition ) statement
  | SwitchStatement
      { _switchStatementPos        :: SourcePos
      , _switchStatementAttributes :: [AttributeSpecifier]
      , _switchStatementCondition  :: Condition
      , _switchStatementBlock      :: Statement
      }
  --  	attribute-specifier-seq[opt] iteration-statement     C++0x
  --  	while ( condition ) statement
  | WhileStatement
      { _whileStatementPos        :: SourcePos
      , _whileStatementAttributes :: [AttributeSpecifier]
      , _whileStatementCondition  :: Condition
      , _whileStatementBlock      :: Statement
      }
  --  	do statement while ( expression ) ;
  | DoWhileStatement
      { _doWhileStatementPos        :: SourcePos
      , _doWhileStatementAttributes :: [AttributeSpecifier]
      , _doWhileStatementBlock      :: Statement
      , _doWhileStatementExpression :: Expression
      }
  --  	for ( for-init-statement condition[opt] ; expression[opt] ) statement
  | ForStatement
      { _forStatementPos        :: SourcePos
      , _forStatementAttributes :: [AttributeSpecifier]
      , _forStatementInit       :: ForInitStatement
      , _forStatementCondition  :: Maybe Condition
      , _forStatementExpression :: Maybe Expression
      , _forStatementBlock      :: Statement
      }
  --  	for ( for-range-declaration : for-range-initializer ) statement     C++0x
  | ForRangeStatement
      { _forRangeStatementPos         :: SourcePos
      , _forRangeStatementAttributes  :: [AttributeSpecifier]
      , _forRangeStatementDeclaration :: ForRangeDeclaration
      , _forRangeStatementInitializer :: ForRangeInitializer
      , _forRangeStatementBlock       :: Statement
      }
  --  	attribute-specifier-seq[opt] jump-statement     C++0x
  --  	break ;
  | BreakStatement
      { _breakStatementPos        :: SourcePos
      , _breakStatementAttributes :: [AttributeSpecifier]
      }
  --  	continue ;
  | ContinueStatement
      { _continueStatementPos        :: SourcePos
      , _continueStatementAttributes :: [AttributeSpecifier]
      }
  --  	return expression[opt] ;
  | ReturnStatement
      { _returnStatementPos        :: SourcePos
      , _returnStatementAttributes :: [AttributeSpecifier]
      , _returnStatementValue      :: Maybe Expression
      }
  --  	return braced-init-list[opt] ;     C++0x
  | ReturnBracedStatement
      { _returnBracedStatementPos        :: SourcePos
      , _returnBracedStatementAttributes :: [AttributeSpecifier]
      , _returnBracedStatementValue      :: Maybe BracedInitList
      }
  --  	goto identifier ;
  | GotoStatement
      { _gotoStatementPos        :: SourcePos
      , _gotoStatementAttributes :: [AttributeSpecifier]
      , _gotoStatementLabel      :: Identifier
      }
  --  	declaration-statement
  | DeclarationStatement
      { _declarationStatementPos   :: SourcePos
      , _declarationStatementValue :: Declaration
      }
  --  	attribute-specifier-seq[opt] try-block
  --  	try compound-statement handler-seq
  | TryStatement
      { _tryStatementPos        :: SourcePos
      , _tryStatementAttributes :: [AttributeSpecifier]
      , _tryStatementBlock      :: TryBlock
      --, _tryStatementHandlers   :: [Handler]
      }
  deriving (Show, Eq)

data Condition
  --  	expression
  = ExpressionCondition
      { _expressionConditionPos   :: SourcePos
      , _expressionConditionValue :: Expression
      }
  --  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
  | InitializerCondition
      { _initializerConditionPos            :: SourcePos
      , _initializerConditionAttributes     :: [AttributeSpecifier]
      , _initializerConditionDeclSpecifiers :: DeclSpecifierSeq
      , _initializerConditionDeclarator     :: Declarator
      , _initializerConditionInitializer    :: InitializerClause
      }
  --  	attribute-specifier-seq[opt] decl-specifier-seq declarator braced-init-list     C++0x
  | InitBracedCondition
      { _initBracedConditionPos            :: SourcePos
      , _initBracedConditionAttributes     :: [AttributeSpecifier]
      , _initBracedConditionDeclSpecifiers :: DeclSpecifierSeq
      , _initBracedConditionDeclarator     :: Declarator
      , _initBracedConditionBracedInitList :: BracedInitList
      }
  deriving (Show, Eq)

data ForInitStatement =
  ForInitStatement
    { _forInitStatementPos   :: SourcePos
    , _forInitStatementValue :: Either Statement Declaration
    }
  deriving (Show, Eq)

data ForRangeDeclaration =
  ForRangeDeclaration
    { _forRangeDeclarationPos            :: SourcePos
    , _forRangeDeclarationAttributes     :: [AttributeSpecifier]
    , _forRangeDeclarationTypeSpecifiers :: TypeSpecifierSeq
    , _forRangeDeclarationDeclarator     :: Declarator
    }
  deriving (Show, Eq)

data ForRangeInitializer =
  ForRangeInitializer
    { _forRangeInitializerPos   :: SourcePos
    , _forRangeInitializerValue :: Either Expression BracedInitList
    }
  deriving (Show, Eq)

data Declaration
  --  	block-declaration
  --  	simple-declaration
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] init-declarator-list[opt] ;     C++0x
  = SimpleDeclaration
      { _simpleDeclarationPos             :: SourcePos
      , _simpleDeclarationAttributes      :: [AttributeSpecifier]
      , _simpleDeclarationDeclSpecifiers  :: Maybe DeclSpecifierSeq
      , _simpleDeclarationInitDeclarators :: [InitDeclarator]
      }
  --  	asm-definition
  --  	asm ( string-literal ) ;
  | AsmDefinition
      { _asmDefinitionPos    :: SourcePos
      , _asmDefinitionString :: Literal
      }
  --  	namespace-alias-definition
  --  	namespace identifier = qualified-namespace-specifier ;
  | NamespaceAliasDefinition
      { _namespaceAliasDefinitionPos       :: SourcePos
      , _namespaceAliasDefinitionId        :: Identifier
      , _namespaceAliasDefinitionSpecifier :: QualifiedNamespaceSpecifier
      }
  --  	using-declaration
  --  	using typename[opt] ::opt nested-name-specifier unqualified-id ;
  | UsingNestedDeclaration
      { _usingNestedDeclarationPos           :: SourcePos
      , _usingNestedDeclarationTypename      :: Maybe TypeName
      , _usingNestedDeclarationHasSquareDot  :: Bool
      , _usingNestedDeclarationNameSpecifier :: NestedNameSpecifier
      , _usingNestedDeclarationId            :: UnqualifiedId
      }
  --  	using :: unqualified-id ;
  | UsingDeclaration
      { _usingDeclarationPos :: SourcePos
      , _usingDeclarationId  :: Identifier
      }
  --  	using-directive
  --  	attribute-specifier-seq[opt] using namespace ::opt nested-name-specifier[opt] namespace-name ;
  | UsingDirective
      { _usingDirectivePos           :: SourcePos
      , _usingDirectiveAttributes    :: [AttributeSpecifier]
      , _usingDirectiveHasSquareDot  :: Bool
      , _usingDirectiveNameSpecifier :: Maybe NestedNameSpecifier
      , _usingDirectiveName          :: NamespaceName
      }
  --  	static_assert-declaration     C++0x
  --  	static_assert ( constant-expression , string-literal ) ;     C++0x
  | StaticAssertDeclaration
      { _staticAssertDeclarationPos        :: SourcePos
      , _staticAssertDeclarationExpression :: Expression
      , _staticAssertDeclarationString     :: Literal
      }
  --  	alias-declaration     C++0x
  --  	using identifier = type-id ;     C++0x
  | AliasDeclaration
      { _aliasDeclarationPos  :: SourcePos
      , _aliasDeclarationId   :: Identifier
      , _aliasDeclarationType :: TypeId
      }
  --  	opaque-enum-declaration     C++0x
  --  	enum-key attribute-specifier-seq[opt] identifier enum-base[opt] ;     C++0x
  | OpaqueEnumDeclaration
      { _opaqueEnumDeclarationPos        :: SourcePos
      , _opaqueEnumDeclarationKey        :: EnumKey
      , _opaqueEnumDeclarationAttributes :: [AttributeSpecifier]
      , _opaqueEnumDeclarationId         :: Identifier
      , _opaqueEnumDeclarationBase       :: Maybe EnumBase
      }
  --  	function-definition
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator function-body     C++0x
  | FunctionDefinition
      { _functionDefinitionPos            :: SourcePos
      , _functionDefinitionAttributes     :: [AttributeSpecifier]
      , _functionDefinitionDeclSpecifiers :: DeclSpecifierSeq
      , _functionDefinitionDeclarator     :: Declarator
      , _functionDefinitionBody           :: FunctionBody
      }
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = default ;     C++0x
  | FunctionDefaultDefinition
      { _functionDefaultDefinitionPos            :: SourcePos
      , _functionDefaultDefinitionAttributes     :: [AttributeSpecifier]
      , _functionDefaultDefinitionDeclSpecifiers :: DeclSpecifierSeq
      , _functionDefaultDefinitionDeclarator     :: Declarator
      }
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = delete ;     C++0x
  | FunctionDeleteDefinition
      { _functionDeleteDefinitionPos            :: SourcePos
      , _functionDeleteDefinitionAttributes     :: [AttributeSpecifier]
      , _functionDeleteDefinitionDeclSpecifiers :: DeclSpecifierSeq
      , _functionDeleteDefinitionDeclarator     :: Declarator
      }
  --  	template-declaration
  --  	template < template-parameter-list > declaration     C++0x - The export keyword is reserved for future use
  | TemplateDeclaration
      { _templateDeclarationPos        :: SourcePos
      , _templateDeclarationParameters :: [TemplateParameter]
      , _templateDeclarationDeclarator :: Declaration
      }
  --  	explicit-instantiation
  --  	extern[opt] template declaration     C++0x
  | ExplicitInstantiation
      { _explicitInstantiationPos         :: SourcePos
      , _explicitInstantiationHasExtern   :: Bool
      , _explicitInstantiationDeclaration :: Declaration
      }
  --  	explicit-specialization
  --  	template < > declaration
  | ExplicitSpecialization
      { _explicitSpecializationPos         :: SourcePos
      , _explicitSpecializationDeclaration :: Declaration
      }
  --  	linkage-specification
  --  	extern string-literal { declaration-seq[opt] }
  --  	extern string-literal declaration
  | LinkageSpecification
      { _linkageSpecificationPos         :: SourcePos
      , _linkageSpecificationString      :: Literal
      , _linkageSpecificationDeclaration :: Either Declaration [Declaration]
      }
  --  	namespace-definition
  | NamespaceDefinition
      { _namespaceDefinitionPos :: SourcePos
      , _namespaceDefinitionValue :: Either NamedNamespaceDefinition UnnamedNamespaceDefinition
      }
  --  	empty-declaration     C++0x
  | EmptyDeclaration
      { _emptyDeclarationPos :: SourcePos
      }
  --  	attribute-declaration     C++0x
  --  	attribute-specifier-seq ;     C++0x
  | AttributeDeclaration
      { _attributeDeclarationPos        :: SourcePos
      , _attributeDeclarationAttributes :: [AttributeSpecifier]
      }
  deriving (Show, Eq)

data DeclSpecifier
  = StorageDeclSpecifier
      { _storageDeclSpecifierPos   :: SourcePos
      , _storageDeclSpecifierValue :: StorageClassSpecifier
      }
  | TypeDeclSpecifier
      { _typeDeclSpecifierPos   :: SourcePos
      , _typeDeclSpecifierValue :: TypeSpecifier
      }
  | FunctionDeclSpecifier
      { _functionDeclSpecifierPos   :: SourcePos
      , _functionDeclSpecifierValue :: FunctionSpecifier
      }
  | FriendDeclSpecifier
      { _friendDeclSpecifierPos :: SourcePos
      }
  | TypedefDeclSpecifier
      { _typedefDeclSpecifierPos :: SourcePos
      }
  | ConstexprDeclSpecifier
      { _constexprDeclSpecifierPos :: SourcePos
      }
  deriving (Show, Eq)

data DeclSpecifierSeq =
  DeclSpecifierSeq
    { _attributedDeclSpecifierPos        :: SourcePos
    , _attributedDeclSpecifierValue      :: [DeclSpecifier]
    , _attributedDeclSpecifierAttributes :: [AttributeSpecifier]
    }
  deriving (Show, Eq)

data StorageClassSpecifier
  = AutoSpecifier
      { _autoSpecifierPos :: SourcePos
      }
  | RegisterSpecifier
      { _registerSpecifierPos :: SourcePos
      }
  | StaticSpecifier
      { _staticSpecifierPos :: SourcePos
      }
  | ThreadLocalSpecifier
      { _threadLocalSpecifierPos :: SourcePos
      }
  | ExternSpecifier
      { _externSpecifierPos :: SourcePos
      }
  | MutableSpecifier
      { _mutableSpecifierPos :: SourcePos
      }
  deriving (Show, Eq)

data FunctionSpecifier
  = InlineSpecifier
      { _inlineSpecifierPos :: SourcePos
      }
  | VirtualSpecifier
      { _virtualSpecifierPos :: SourcePos
      }
  | ExplicitSpecifier
      { _explicitSpecifierPos :: SourcePos
      }
  deriving (Show, Eq)

data TypedefName =
  TypedefName
    { _typedefNamePos :: SourcePos
    , _typedefNameId  :: Identifier
    }
  deriving (Show, Eq)

data TypeSpecifier
  = TypeSpecifierTrailing
      { _typeSpecifierTrailingPos   :: SourcePos
      , _typeSpecifierTrailingValue :: TrailingTypeSpecifier
      }
  | TypeSpecifierClass
      { _typeSpecifierClassPos   :: SourcePos
      , _typeSpecifierClassValue :: ClassSpecifier
      }
  | TypeSpecifierEnum
      { _typeSpecifierEnumPos   :: SourcePos
      , _typeSpecifierEnumValue :: EnumSpecifier
      }
  deriving (Show, Eq)

data TrailingTypeSpecifier
  = TrailingTypeSpecifierSimple
      { _trailingTypeSpecifierSimplePos   :: SourcePos
      , _trailingTypeSpecifierSimpleValue :: SimpleTypeSpecifier
      }
  | TrailingTypeSpecifierElaborated
      { _trailingTypeSpecifierElaboratedPos   :: SourcePos
      , _trailingTypeSpecifierElaboratedValue :: ElaboratedTypeSpecifier
      }
  | TrailingTypeSpecifierTypename
      { _trailingTypeSpecifierTypenamePos   :: SourcePos
      , _trailingTypeSpecifierTypenameValue :: TypenameSpecifier
      }
  | TrailingTypeSpecifierCV
      { _trailingTypeSpecifierCVPos   :: SourcePos
      , _trailingTypeSpecifierCVValue :: CvQualifier
      }
  deriving (Show, Eq)

data TypeSpecifierSeq =
  TypeSpecifierSeq
    { _typeSpecifierAttributedPos   :: SourcePos
    , _typeSpecifierAttributedValue :: [TypeSpecifier]
    , _typeSpecifierAttributes      :: [AttributeSpecifier]
    }
  deriving (Show, Eq)

data TrailingTypeSpecifierSeq =
  TrailingTypeSpecifierSeq
    { _trailingTypeSpecifierAttributedPos   :: SourcePos
    , _trailingTypeSpecifierAttributedValue :: [TrailingTypeSpecifier]
    , _trailingTypeSpecifierAttributes      :: [AttributeSpecifier]
    }
  deriving (Show, Eq)

data SimpleTypeSpecifier
  = SimpleTypeSpecifier
      { _simpleTypeSpecifierPos          :: SourcePos
      , _simpleTypeSpecifierHasSquareDot :: Bool
      , _simpleTypeSpecifierNestedName   :: Maybe NestedNameSpecifier
      , _simpleTypeSpecifierTypeName     :: TypeName
      }
  | SimpleTypeSpecifierTemplate
      { _simpleTypeSpecifierTemplatePos          :: SourcePos
      , _simpleTypeSpecifierTemplateHasSquareDot :: Bool
      , _simpleTypeSpecifierTemplateNestedName   :: NestedNameSpecifier
      , _simpleTypeSpecifierTemplateId           :: SimpleTemplateId
      }
  | SimpleTypeSpecifierChar
      { _simpleTypeSpecifierCharPos :: SourcePos
      }
  | SimpleTypeSpecifierChar16T
      { _simpleTypeSpecifierChar16TPos :: SourcePos
      }
  | SimpleTypeSpecifierChar32T
      { _simpleTypeSpecifierChar32TPos :: SourcePos
      }
  | SimpleTypeSpecifierWcharT
      { _simpleTypeSpecifierWcharTPos :: SourcePos
      }
  | SimpleTypeSpecifierBool
      { _simpleTypeSpecifierBoolPos :: SourcePos
      }
  | SimpleTypeSpecifierShort
      { _simpleTypeSpecifierShortPos :: SourcePos
      }
  | SimpleTypeSpecifierInt
      { _simpleTypeSpecifierIntPos :: SourcePos
      }
  | SimpleTypeSpecifierLong
      { _simpleTypeSpecifierLongPos :: SourcePos
      }
  | SimpleTypeSpecifierSigned
      { _simpleTypeSpecifierSignedPos :: SourcePos
      }
  | SimpleTypeSpecifierUnsigned
      { _simpleTypeSpecifierUnsignedPos :: SourcePos
      }
  | SimpleTypeSpecifierFloat
      { _simpleTypeSpecifierFloatPos :: SourcePos
      }
  | SimpleTypeSpecifierDouble
      { _simpleTypeSpecifierDoublePos :: SourcePos
      }
  | SimpleTypeSpecifierVoid
      { _simpleTypeSpecifierVoidPos :: SourcePos
      }
  | SimpleTypeSpecifierAuto
      { _simpleTypeSpecifierAutoPos :: SourcePos
      }
  | SimpleTypeSpecifierDecltype
      { _simpleTypeSpecifierDecltypePos   :: SourcePos
      , _simpleTypeSpecifierDecltypeValue :: DecltypeSpecifier
      }
  deriving (Show, Eq)

data TypeName
  = TypeNameClass
      { _typeNameClassPos   :: SourcePos
      , _typeNameClassValue :: ClassName
      }
  | TypeNameEnum
      { _typeNameEnumPos   :: SourcePos
      , _typeNameEnumValue :: EnumName
      }
  | TypeNameTypedef
      { _typeNameTypedefPos   :: SourcePos
      , _typeNameTypedefValue :: TypedefName
      }
  | TypeNameTemplate
      { _typeNameTemplatePos   :: SourcePos
      , _typeNameTemplateValue :: SimpleTemplateId
      }
  deriving (Show, Eq)

data DecltypeSpecifier =
  DecltypeSpecifier
    { _decltypeSpecifierPos   :: SourcePos
    , _decltypeSpecifierValue :: Expression
    }
  deriving (Show, Eq)

data ElaboratedTypeSpecifier
  = ElaboratedTypeSpecifierId
      { _elaboratedTypeSpecifierIdPos          :: SourcePos
      , _elaboratedTypeSpecifierIdClassKey     :: ClassKey
      , _elaboratedTypeSpecifierIdAttributes   :: [AttributeSpecifier]
      , _elaboratedTypeSpecifierIdHasSquareDot :: Bool
      , _elaboratedTypeSpecifierIdNestedName   :: Maybe NestedNameSpecifier
      , _elaboratedTypeSpecifierId             :: Identifier
      }
  | ElaboratedTypeSpecifierTemplate
      { _elaboratedTypeSpecifierTemplatePos :: SourcePos
      , _elaboratedTypeSpecifierTemplateClassKey :: ClassKey
      , _elaboratedTypeSpecifierTemplateHasSquareDot :: Bool
      , _elaboratedTypeSpecifierTemplateNestedName :: Maybe NestedNameSpecifier
      , _elaboratedTypeSpecifierHasTemplate :: Bool
      , _elaboratedTypeSpecifierTemplateId :: SimpleTemplateId
      }
  | ElaboratedTypeSpecifierEnum
      { _elaboratedTypeSpecifierEnumPos          :: SourcePos
      , _elaboratedTypeSpecifierEnumHasSquareDot :: Bool
      , _elaboratedTypeSpecifierEnumNestedName   :: Maybe NestedNameSpecifier
      , _elaboratedTypeSpecifierEnumId           :: Identifier
      }
  deriving (Show, Eq)

data EnumName =
  EnumName
    { _enumNamePos   :: SourcePos
    , _enumNameValue :: Identifier
    }
  deriving (Show, Eq)

data EnumSpecifier =
  EnumSpecifier
    { _enumSpecifierPos         :: SourcePos
    , _enumSpecifierHead        :: EnumHead
    , _enumSpecifierEnumerators :: [EnumeratorDefinition]
    , _enumSpecifierHasDot      :: Bool
    }
  deriving (Show, Eq)

data EnumHead
  --  	enum-key attribute-specifier-seq[opt] identifier[opt] enum-base[opt]     C++0x
  = EnumHead
      { _enumHeadKey        :: SourcePos
      , _enumHeadAttributes :: [AttributeSpecifier]
      , _enumHeadId         :: Maybe Identifier
      , _enumHeadBase       :: Maybe EnumBase
      }
  --  	enum-key attribute-specifier-seq[opt] nested-name-specifier identifier enum-base[opt]     CD0x
  | EnumHeadNested
      { _enumHeadNestedPos        :: SourcePos
      , _enumHeadNestedKey        :: EnumKey
      , _enumHeadNestedAttributes :: [AttributeSpecifier]
      , _enumHeadNestedName       :: NestedNameSpecifier
      , _enumHeadNestedId         :: Identifier
      , _enumHeadNestedBase       :: Maybe EnumBase
      }
  deriving (Show, Eq)

data EnumKeyType
  = Enum
  | EnumClass
  | EnumStruct
  deriving (Show, Eq)

data EnumKey =
  EnumKey
    { _enumKeyPos  :: SourcePos
    , _enumKeyType :: EnumKeyType
    }
  deriving (Show, Eq)

data EnumBase =
  EnumBase
    { _enumBasePos   :: SourcePos
    , _enumBaseValue :: TypeSpecifierSeq
    }
  deriving (Show, Eq)

data EnumeratorDefinition
  = EnumeratorDefinition
      { _enumeratorDefinitionPos :: SourcePos
      , _enumeratorDefinition    :: Enumerator
      }
  | EnumeratorDefinitionWithValue
      { _enumeratorDefinitionWithValuePos :: SourcePos
      , _enumeratorDefinitionWithValueId  :: Enumerator
      , _enumeratorDefinitionValue        :: Expression
      }
  deriving (Show, Eq)

data Enumerator =
  Enumerator
    { _enumeratorPos   :: SourcePos
    , _enumeratorValue :: Identifier
    }
  deriving (Show, Eq)

data NamespaceName =
  NamespaceName
    { _namespaceNamePos   :: SourcePos
    , _namespaceNameValue :: Either OriginalNamespaceName NamespaceAlias
    }
  deriving (Show, Eq)

data OriginalNamespaceName =
  OriginalNamespaceName
    { _originalNamespaceNamePos   :: SourcePos
    , _originalNamespaceNameValue :: Identifier
    }
  deriving (Show, Eq)

data NamedNamespaceDefinition =
  NamedNamespaceDefinition
    { _namedNamespaceDefinitionPos :: SourcePos
    , _namedNamespaceDefinitionValue :: Either OriginalNamespaceDefinition ExtensionNamespaceDefinition
    }
  deriving (Show, Eq)

data OriginalNamespaceDefinition =
  OriginalNamespaceDefinition
    { _originalNamespaceDefinitionPos      :: SourcePos
    , _originalNamespaceDefinitionIsInline :: Bool
    , _originalNamespaceDefinitionId       :: Identifier
    , _originalNamespaceDefinitionBody     :: [Declaration]
    }
  deriving (Show, Eq)

data ExtensionNamespaceDefinition =
  ExtensionNamespaceDefinition
    { _extensionNamespaceDefinitionPos      :: SourcePos
    , _extensionNamespaceDefinitionIsInline :: Bool
    , _extensionNamespaceDefinitionOriginal :: OriginalNamespaceName
    , _extensionNamespaceDefinitionBody     :: [Declaration]
    }
  deriving (Show, Eq)

data UnnamedNamespaceDefinition =
  UnnamedNamespaceDefinition
    { _unnamedNamespaceDefinitionPos      :: SourcePos
    , _unnamedNamespaceDefinitionIsInline :: Bool
    , _unnamedNamespaceDefinitionBody     :: [Declaration]
    }
  deriving (Show, Eq)

data NamespaceAlias =
  NamespaceAlias
    { _namespaceAliasPos   :: SourcePos
    , _namespaceAliasValue :: Identifier
    }
  deriving (Show, Eq)

data QualifiedNamespaceSpecifier =
  QualifiedNamespaceSpecifier
    { _qualifiedNamespaceSpecifierPos          :: SourcePos
    , _qualifiedNamespaceSpecifierHasSquareDot :: Bool
    , _qualifiedNamespaceSpecifierNestedName   :: Maybe NestedNameSpecifier
    , _qualifiedNamespaceSpecifierName         :: NamespaceName
    }
  deriving (Show, Eq)

data AttributeSpecifier
  = AttributeSpecifier
      { _attributeSpecifierPos   :: SourcePos
      , _attributeSpecifierValue :: AttributeList
      }
  | AlignmentAttribute
      { _alignmentAttributePos   :: SourcePos
      , _alignmentAttributeValue :: AlignmentSpecifier
      }
  deriving (Show, Eq)

data AlignmentSpecifier
  = AlignAsType
      { _alignAsTypePos      :: SourcePos
      , _alignAsTypeValue    :: TypeId
      , _alignAsTypeThreeDot :: Bool
      }
  | AlignAsExpression
      { _alignAsExpressionPos      :: SourcePos
      , _alignAsExpressionValue    :: Expression
      , _alignAsExpressionThreeDot :: Bool
      }
  deriving (Show, Eq)

data AttributeList =
  AttributeList
    { _attributeListPos      :: SourcePos
    , _attributeListValue    :: [Attribute]
    , _attributeListThreeDot :: Bool
    }
  deriving (Show, Eq)

data Attribute =
  Attribute
    { _attributePos            :: SourcePos
    , _attributeToken          :: AttributeToken
    , _attributeArgumentClause :: Maybe AttributeArgumentClause
    }
  deriving (Show, Eq)

data AttributeToken =
  AttributeToken
    { _attributeTokenPos   :: SourcePos
    , _attributeTokenValue :: Either Identifier AttributeScopedToken
    }
  deriving (Show, Eq)

data AttributeScopedToken =
  AttributeScopedToken
    { _attributeScopedTokenPos       :: SourcePos
    , _attributeScopedTokenNamespace :: AttributeNamespace
    , _attributeScopedTokenId        :: Identifier
    }
  deriving (Show, Eq)

data AttributeNamespace =
  AttributeNamespace
    { _attributeNamespacePos   :: SourcePos
    , _attributeNamespaceValue :: Identifier
    }
  deriving (Show, Eq)

data AttributeArgumentClause =
  AttributeArgumentClause
    { _attributeArgumentClausePos   :: SourcePos
    , _attributeArgumentClauseValue :: [BalancedToken]
    }
  deriving (Show, Eq)

data BalancedToken
  = ParensedTokenSeq
      { _parensedTokenSeqPos   :: SourcePos
      , _parensedTokenSeqValue :: [BalancedToken]
      }
  | SquaredTokenSeq
      { _squaredTokenSeqPos   :: SourcePos
      , _squaredTokenSeqValue :: [BalancedToken]
      }
  | BracedTokenSeq
      { _bracedTokenSeqPos   :: SourcePos
      , _bracedTokenSeqValue :: [BalancedToken]
      }
  | BalancedToken
      { _balancedTokenPos   :: SourcePos
      , _balancedTokenValue :: BToken
      }
  deriving (Show, Eq)

data InitDeclarator =
  InitDeclarator
    { _initDeclaratorPos         :: SourcePos
    , _initDeclaratorValue       :: Declarator
    , _initDeclaratorInitializer :: Maybe Initializer
    }
  deriving (Show, Eq)

data Declarator
  = DeclaratorPtr
      { _declaratorPtrPos   :: SourcePos
      , _declaratorPtrValue :: PtrDeclarator
      }
  | DeclaratorNoptr
      { _declaratorNoptrPos        :: SourcePos
      , _declaratorNoptrValue      :: NoptrDeclarator
      , _declaratorNoptrParameters :: ParametersAndQualifiers
      , _declaratorNoptrReturnType :: TrailingReturnType
      }
  deriving (Show, Eq)

data PtrDeclarator
  = PtrDeclaratorNoptr
      { _ptrDeclaratorNoptrPos   :: SourcePos
      , _ptrDeclaratorNoptrValue :: NoptrDeclarator
      }
  | PtrDeclarator
      { _ptrDeclaratorPos      :: SourcePos
      , _ptrDeclaratorOperator :: PtrOperator
      , _ptrDeclaratorValue    :: PtrDeclarator
      }
  deriving (Show, Eq)

data NoptrDeclarator
  = NoptrDeclarator
      { _noptrDeclaratorPos        :: SourcePos
      , _noptrDeclaratorValue      :: DeclaratorId
      , _noptrDeclaratorAttributes :: [AttributeSpecifier]
      }
  | NoptrDeclaratorParametred
      { _noptrDeclaratorParametredPos   :: SourcePos
      , _noptrDeclaratorParametredValue :: NoptrDeclarator
      , _noptrDeclaratorParameters      :: ParametersAndQualifiers
      }
  | NoptrDeclaratorIndexed
      { _noptrDeclaratorIndexedPos        :: SourcePos
      , _noptrDeclaratorIndexedValue      :: NoptrDeclarator
      , _noptrDeclaratorIndex             :: Expression
      , _noptrDeclaratorIndexedAttributes :: [AttributeSpecifier]
      }
  | NoptrDeclaratorParensed
      { _noptrDeclaratorParensedPos   :: SourcePos
      , _noptrDeclaratorParensedValue :: PtrDeclarator
      }
  deriving (Show, Eq)

data ParametersAndQualifiers =
  ParametersAndQualifiers
    { _parametersAndQualifiersPos :: SourcePos
    , _parametersAndQualifiersParameterDeclarations :: ParameterDeclarationClause
    , _parametersAndQualifiersAttributes :: [AttributeSpecifier]
    , _parametersAndQualifiersCv :: [CvQualifier]
    , _parametersAndQualifiersRef :: Maybe RefQualifier
    , _parametersAndQualifiersException :: Maybe ExceptionSpecification
    }
  deriving (Show, Eq)

data TrailingReturnType =
  TrailingReturnType
    { _trailingReturnType           :: SourcePos
    , _trailingReturnTypeSpecifier  :: TrailingTypeSpecifierSeq
    , _trailingReturnTypeDeclarator :: Maybe AbstractDeclarator
    }
  deriving (Show, Eq)

data PtrOperator
  = StarOperator
      { _starOperatorPos        :: SourcePos
      , _starOperatorAttributes :: [AttributeSpecifier]
      , _starOperatorQualifiers :: [CvQualifier]
      }
  | RefOperator
      { _refOperatorPos        :: SourcePos
      , _refOperatorAttributes :: [AttributeSpecifier]
      }
  | DoubleRefOperator
      { _doubleRefOperatorPos        :: SourcePos
      , _doubleRefOperatorAttributes :: [AttributeSpecifier]
      }
  | NestedStarOperator
      { _nestedStarOperatorPos          :: SourcePos
      , _nestedStarOperatorHasSquareDot :: Bool
      , _nestedStarOperatorNestedName   :: NestedNameSpecifier
      , _nestedStarOperatorAttributes   :: [AttributeSpecifier]
      , _nestedStarOperatorQualifiers   :: [CvQualifier]
      }
  deriving (Show, Eq)

data CvQualifier
  = ConstQualifier
      { _constQualifierPos :: SourcePos
      }
  | VolatileQualifier
      { _volatileQualifierPos :: SourcePos
      }
  deriving (Show, Eq)

data RefQualifier
  = RefQualifier
      { _refQualifierPos :: SourcePos
      }
  | RefQualifierDouble
      { _refQualifierDoublePos :: SourcePos
      }
  deriving (Show, Eq)

data DeclaratorId
  = DeclaratorIdExpression
      { _declaratorIdExpressionPos         :: SourcePos
      , _declaratorIdExpressionHasThreeDot :: Bool
      , _declaratorIdExpressionValue       :: Expression
      }
  | DeclaratorIdClass
      { _declaratorIdClassPos          :: SourcePos
      , _declaratorIdClassHasSquareDot :: Bool
      , _declaratorIdClassNestedName   :: Maybe NestedNameSpecifier
      , _declaratorIdClassName         :: ClassName
      }
  deriving (Show, Eq)

data TypeId =
  TypeId
    { _typeIdPos        :: SourcePos
    , _typeIdSpecifiers :: TypeSpecifierSeq
    , _typeIdDeclarator :: Maybe AbstractDeclarator
    }
  deriving (Show, Eq)

data AbstractDeclarator
  = AbstractDeclaratorPtr
      { _abstractDeclaratorPtrPos   :: SourcePos
      , _abstractDeclaratorPtrValue :: PtrAbstractDeclarator
      }
  | AbstractDeclaratorNoptr
      { _abstractDeclaratorNoptrPos :: SourcePos
      , _abstractDeclaratorNoptrValue :: Maybe NoptrAbstractDeclarator
      , _abstractDeclaratorNoptrParameters :: ParametersAndQualifiers
      , _abstractDeclaratorNoptrTralingReturnType :: TrailingReturnType
      }
  | AbstractDeclaratorThreeDot
      { _abstractDeclaratorThreeDotPos :: SourcePos
      }
  deriving (Show, Eq)

data PtrAbstractDeclarator
  = PtrAbstractDeclaratorNoptr
      { _ptrAbstractDeclaratorNoptrPos   :: SourcePos
      , _ptrAbstractDeclaratorNoptrValue :: NoptrAbstractDeclarator
      }
  | PtrAbstractDeclarator
      { _ptrAbstractDeclaratorPos      :: SourcePos
      , _ptrAbstractDeclaratorOperator :: PtrOperator
      , _ptrAbstractDeclaratorValue    :: Maybe PtrAbstractDeclarator
      }
  deriving (Show, Eq)

data NoptrAbstractDeclarator
  = NoptrAbstractDeclarator
      { _noptrAbstractDeclaratorPos        :: SourcePos
      , _noptrAbstractDeclaratorPref       :: Maybe NoptrAbstractDeclarator
      , _noptrAbstractDeclaratorParameters :: ParametersAndQualifiers
      }
  | ArrayNoptrAbstractDeclarator
      { _arrayNoptrAbstractDeclaratorPos        :: SourcePos
      , _arrayNoptrAbstractDeclaratorPref       :: Maybe NoptrAbstractDeclarator
      , _arrayNoptrAbstractDeclaratorExpression :: Expression
      , _arrayNoptrAbstractDeclaratorAttributes :: [AttributeSpecifier]
      }
  | ParensedPtrAbstractDeclarator
      { _parensedPtrAbstractDeclaratorPos   :: SourcePos
      , _parensedPtrAbstractDeclaratorValue :: PtrAbstractDeclarator
      }
  deriving (Show, Eq)

data ParameterDeclarationClause
  = ParameterDeclarationClause
      { _parameterDeclarationClausePos          :: SourcePos
      , _parameterDeclarationClauseList         :: [ParameterDeclaration]
      , _parameterDeclarationClauseHasThreeDots :: Bool
      }
  | ParameterDeclarationClauseDotted
      { _parameterDeclarationClauseDottedPos  :: SourcePos
      , _parameterDeclarationClauseDottedList :: [ParameterDeclaration]
      }
  deriving (Show, Eq)

data ParameterDeclaration
  = ParameterDeclaration
      { _parameterDeclarationPos            :: SourcePos
      , _parameterDeclarationAttributes     :: [AttributeSpecifier]
      , _parameterDeclarationDeclSpecifiers :: DeclSpecifierSeq
      , _parameterDeclarationDeclarator     :: Declarator
      }
  | ParameterDeclarationInitialized
      { _parameterDeclarationInitializedPos            :: SourcePos
      , _parameterDeclarationInitializedAttributes     :: [AttributeSpecifier]
      , _parameterDeclarationInitializedDeclSpecifiers :: DeclSpecifierSeq
      , _parameterDeclarationInitializedDeclarator     :: Declarator
      , _parameterDeclarationInitializedInitializers   :: InitializerClause
      }
  | AbstractParameterDeclaration
      { _abstractParameterDeclarationPos            :: SourcePos
      , _abstractParameterDeclarationAttributes     :: [AttributeSpecifier]
      , _abstractParameterDeclarationDeclSpecifiers :: DeclSpecifierSeq
      , _abstractParameterDeclarationDeclarator     :: Maybe AbstractDeclarator
      }
  | AbstractParameterDeclarationInitialized
      { _abstractParameterDeclarationInitializedPos :: SourcePos
      , _abstractParameterDeclarationInitializedAttributes :: [AttributeSpecifier]
      , _abstractParameterDeclarationInitializedDeclSpecifiers :: DeclSpecifierSeq
      , _abstractParameterDeclarationInitializedDeclarator :: Maybe AbstractDeclarator
      , _abstractParameterDeclarationInitializedInitializers :: InitializerClause
      }
  deriving (Show, Eq)

data FunctionBody
  = CtorBody
      { _ctorBodyPos         :: SourcePos
      , _ctorBodyInitializer :: Maybe CtorInitializer
      , _ctorBodyValue       :: Statement
      }
  | FunctionBody
      { _functionBodyPos   :: SourcePos
      , _functionBodyValue :: FunctionTryBlock
      }
  deriving (Show, Eq)

data Initializer
  = Initializer
      { _initializerPos   :: SourcePos
      , _initializerValue :: BraceOrEqualInitializer
      }
  | ParensedExpressionList
      { _parensedExpressionListPos   :: SourcePos
      , _parensedExpressionListValue :: ExpressionList
      }
  deriving (Show, Eq)

data BraceOrEqualInitializer
  = EqualInitializer
      { _equalInitializerPos   :: SourcePos
      , _equalInitializerValue :: InitializerClause
      }
  | BraceInitializer
      { _braceInitializerPos   :: SourcePos
      , _braceInitializerValue :: BracedInitList
      }
  deriving (Show, Eq)

data InitializerClause
  = ExpressionInitializerClause
      { _expressionInitializerClausePos   :: SourcePos
      , _expressionInitializerClauseValue :: Expression
      }
  | ListInitializerClause
      { _listInitializerClausePos   :: SourcePos
      , _listInitializerClauseValue :: BracedInitList
      }
  deriving (Show, Eq)

data InitializerList =
  InitializerList
    { _initializerListPos          :: SourcePos
    , _initializerListClauses      :: [InitializerClause]
    , _initializerListHasThreeDots :: Bool
    }
  deriving (Show, Eq)

data BracedInitList
  = BracedInitList
      { _bracedInitListPos              :: SourcePos
      , _bracedInitListValue            :: InitializerList
      , _bracedInitListHasTrailingComma :: Bool
      }
  | EmptyBracedInitList
      { _emptyBracedInitListPos :: SourcePos
      }
  deriving (Show, Eq)

data ClassName
  = ClassNameId
      { _classNameIdPos   :: SourcePos
      , _classNameIdValue :: Identifier
      }
  | ClassNameTemplate
      { _classNameTemplatePos   :: SourcePos
      , _classNameTemplateValue :: SimpleTemplateId
      }
  deriving (Show, Eq)

data ClassSpecifier =
  ClassSpecifier
    { _classSpecifierPos     :: SourcePos
    , _classSpecifierHead    :: ClassHead
    , _classSpecifierMembers :: Maybe MemberSpecification
    }
  deriving (Show, Eq)

data ClassHead
  = ClassHead
      { _classHeadPos            :: SourcePos
      , _classHeadKey            :: ClassKey
      , _classHeadAttributes     :: [AttributeSpecifier]
      , _classHeadName           :: ClassHeadName
      , _classHeadVirtSpecifiers :: [ClassVirtSpecifier]
      , _classHeadBaseClause     :: Maybe BaseClause
      }
  | OpaqueClassHead
      { _opaqueClassHeadPos        :: SourcePos
      , _opaqueClassHeadKey        :: ClassKey
      , _opaqueClassHeadAttributes :: [AttributeSpecifier]
      , _opaqueClassHeadBaseClause :: Maybe BaseClause
      }
  deriving (Show, Eq)

data ClassHeadName =
  ClassHeadName
    { _classHeadNamePos    :: SourcePos
    , _classHeadNameNested :: Maybe NestedNameSpecifier
    , _classHeadNameValue  :: ClassName
    }
  deriving (Show, Eq)

data ClassVirtSpecifier =
  ClassVirtSpecifier
    { _classVirtSpecifierPos   :: SourcePos
    , _classVirtSpecifierValue :: ClassVirtSpecifierType
    }
  deriving (Show, Eq)

data ClassVirtSpecifierType
  = ClassFinal
  | ClassExplicit
  deriving (Show, Eq)

data ClassKey =
  ClassKey
    { _classKeyPos   :: SourcePos
    , _classKeyValue :: ClassKeyType
    }
  deriving (Show, Eq)

data ClassKeyType
  = Class
  | Struct
  | Union
  deriving (Show, Eq)

data MemberSpecification
  = MemberSpecification
      { _memberSpecificationPos   :: SourcePos
      , _memberSpecificationValue :: MemberDeclaration
      , _memberSpecificationRest  :: Maybe MemberSpecification
      }
  | AccessSpecifiedMemberSpecification
      { _accessSpecifiedMemberSpecificationPos :: SourcePos
      , _accessSpecifiedMemberSpecificationSpecifier :: AccessSpecifier
      , _accessSpecifiedMemberSpecificationRest :: Maybe MemberSpecification
      }
  deriving (Show, Eq)

data MemberDeclaration
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] member-declarator-list[opt] ;     C++0x
  = SimpleMemberDeclaration
      { _simpleMemberDeclarationPos            :: SourcePos
      , _simpleMemberDeclarationDeclSpecifiers :: Maybe DeclSpecifierSeq
      , _simpleMemberDeclarationList           :: [MemberDeclarator]
      }
  --  	function-definition ;[opt]
  | FunctionMemberDeclaration
      { _functionMemberDeclarationPos                  :: SourcePos
      , _functionMemberDeclarationValue                :: Declaration
      , _functionMemberDeclarationHasTrailingSemicolon :: Bool
      }
  --  	using-declaration
  | UsingMemberDeclaration
      { _usingMemberDeclarationPos   :: SourcePos
      , _usingMemberDeclarationValue :: Declaration
      }
  --  	static_assert-declaration     C++0x
  | StaticAssertMemberDeclaration
      { _staticAssertMemberDeclarationPos   :: SourcePos
      , _staticAssertMemberDeclarationValue :: Declaration
      }
  --  	template-declaration
  | TemplateMemberDeclaration
      { _templateMemberDeclarationPos   :: SourcePos
      , _templateMemberDeclarationValue :: Declaration
      }
  --  	alias-declaration     C++0x
  | AliasMemberDeclaration
      { _aliasMemberDeclarationPos   :: SourcePos
      , _aliasMemberDeclarationValue :: Declaration
      }
  deriving (Show, Eq)

data MemberDeclarator
  --  	declarator virt-specifier-seq[opt] pure-specifier[opt]
  = MemberDeclarator
      { _memberDeclaratorPos        :: SourcePos
      , _memberDeclaratorValue      :: Declarator
      , _memberDeclaratorSpecifiers :: [VirtSpecifier]
      , _memberDeclaratorPure       :: Maybe PureSpecifier
      }
  --  	declarator virt-specifier-seq[opt] brace-or-equal-initializer[opt]     C++0x
  | InitializedMemberDeclarator
      { _initializedMemberDeclaratorPos         :: SourcePos
      , _initializedMemberDeclaratorValue       :: Declarator
      , _initializedMemberDeclaratorSpecifiers  :: [VirtSpecifier]
      , _initializedMemberDeclaratorInitializer :: Maybe BraceOrEqualInitializer
      }
  --  	identifier[opt] attribute-specifier-seq[opt] virt-specifier-seq[opt] : constant-expression
  | ExpressionMemberDeclarator
      { _expressionMemberDeclaratorPos        :: SourcePos
      , _expressionMemberDeclaratorId         :: Maybe Identifier
      , _expressionMemberDeclaratorAttributes :: [AttributeSpecifier]
      , _expressionMemberDeclaratorSpecifiers :: [VirtSpecifier]
      , _expressionMemberDeclaratorValue      :: Expression
      }
  deriving (Show, Eq)

data VirtSpecifier =
  VirtSpecifier
    { _virtSpecifierPos   :: SourcePos
    , _virtSpecifierValue :: VirtSpecifierType
    }
  deriving (Show, Eq)

data VirtSpecifierType
  = MemberFinal
  | MemberOverride
  | MemberNew
  deriving (Show, Eq)

data PureSpecifier =
  PureSpecifier
    { _pureSpecifierPos :: SourcePos
    }
  deriving (Show, Eq)

data BaseClause =
  BaseClause
    { _baseClausePos  :: SourcePos
    , _baseClauseList :: BaseSpecifierList
    }
  deriving (Show, Eq)

data BaseSpecifierList =
  BaseSpecifierList
    { _baseSpecifierListPos          :: SourcePos
    , _baseSpecifierListValue        :: [BaseSpecifier]
    , _baseSpecifierListHasThreeDots :: Bool
    }
  deriving (Show, Eq)

data BaseSpecifier
  --  	attribute-specifier-seq[opt] base-type-specifier     C++0x
  = BaseSpecifier
      { _baseSpecifierPos        :: SourcePos
      , _baseSpecifierAttributes :: [AttributeSpecifier]
      , _baseSpecifierValue      :: BaseTypeSpecifier
      }
  --  	attribute-specifier-seq[opt] virtual access-specifier[opt] base-type-specifier     C++0x
  | VirtualBaseSpecifier
      { _virtualBaseSpecifierPos             :: SourcePos
      , _virtualBaseSpecifierAttributes      :: [AttributeSpecifier]
      , _virtualBaseSpecifierAccessSpecifier :: Maybe AccessSpecifier
      , _virtualBaseSpecifierValue           :: BaseTypeSpecifier
      }
  --  	attribute-specifier-seq[opt] access-specifier virtual[opt] base-type-specifier     C++0x
  | AccessSpecBaseSpecifier
      { _accessSpecBaseSpecifierPos             :: SourcePos
      , _accessSpecBaseSpecifierAttributes      :: [AttributeSpecifier]
      , _accessSpecBaseSpecifierAccessSpecifier :: AccessSpecifier
      , _accessSpecBaseSpecifierIsVirtual       :: Bool
      , _accessSpecBaseSpecifierValue           :: BaseTypeSpecifier
      }
  deriving (Show, Eq)

data ClassOrDecltype
  = ClassOrDecltypeFirst
      { _classOrDecltypeFirstPos             :: SourcePos
      , _classOrDecltypeFirstHasSquareDot    :: Bool
      , _classOrDecltypeFirstNestedSpecifier :: Maybe NestedNameSpecifier
      , _classOrDecltypeFirstValue           :: ClassName
      }
  | ClassOrDecltypeSecond
      { _classOrDecltypeSecondPos   :: SourcePos
      , _classOrDecltypeSecondValue :: DecltypeSpecifier
      }
  deriving (Show, Eq)

data BaseTypeSpecifier =
  BaseTypeSpecifier
    { _baseTypeSpecifierPos   :: SourcePos
    , _baseTypeSpecifierValue :: ClassOrDecltype
    }
  deriving (Show, Eq)

data AccessSpecifier =
  AccessSpecifier
    { _accessSpecifierPos   :: SourcePos
    , _accessSpecifierValue :: AccessSpecifierType
    }
  deriving (Show, Eq)

data AccessSpecifierType
  = Public
  | Protected
  | Private
  deriving (Show, Eq)

data ConversionFunctionId =
  ConversionFunctionId
    { _conversionFunctionIdPos    :: SourcePos
    , _conversionFunctionIdTypeId :: ConversionTypeId
    }
  deriving (Show, Eq)

data ConversionTypeId =
  ConversionTypeId
    { _conversionTypeIdPos        :: SourcePos
    , _conversionTypeIdSpecifiers :: TypeSpecifierSeq
    , _conversionTypeIdDeclarator :: Maybe ConversionDeclarator
    }
  deriving (Show, Eq)

data ConversionDeclarator =
  ConversionDeclarator
    { _conversionDeclaratorPos      :: SourcePos
    , _conversionDeclaratorOperator :: PtrOperator
    , _conversionDeclaratorRest     :: Maybe ConversionDeclarator
    }
  deriving (Show, Eq)

data CtorInitializer =
  CtorInitializer
    { _ctorInitializerPos  :: SourcePos
    , _ctorInitializerList :: MemInitializerList
    }
  deriving (Show, Eq)

data MemInitializerList =
  MemInitializerList
    { _memInitializerListPos          :: SourcePos
    , _memInitializerListValue        :: [MemInitializer]
    , _memInitializerListHasThreeDots :: Bool
    }
  deriving (Show, Eq)

data MemInitializer
  = MemInitializerExpression
      { _memInitializerExpressionPos   :: SourcePos
      , _memInitializerExpressionId    :: MemInitializerId
      , _memInitializerExpressionValue :: Maybe ExpressionList
      }
  | MemInitializerBracedList
      { _memInitializerBracedListPos   :: SourcePos
      , _memInitializerBracedListId    :: MemInitializerId
      , _memInitializerBracedListValue :: BracedInitList
      }
  deriving (Show, Eq)

data MemInitializerId =
  MemInitializerId
    { _memInitializerIdPos   :: SourcePos
    , _memInitializerIdValue :: Either ClassOrDecltype Identifier
    }
  deriving (Show, Eq)

data OperatorFunctionId
  = OperatorFunctionId
      { _operatorFunctionIdPos   :: SourcePos
      , _operatorFunctionIdValue :: OverloadableOperator
      }
  | TemplateOperatorFunctionId
      { _templateOperatorFunctionIdPos       :: SourcePos
      , _templateOperatorFunctionIdValue     :: OverloadableOperator
      , _templateOperatorFunctionIdArguments :: Maybe TemplateArgumentList
      }
  deriving (Show, Eq)

data OverloadableOperator =
  OverloadableOperator
    { _overloadableOperatorPos   :: SourcePos
    , _overloadableOperatorValue :: OverloadableOperatorType
    }
  deriving (Show, Eq)

data OverloadableOperatorType
  = OperatorNew
  | OperatorDelete
  | OperatorNewArray
  | OperatorDeleteArray
  | OperatorPlus
  | OperatorMinus
  | OperatorMultiply
  | OperatorDivide
  | OperatorRem
  | OperatorBitAnd
  | OperatorBitXor
  | OperatorBitOr
  | OperatorBitNot
  | OperatorLogicalNot
  | OperatorAssign
  | OperatorGreater
  | OperatorLess
  | OperatorAddAssign
  | OperatorSubAssign
  | OperatorMulAssign
  | OperatorDivAssign
  | OperatorRemAssign
  | OperatorBitXorAssign
  | OperatorBitAndAssign
  | OperatorBitOrAssign
  | OperatorLeftShift
  | OperatorRightShift
  | OperatorLeftShiftAssign
  | OperatorRightShiftAssign
  | OperatorEqual
  | OperatorNotEqual
  | OperatorLessOrEqual
  | OperatorGreaterOrEqual
  | OperatorLogicalAnd
  | OperatorLogicalOr
  | OperatorIncrement
  | OperatorDecrement
  | OperatorComma
  | OperatorAccess
  | OperatorAccessPtr
  | OperatorCall
  | OperatorIndex
  deriving (Show, Eq)

data LiteralOperatorId =
  LiteralOperatorId
    { _literalOperatorIdPos   :: SourcePos
    , _literalOperatorIdValue :: Identifier
    }
  deriving (Show, Eq)

data TemplateParameter
  = TemplateTypeParameter
      { _templateTypeParameterPos   :: SourcePos
      , _templateTypeParameterValue :: TypeParameter
      }
  | TemplateParameterDeclaration
      { _templateParameterDeclarationPos   :: SourcePos
      , _templateParameterDeclarationValue :: ParameterDeclaration
      }
  deriving (Show, Eq)

data TypeParameter
  --  	class ...opt identifier[opt]     C++0x
  = ClassParameter
      { _classParameterPos          :: SourcePos
      , _classParameterHasThreeDots :: Bool
      , _classParameterId           :: Maybe Identifier
      }
  --  	class identifier[opt] = type-id
  | ClassWithIdParameter
      { _classWithIdParameterPos    :: SourcePos
      , _classWithIdParameterId     :: Maybe Identifier
      , _classWithIdParameterTypeId :: TypeId
      }
  --  	typename ...opt identifier[opt]     C++0x
  | TypenameParameter
      { _typenameParameterPos          :: SourcePos
      , _typenameParameterHasThreeDots :: Bool
      , _typenameParameterId           :: Maybe Identifier
      }
  --  	typename identifier[opt] = type-id
  | TypenameWithIdParameter
      { _typenameWithIdParameterPos    :: SourcePos
      , _typenameWithIdParameterId     :: Maybe Identifier
      , _typenameWithIdParameterTypeId :: TypeId
      }
  --  	template < template-parameter-list > class ...opt identifier[opt]     C++0x
  | TemplateParameter
      { _templateParameterPos          :: SourcePos
      , _templateParameterList         :: [TemplateParameter]
      , _templateParameterHasThreeDots :: Bool
      , _templateParameterId           :: Maybe Identifier
      }
  --  	template < template-parameter-list > class identifier[opt] = id-expression
  | TemplateWithIdParameter
      { _templateWithIdParameterPos        :: SourcePos
      , _templateWithIdParameterList       :: [TemplateParameter]
      , _templateWithIdParameterId         :: Maybe Identifier
      , _templateWithIdParameterExpression :: Expression
      }
  deriving (Show, Eq)

data SimpleTemplateId =
  SimpleTemplateId
    { _simpleTemplateIdPos       :: SourcePos
    , _simpleTemplateIdName      :: TemplateName
    , _simpleTemplateIdArguments :: Maybe TemplateArgumentList
    }
  deriving (Show, Eq)

data TemplateId
  = TemplateIdSimple
      { _templateIdSimplePos   :: SourcePos
      , _templateIdSimpleValue :: SimpleTemplateId
      }
  | TemplateIdOperator
      { _templateIdOperatorPos       :: SourcePos
      , _templateIdOperatorFunction  :: OperatorFunctionId
      , _templateIdOperatorArguments :: Maybe TemplateArgumentList
      }
  | TemplateIdLiteral
      { _templateIdLiteralPos       :: SourcePos
      , _templateIdLiteralOperator  :: LiteralOperatorId
      , _templateIdLiteralArguments :: Maybe TemplateArgumentList
      }
  deriving (Show, Eq)

data TemplateName =
  TemplateName
    { _templateNamePos   :: SourcePos
    , _templateNameValue :: Identifier
    }
  deriving (Show, Eq)

data TemplateArgumentList =
  TemplateArgumentList
    { _templateArgumentListPos          :: SourcePos
    , _templateArgumentListValue        :: [TemplateArgument]
    , _templateArgumentListHasThreeDots :: Bool
    }
  deriving (Show, Eq)

data TemplateArgument =
  TemplateArgument
    { _templateArgumentPos   :: SourcePos
    , _templateArgumentValue :: Either Expression TypeId
    }
  deriving (Show, Eq)

data TypenameSpecifier
  = TypenameSpecifier
      { _typenameSpecifierPos          :: SourcePos
      , _typenameSpecifierHasSquareDot :: Bool
      , _typenameSpecifierNestedName   :: NestedNameSpecifier
      , _typenameSpecifierId           :: Identifier
      }
  | TypenameSpecifierTemplate
      { _typenameSpecifierTemplatePos           :: SourcePos
      , _typenameSpecifierTemplateHasSquareDot  :: Bool
      , _typenameSpecifierTemplateNestedName    :: NestedNameSpecifier
      , _typenameSpecifierTemplateHasTemplateKW :: Bool
      , _typenameSpecifierTemplateId            :: SimpleTemplateId
      }
  deriving (Show, Eq)

data TryBlock =
  TryBlock
    { _tryBlockPos      :: SourcePos
--    , _tryAttributes    :: [AttributeSpecifier]
    , _tryBlockCompound :: Statement
    , _tryBlockHandlers :: [Handler]
    }
  deriving (Show, Eq)

data FunctionTryBlock =
  FunctionTryBlock
    { _functionTryBlockPos             :: SourcePos
    , _functionTryBlockCtorInitializer :: Maybe CtorInitializer
    , _functionTryBlockCompound        :: Statement
    , _functionTryBlockHandlers        :: [Handler]
    }
  deriving (Show, Eq)

data Handler =
  Handler
    { _handlerPos         :: SourcePos
    , _handlerDeclaration :: ExceptionDeclaration
    , _handlerCompound    :: Statement
    }
  deriving (Show, Eq)

data ExceptionDeclaration
  = ExceptionDeclaration
      { _exceptionDeclarationPos            :: SourcePos
      , _exceptionDeclarationAttributes     :: [AttributeSpecifier]
      , _exceptionDeclarationTypeSpecifiers :: TypeSpecifierSeq
      , _exceptionDeclarationDeclarator     :: Declarator
      }
  | ExceptionDeclarationAbstract
      { _exceptionDeclarationAbstractPos            :: SourcePos
      , _exceptionDeclarationAbstractAttributes     :: [AttributeSpecifier]
      , _exceptionDeclarationAbstractTypeSpecifiers :: TypeSpecifierSeq
      , _exceptionDeclarationAbstractDeclarator     :: Maybe AbstractDeclarator
      }
  | ThreeDotDeclaration
      { _threeDotDeclarationPos :: SourcePos
      }
  deriving (Show, Eq)

data ExceptionSpecification =
  ExceptionSpecification
    { _exceptionSpecificationPos :: SourcePos
    , _exceptionSpecificationValue :: Either DynamicExceptionSpecification NoexceptSpecification
    }
  deriving (Show, Eq)

data DynamicExceptionSpecification =
  DynamicExceptionSpecification
    { _dynamicExceptionSpecificationPos        :: SourcePos
    , _dynamicExceptionSpecificationTypeIdList :: Maybe TypeIdList
    }
  deriving (Show, Eq)

data TypeIdList =
  TypeIdList
    { _typeIdListPos         :: SourcePos
    , _typeIdListValue       :: [TypeId]
    , _typeIdListHasThreeDot :: Bool
    }
  deriving (Show, Eq)

data NoexceptSpecification
  = NoexceptSpecification
      { _noexceptSpecificationPos        :: SourcePos
      , _noexceptSpecificationExpression :: Expression
      }
  | NoexceptSpecificationEmpty
      { _noexceptSpecificationEmptyPos :: SourcePos
      }
  deriving (Show, Eq)
