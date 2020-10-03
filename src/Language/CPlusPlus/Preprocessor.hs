module Language.CPlusPlus.Preprocessor where

import           Text.Parsec

import           Language.CPlusPlus.AST
import           Language.CPlusPlus.Internal.Base
import           Language.CPlusPlus.Internal.Lexer
import           Language.CPlusPlus.Internal.Types.Lexer

newtype PreprocessingFile = PreprocessingFile [GroupPart] deriving (Show, Eq)

data GroupPart
  = IfSectionPart
  { _ifSectionPartPos   :: SourcePos
  , _ifSectionPartValue :: IfSection
  }
  | ControlLinePart
  { _controlLinePos   :: SourcePos
  , _controlLineValue :: ControlLine
  }
  | TextLinePart
  { _textLinePartPos   :: SourcePos
  , _textLinePartValue :: TextLine
  }
  | NonDirectivePart 
  { _nonDirectivePartPos   :: SourcePos
  , _nonDirectivePartValue :: NonDirective
  }
  deriving (Show, Eq)

data IfSection = IfSection SourcePos IfGroup [ElifGroup] (Maybe ElseGroup) deriving (Show, Eq)

data IfGroup
  = IfGroup SourcePos Expression [GroupPart]
  | IfdefGroup SourcePos Identifier [GroupPart]
  | IfndefGroup SourcePos Identifier [GroupPart]
  deriving (Show, Eq)

data ElifGroup = ElifGroup SourcePos Expression [GroupPart] deriving (Show, Eq)

data ElseGroup = ElseGroup SourcePos [GroupPart] deriving (Show, Eq)

newtype EndifLine = EndifLine SourcePos deriving (Show, Eq)

data ControlLine
  = Include
  | DefineId
  | DefineFun
  | Undef
  | Line
  | Error
  | Pragma
  | Empty
  deriving (Show, Eq)

data TextLine = TextLine deriving (Show, Eq)

data NonDirective = NonDirective deriving (Show, Eq)

data PreprocessingToken = PreprocessingToken deriving (Show, Eq)

-- cpp
-- preprocessing-file:	 
--     group[opt]
preprocessingFile :: P PreprocessingFile
preprocessingFile = PreprocessingFile <$> option [] group

-- group:	 
--     group-part
--     group group-part
group :: P [GroupPart]
group = many1 groupPart

-- group-part:	 
--     if-section
--     control-line
--     text-line     C++0x
--     # non-directive     C++0x
groupPart :: P GroupPart
groupPart = undefined

-- if-section:	 
--     if-group elif-groups[opt] else-group[opt] endif-line
ifSection :: P IfSection
ifSection = undefined

-- if-group:	 
--     # if constant-expression new-line group[opt]
--     # ifdef identifier new-line group[opt]
--     # ifndef identifier new-line group[opt]
ifGroup :: P IfGroup
ifGroup = undefined

-- elif-groups:	 
--     elif-group
--     elif-groups elif-group
elifGroups :: P [ElifGroup]
elifGroups = many1 elifGroup

-- elif-group:	 
--     # elif constant-expression new-line group[opt]
elifGroup :: P ElifGroup
elifGroup = undefined

-- else-group:	 
--     # else new-line group[opt]
elseGroup :: P ElseGroup
elseGroup = undefined

-- endif-line:	 
--     # endif new-line
endifLine :: P EndifLine
endifLine = undefined

-- control-line:	 
--     # include pp-tokens new-line
--     # define identifier replacement-list new-line
--     # define identifier lparen identifier-list[opt] ) replacement-list new-line     C++0x
--     # define identifier lparen identifier-list , ... ) replacement-list new-line     C++0x
--     # undef identifier new-line
--     # line pp-tokens new-line
--     # error pp-tokens[opt] new-line
--     # pragma pp-tokens[opt] new-line
--     # new-line
controlLine :: P ControlLine
controlLine = undefined

-- text-line:	 
--     pp-tokens[opt] new-line     C++0x
textLine :: P TextLine
textLine = undefined

-- non-directive:	 
--     pp-tokens new-line     C++0x
nonDirective :: P NonDirective
nonDirective = undefined

-- lparen:	 
--     a ( character not immediately preceded by white-space
-- identifier-list:	 
--     identifier
--     identifier-list , identifier
identifierList :: P [Identifier]
identifierList = identifier `sepBy1` comma

-- replacement-list:	 
--     pp-tokens[opt]
replacementList :: P [PreprocessingToken]
replacementList = option [] ppTokens

-- pp-tokens:	 
--     preprocessing-token
--     pp-tokens preprocessing-token
ppTokens :: P [PreprocessingToken]
ppTokens = many1 preprocessingToken

preprocessingToken :: P PreprocessingToken
preprocessingToken = undefined

-- new-line:	 
--     the new-line character
newLine :: P ()
newLine = eol
