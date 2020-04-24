module Language.CPlusPlus.Comment where

import           Text.Parsec
import Text.Parsec.Pos

import           Language.CPlusPlus.Internal.Types.Lexer

newtype CommentMetadata = Comments [Comment] deriving (Show, Eq)

data Comment = Comment SourcePos String deriving (Show, Eq)

extractComments :: [CppToken] -> ([CppToken], CommentMetadata)
extractComments tokens =
  let isComment t = case t of
        L _ (TComment     _) -> True
        L _ (TLineComment _) -> True
        _                    -> False
      replaceEOL :: CppToken -> CppToken
      replaceEOL (L p t) = L p EOL
      comment (L (line, column) (TComment str)) = Comment (newPos "" line column) str
      extract []       comments rest = (reverse rest, reverse comments)
      extract (t : ts) comments rest = if isComment t
        then extract ts (comment t : comments) (replaceEOL t : rest)
        else extract ts comments (t : rest)
      meta (rest, list) = (rest, Comments list)
  in  meta (extract tokens [] [])
