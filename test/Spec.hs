import           Test.HUnit
import           Control.Monad                  ( void )

import qualified Tests.Lexer                   as Lex
import qualified Tests.Parser as Par

tests :: Test
tests = TestList [Lex.tests, Par.tests]

main :: IO ()
main = void $ runTestTT tests
