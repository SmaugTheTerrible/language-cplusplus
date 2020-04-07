import           Test.HUnit
import           Control.Monad                  ( void )

import qualified CPPTest.LexerTest             as Lex

tests :: Test
tests = TestList [Lex.tests]

main :: IO ()
main = void $ runTestTT tests
