module Main where

import Language.CPlusPlus.Internal.Lexer

test :: String
test = "{}"

main :: IO ()
main = do 
  let res = lexer test
  print res
