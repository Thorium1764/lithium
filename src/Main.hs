module Main (main) where

import Lexer
import Parser
import Analyzer

main :: IO ()
main = do
  src <- getLine --readFile "/home/tobias/programming/lithium/example.li" --"// example struct ; struct stores 2 * T \n declare_struct: \n \t ptr m_1_ptr = alloc(2 * sizeof(T)); \n \t ptr m_2_ptr = m_1_ptr + sizeof(T); \n \t *m_1_ptr = m_1; \n \t *m_2_ptr = m_2;"
  let tokens = tokenizeSrc src
  ast <- parse tokens
  ast_analyzed <- analyze ast
  print ast
