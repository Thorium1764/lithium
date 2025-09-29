module Parser (
  parse,
)
where

import Lexer
import System.IO

-- define node types
data NodeTerm = Identifier Token 
              | RegIdentifier Token
              | Literal Token 
              | ParenExpr NodeExpr  
              | TermRead NodeExpr
              | TermOpen NodeExpr
              | TermSystem NodeExpr
              | TermHeapAlloc NodeExpr
              | TermStackAlloc NodeExpr
              | TermCall [NodeExpr] --missing
              | UnExpr UnOp NodeExpr
              | Sizeof Type
  deriving (Show)

data NodeExpr = TermExpr NodeTerm | BinExpr BinOp NodeExpr NodeExpr
  deriving (Show)

data BinOp = Add | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq | Neq | AndOp | OrOp | BOrOp | BXorOp | BAndOp | Mod | LShiftOp | RShiftOp
  deriving (Show)

data UnOp = NotOp | BNotOp | Deref | Addr | Neg
   deriving (Show)

data NodeStmt
  = StmtExit NodeExpr
  | StmtLet Token NodeExpr Type
  | StmtLetReg Token NodeExpr Type
  | StmtScope NodeScope
  | StmtIf NodeExpr NodeScope (Maybe NodeIfPred)
  | StmtAssign Token NodeExpr
  | StmtAsReg Token NodeExpr
  | StmtPtrAs Token NodeExpr --in future also assign ptrs with: store value, ptr
  | StmtPtrAsReg Token NodeExpr
  | StmtLabel Token NodeScope
  | StmtGoto Token
  | StmtFree NodeExpr
  | StmtInclude String
  | StmtWrite NodeExpr NodeExpr
  | StmtClose NodeExpr
  | StmtReturn
  deriving (Show)

data Type
   = Int_t --int
   | Float_t --float
   | Double_t --double
   | Char_t --char
   | String_t --string
   | Pointer_t --ptr
   deriving (Show)

data NodeIfPred
  = NodeIfPredElse NodeScope
  | NodeIfPredElif NodeExpr NodeScope (Maybe NodeIfPred)
  deriving (Show)

type NodeProg = [NodeStmt]
type NodeScope = [NodeStmt]

-- define parsing functions
parse :: [Token] -> Either String NodeProg
parse tokens = case parseProg tokens 0 [] of
  Right prog -> Right (reverse prog)
  Left err -> Left err

parseProg :: [Token] -> Int -> NodeProg -> Either String NodeProg
parseProg tokens index prog =
  if (peek index 0) /= EmptyToken
    then do
      (stmt, new_index) <- parseStmt tokens index
      parseProg tokens new_index (stmt : prog)
    else Right prog
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

parseStmt :: [Token] -> Int -> Either String (NodeStmt, Int)
parseStmt tokens index
  | (tokenType (peek index 0)) == RegIdent && (tokenType (peek index 1)) == Colon && (tokenType (peek index 2)) == Type && (tokenType (peek index 3)) == Equal = 
      do
        (expr, index1) <- parseExpr tokens (index + 4) 0
        index2 <- expect Semi index1
        let ident = peek index 0
            dataType = strtotype $ value (peek index 2)
        pure (StmtLetReg ident expr dataType, index2)
  | (tokenType (peek index 0)) == Ident && (tokenType (peek index 1)) == Colon && (tokenType (peek index 2)) == Type && (tokenType (peek index 3)) == Equal = 
      do
        (expr, index1) <- parseExpr tokens (index + 4) 0
        index2 <- expect Semi index1
        let ident = peek index 0
            dataType = strtotype $ value (peek index 2)
        pure (StmtLet ident expr dataType, index2)
  | (tokenType (peek index 0)) == Exit && (tokenType (peek index 1)) == OpenParen =
      do
        (expr, index1) <- parseExpr tokens (index + 2) 0
        index2 <- expect CloseParen index1
        index3 <- expect Semi index2
        pure (StmtExit expr, index3)
  | (tokenType (peek index 0)) == RegIdent && (tokenType (peek index 1)) == Equal =
      do
        (expr, index1) <- parseExpr tokens (index + 2) 0
        index2 <- expect Semi index1
        pure (StmtAsReg (peek index 0) expr, index2)
  | (tokenType (peek index 0)) == Star && (tokenType (peek index 1)) == Ident && (tokenType $ peek index 2) == Equal =
      do 
        (expr, index1) <- parseExpr tokens (index + 3) 0
        index2 <- expect Semi index1
        pure (StmtPtrAs (peek index 1) expr, index2)
  | (tokenType (peek index 0)) == Star && (tokenType (peek index 1)) == RegIdent && (tokenType $ peek index 2) == Equal =
      do
        (expr, index1) <- parseExpr tokens (index + 3) 0
        index2 <- expect Semi index1
        pure (StmtPtrAsReg (peek index 1) expr, index2)
  | (tokenType (peek index 0)) == Ident && (tokenType (peek index 1)) == Equal =
      do
        (expr, index1) <- parseExpr tokens (index + 2) 0
        index2 <- expect Semi index1
        pure (StmtAssign (peek index 0) expr, index2)
  | (tokenType (peek index 0)) == OpenCurly =
      do
        (scope, index1) <- parseScope tokens index
        pure (StmtScope scope, index1)
  | (tokenType (peek index 0)) == If =
      do
        index1 <- expect OpenParen (index + 1)
        (expr, index2) <- parseExpr tokens index1 0
        index3 <- expect CloseParen index2
        (scope, index4) <- parseScope tokens index3
        case parseIfPred tokens index4 of
          Left _ -> pure (StmtIf expr scope Nothing, index4)
          Right (if_pred, index5) -> pure (StmtIf expr scope (Just if_pred), index5)
  | (tokenType (peek index 0)) == Goto && (tokenType (peek index 1)) == Label =  
      do 
        index2 <- expect Semi (index + 2)
        pure (StmtGoto (peek index 1), index2)
  | (tokenType (peek index 0)) == Label && (tokenType $ peek index 1) == Colon = 
      do 
        (scope, index2) <- parseScope tokens (index + 2)
        pure (StmtLabel (peek index 0) scope, index2)
  | (tokenType (peek index 0)) == Free && (tokenType (peek index 1)) == OpenParen =
      do
        (expr, index1) <- parseExpr tokens (index + 2) 0
        index2 <- expect CloseParen index1
        index3 <- expect Semi index2
        pure (StmtFree expr, index3)
  | (tokenType (peek index 0)) == Include =
      do 
        (index2) <- expect StringLit (index+1)
        (index3) <- expect Semi index2
        pure (StmtInclude (value $ peek index 1), index3)
  | (tokenType $ peek index 0) == Write && (tokenType $ peek index 1) == OpenParen =
      do 
        (fd, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect Comma index2
        (string, index4) <- parseExpr tokens index3 0
        index5 <- expect CloseParen index4
        index6 <- expect Semi index5
        pure (StmtWrite fd string, index6)
  | (tokenType $ peek index 0) == Close && (tokenType $ peek index 1) == OpenParen = 
      do 
        (fd, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        index4 <- expect Semi index3
        pure (StmtClose fd, index4)
  | (tokenType $ peek index 0) == Return =
      do 
        index2 <- expect Semi (index + 1)
        pure (StmtReturn, index2)
  | otherwise = Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " at Line " ++ show (line_ $ peek index 0))
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

  expect :: TokenTypes -> Int -> Either String Int
  expect expected index =
    if (peek index 0) /= EmptyToken
      then if (tokenType $ peek index 0) == expected
         then Right (index + 1)
         else Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " instead of " ++ show expected ++ " at Line " ++ show (line_ $ peek index 0))
      else Left ("Error: Missing: " ++ show expected)

parseScope :: [Token] -> Int -> Either String (NodeScope, Int)
parseScope tokens index = do
  index1 <- expect OpenCurly index
  (scope, index2) <- readStmts index1 []
  index3 <- expect CloseCurly index2
  pure (scope, index3)
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

  expect :: TokenTypes -> Int -> Either String Int
  expect expected index =
    if (peek index 0) /= EmptyToken
      then if (tokenType $ peek index 0) == expected
         then Right (index + 1)
         else Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " instead of " ++ show expected ++ " at Line " ++ show (line_ $ peek index 0))
      else Left ("Error: Missing: " ++ show expected)

  readStmts :: Int -> NodeScope -> Either String (NodeScope, Int)
  readStmts index scope =
    case parseStmt tokens index of
      Left _ -> Right (reverse scope, index)
      Right (stmt, idx) -> readStmts idx (stmt : scope)

parseIfPred :: [Token] -> Int -> Either String (NodeIfPred, Int)
parseIfPred tokens index
  | (tokenType (peek index 0)) == ElIf =
      do
        index1 <- expect OpenParen (index + 1)
        (expr, index2) <- parseExpr tokens index1 0
        index3 <- expect CloseParen index2
        (scope, index4) <- parseScope tokens index3 
        case parseIfPred tokens index4 of
          Left _ -> pure (NodeIfPredElif expr scope Nothing, index4)
          Right (pred, index5) -> pure (NodeIfPredElif expr scope (Just pred), index5)
  | (tokenType (peek index 0)) == Else =
      do
        (scope, index1) <- parseScope tokens (index + 1)
        pure (NodeIfPredElse scope, index1)
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

  expect :: TokenTypes -> Int -> Either String Int
  expect expected index =
    if (peek index 0) /= EmptyToken
      then if (tokenType $ peek index 0) == expected
         then Right (index + 1)
         else Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " instead of " ++ show expected ++ " at Line " ++ show (line_ $ peek index 0))
      else Left ("Error: Missing: " ++ show expected)

parseTerm :: [Token] -> Int -> Either String (NodeExpr, Int)
parseTerm tokens index
  | elem (tokenType (peek index 0)) literals =
      do
        pure (TermExpr $ Literal (peek index 0), index + 1)
  | (tokenType (peek index 0)) == Ident =
      do
        pure (TermExpr $ Identifier (peek index 0), index + 1)
  | (tokenType (peek index 0)) == RegIdent =
      do
        pure (TermExpr $ RegIdentifier (peek index 0), index + 1)
  | (tokenType (peek index 0)) == OpenParen =
      do
        (expr, index1) <- parseExpr tokens (index + 1) 0
        index2 <- expect CloseParen index
        pure (TermExpr $ ParenExpr expr, index2)
  | (tokenType $ peek index 0) == SizeOf && (tokenType $ peek index 1) == OpenParen = 
      do 
        index2 <- expect Type (index + 2)
        index3 <- expect CloseParen index2
        pure (TermExpr $ Sizeof (strtotype $ value $ peek index 2), index3)
  | elem (tokenType $ peek index 0) unOperators = 
      do 
        (unexpr, index2) <- parseUnOp index
        pure (TermExpr $ unexpr, index2)
  | (tokenType $ peek index 0) == Read && (tokenType $ peek index 1) == OpenParen =
      do 
        (fd, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        pure (TermExpr $ TermRead fd, index3)
  | (tokenType $ peek index 0) == System && (tokenType $ peek index 1) == OpenParen =
      do 
        (call, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        pure (TermExpr $ TermSystem call, index3)
  | (tokenType $ peek index 0) == Open && (tokenType $ peek index 1) == OpenParen =
      do 
        (fname, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        pure (TermExpr $ TermOpen fname, index3)
  | (tokenType $ peek index 0) == HeapAlloc && (tokenType $ peek index 1) == OpenParen =
      do 
        (size, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        pure (TermExpr $ TermHeapAlloc size, index3)
  | (tokenType $ peek index 0) == StackAlloc && (tokenType $ peek index 1) == OpenParen =
      do 
        (size, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect CloseParen index2
        pure (TermExpr $ TermStackAlloc size, index3)
  | (tokenType $ peek index 0) == Call && (tokenType $ peek index 1) == OpenParen = -- i aint checking this; it stays unsafe
      do 
        (callNum, index2) <- parseExpr tokens (index + 2) 0
        index3 <- expect Comma index2
        (rdi, index4) <- parseExpr tokens (index + 2) 0
        index5 <- expect Comma index4
        (rsi, index6) <- parseExpr tokens (index + 2) 0
        index7 <- expect Comma index6
        (rdx, index8) <- parseExpr tokens (index + 2) 0
        index9 <- expect Comma index8
        (r8, index10) <- parseExpr tokens (index + 2) 0
        index11 <- expect Comma index10
        (r9, index12) <- parseExpr tokens (index + 2) 0
        index13 <- expect Comma index12
        (r10, index14) <- parseExpr tokens (index + 2) 0
        index15 <- expect CloseParen index14
        pure (TermExpr $ TermCall [callNum, rdi, rsi, rdx, r8, r9, r10], index15)
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

  expect :: TokenTypes -> Int -> Either String Int
  expect expected index =
    if (peek index 0) /= EmptyToken
      then if (tokenType $ peek index 0) == expected
         then Right (index + 1)
         else Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " instead of " ++ show expected ++ " at Line " ++ show (line_ $ peek index 0))
      else Left ("Error: Missing: " ++ show expected)

  parseUnOp :: Int -> Either String (NodeTerm, Int)
  parseUnOp idx = do
   (expr, index2) <- parseTerm tokens (idx + 1)
   case (tokenType $ peek idx 0) of
      BNot -> pure (UnExpr BNotOp expr, index2)
      Not -> pure (UnExpr NotOp expr, index2)
      Minus -> pure (UnExpr Neg expr, index2)
      Ampersand -> pure (UnExpr Addr expr, index2)
      Star -> pure (UnExpr Deref expr, index2)
      

parseExpr :: [Token] -> Int -> Int -> Either String (NodeExpr, Int)
parseExpr tokens index minPrec =
  do
    (lhs, index1) <- parseTerm tokens index
    (expr, index2) <- recDesc tokens (index1) minPrec lhs
    pure (expr, index2)
 where
  peek :: Int -> Int -> Token
  peek index offset =
    if index + offset >= length tokens
      then EmptyToken
      else tokens !! (index + offset)

  expect :: TokenTypes -> Int -> Either String Int
  expect expected index =
    if (peek index 0) /= EmptyToken
      then if (tokenType $ peek index 0) == expected
         then Right (index + 1)
         else Left ("Error: Unexpected " ++ show (tokenType $ peek index 0) ++ " instead of " ++ show expected ++ " at Line " ++ show (line_ $ peek index 0))
      else Left ("Error: Missing: " ++ show expected)

  binPrec :: TokenTypes -> Int
  binPrec Or = 0
  binPrec And = 1
  binPrec BOr = 2
  binPrec BXor = 3
  binPrec Ampersand = 4
  binPrec Equals = 5
  binPrec Nequals = 5
  binPrec Lesser = 6
  binPrec Greater = 6
  binPrec Lequal = 6
  binPrec Grequal = 6
  binPrec RShift = 7
  binPrec LShift = 7
  binPrec Plus = 8
  binPrec Minus = 8
  binPrec Star = 9
  binPrec FSlash = 9
  binPrec Percentage = 9
  binPrec _ = -1

  recDesc :: [Token] -> Int -> Int -> NodeExpr -> Either String (NodeExpr, Int)
  recDesc tokens index minPrec lhs = do
    let currTok = peek index 0 --here
    if currTok /= EmptyToken
      then do
        let prec = binPrec (tokenType currTok)
        if prec >= minPrec
          then do
            let tokType     = tokenType (peek index 0)
                index2      = index + 1
                nextMinPrec = prec + 1
            (rhs, index3) <- parseExpr tokens index2 nextMinPrec
            let expr = case tokType of
                  Plus   -> BinExpr Add lhs rhs
                  Minus  -> BinExpr Sub lhs rhs
                  Star   -> BinExpr Mult lhs rhs
                  FSlash -> BinExpr Div lhs rhs
                  Percentage -> BinExpr Mod lhs rhs
                  LShift -> BinExpr LShiftOp lhs rhs
                  RShift -> BinExpr LShiftOp lhs rhs
                  Greater -> BinExpr Gt lhs rhs
                  Lesser -> BinExpr Lt lhs rhs
                  Equals -> BinExpr Eq lhs rhs
                  Nequals -> BinExpr Neq lhs rhs
                  Grequal -> BinExpr Geq lhs rhs
                  Lequal -> BinExpr Leq lhs rhs
                  Or -> BinExpr OrOp lhs rhs
                  And -> BinExpr AndOp lhs rhs
                  Ampersand -> BinExpr BAndOp lhs rhs
                  BOr -> BinExpr BOrOp lhs rhs
                  BXor -> BinExpr BXorOp lhs rhs
            recDesc tokens (index3) minPrec expr
          else pure (lhs, index)
      else pure (lhs, index)

{-
-}

strtotype :: String -> Type
strtotype "int" = Int_t
strtotype "char" = Char_t
strtotype "string" = String_t
strtotype "ptr" = Pointer_t
strtotype "double" = Double_t
strtotype "float" = Float_t
strtotype _ = Int_t

unOperators :: [TokenTypes]
unOperators = [BNot, Not, Ampersand, Star, Minus]

literals :: [TokenTypes]
literals = [IntLit, FloatLit, StringLit, TrueLit, FalseLit, CharLit, HexLit]











