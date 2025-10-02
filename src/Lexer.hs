module Lexer where

import Data.Char

data TokenTypes
  = Exit
  | Return
  | IntLit -- integer literal
  | StringLit
  | CharLit
  | FloatLit
  | HexLit --pointers i guess
  | Ident -- identifier ; also used for Labels
  | Label
  | Colon --used for Labels
  | TrueLit
  | FalseLit
  | Semi -- semicolon
  | Comma
  | OpenCurly -- {
  | CloseCurly -- }
  | OpenParen -- (
  | CloseParen -- }
  | OpenSquare --[
  | CloseSquare --]
  | Type --change to i32 like syntax
  | Equal -- assign =
  | Equals -- compare ==
  | Greater
  | Lesser
  | Grequal -- >=
  | Lequal -- =<
  | Nequals -- !=
  | And -- &&
  | Or -- ||
  | Not -- !
  | BOr -- |
  | BNot -- ~
  | BXor -- ^
  | Plus
  | Minus
  | AddAssign
  | SubAssign
  | Star --mult & dereference
  | FSlash -- /
  | MultAssign
  | DivAssign
  | Ampersand -- &; BAnd
  | Percentage -- % ; modulo
  | If
  | ElIf
  | Else
  | Goto
  | Free --munmap
  | Alloc --mmap
  | SizeOf
  | RShift
  | LShift
  | Include
  | Define -- define pi 3.14 
  | Write -- console & files
  | Read --       ""
  | System -- call terminal
  | Open -- files
  | Close -- files
  | Call --custom syscall
  | HardwareReg --rax, rbx, rcx, etc.
  | DotDefine {-.data, .bss, .define -}
  | DotData
  | DotBSS
  | DotStart
  deriving (Show, Eq)

data Token = Token {tokenType :: TokenTypes, line_ :: Int, value :: String} | EmptyToken deriving (Show, Eq)

tokenize :: String -> Int -> Int -> [Token] -> [Token]
tokenize src index line tokens
      | (peek index 0) == '0' && (peek index 1) == 'x'
      = let buf_index = completeHex (index + 2) ""
            newbuf = fst buf_index
            new_index = snd buf_index
            new_tokens = (Token {tokenType = HexLit, line_ = line, value = newbuf}) : tokens
        in tokenize src new_index line new_tokens

      | (peek index 0) == '"'
      = let tok_index = readStr (index + 1) line ""
            token = fst tok_index
            new_index = snd tok_index
            new_tokens = token : tokens
        in tokenize src new_index line new_tokens


      | (peek index 0) == '.'
      = let (buf, idx) = completeWord (index + 1) ""
            token = case buf of
               "data" -> Token DotData line "0"
               "bss" -> Token DotBSS line "0"
               "define" -> Token DotDefine line "0"
               "start" -> Token DotStart line "0"
            new_tokens = token : tokens
        in tokenize src idx line new_tokens

      | (peek index 0) == '%'
      = let (buf, idx) = completeWord (index + 1) ""
            token = Token HardwareReg line buf
            new_tokens = token : tokens
        in tokenize src idx line new_tokens

      | ((peek index 0) == '#' && isAlphaNum (peek index 1))
      = let (buf, index2) = completeWord (index+1) ""
            new_tokens = (Token {tokenType = Label, line_ = line, value = buf}) : tokens
        in tokenize src index2 line new_tokens

      | isAlpha (peek index 0)
      = let buf_index = completeWord index ""
            newbuf = fst buf_index
            new_index = snd buf_index
            new_tokens = (identifyKeywords newbuf line) : tokens
        in tokenize src new_index line new_tokens

      | isDigit (peek index 0)
      = let tok_index = completeNumber index ""
            token = fst tok_index
            new_index = snd tok_index
            new_tokens = token : tokens
        in tokenize src new_index line new_tokens

      | (peek index 0) == '/' && (peek index 1) == '/'
      = let temp_index = index + 2
            new_index = skipComments temp_index
        in tokenize src new_index line tokens

      | (peek index 0) == '\n'
      = tokenize src (index+1) (line+1) tokens

      | (fst (charExp index line)) /= EmptyToken
      = let (token, index2)= (charExp index line)
        in tokenize src index2 line (token : tokens)

      | (peek index 0) == '\'' -- if it doesnt work with this approaach i just check elem \ buf
      = let new_tokens = (Token {tokenType = CharLit, line_ = line, value = [(peek index 1)]}) : tokens 
        in tokenize src (index + 3) line new_tokens

      | (peek index 0) /= '\0'
      = tokenize src (index+1) line tokens

      | otherwise
      = tokens

  where
    peek :: Int -> Int -> Char
    peek index offset =
      if (index + offset >= length src) || (index + offset < 0)
        then '\0'
        else src !! (index + offset)

    completeWord :: Int -> String -> (String, Int)
    completeWord index buf =
      if ((peek index 0) /= '\0') && isAlphaNum (peek index 0) || (peek index 0) == '_'
        then
          let newbuf = (peek index 0) : buf
           in completeWord (index + 1) newbuf
        else (reverse buf, index)

    completeNumber :: Int -> String -> (Token, Int)
    completeNumber index buf =
      if ((peek index 0) == '.') || ((peek index 0) /= '\0' && isDigit (peek index 0))
        then
          let newbuf = (peek index 0) : buf
           in completeNumber (index + 1) newbuf
        else if elem '.' buf
               then (Token {tokenType = FloatLit, line_ = line, value = reverse buf}, index)
               else (Token {tokenType = IntLit, line_ = line, value = reverse buf}, index)

    completeHex :: Int -> String -> (String, Int)
    completeHex index buf =
      if ((peek index 0) /= '\0') && isHexDigit (peek index 0)
        then
          let newbuf = (peek index 0) : buf
           in completeHex (index + 1) newbuf
        else (reverse buf, index)

    identifyKeywords :: String -> Int -> Token
    identifyKeywords "exit" line = Token {tokenType = Exit, line_ = line, value = "0"}
    identifyKeywords "int" line = Token {tokenType = Type, line_ = line, value = "int"}
    identifyKeywords "char" line = Token {tokenType = Type, line_ = line, value = "char"}
    identifyKeywords "string" line = Token {tokenType = Type, line_ = line, value = "string"}
    identifyKeywords "bool" line = Token {tokenType = Type, line_ = line, value = "bool"}
    identifyKeywords "ptr" line = Token {tokenType = Type, line_ = line, value = "ptr"}
    identifyKeywords "float" line = Token {tokenType = Type, line_ = line, value = "float"}
    identifyKeywords "double" line = Token {tokenType = Type, line_ = line, value = "double"}
    identifyKeywords "else" line = Token {tokenType = Else, line_ = line, value = "0"}
    identifyKeywords "elif" line = Token {tokenType = ElIf, line_ = line, value = "0"}
    identifyKeywords "if" line = Token {tokenType = If, line_ = line, value = "0"}
    identifyKeywords "goto" line = Token {tokenType = Goto, line_ = line, value = "0"}
    identifyKeywords "write" line = Token {tokenType = Write, line_ = line, value = "0"}
    identifyKeywords "read" line = Token {tokenType = Read, line_ = line, value = "0"}
    identifyKeywords "close" line = Token {tokenType = Close, line_ = line, value = "0"}
    identifyKeywords "open" line = Token {tokenType = Open, line_ = line, value = "0"}
    identifyKeywords "call" line = Token {tokenType = Call, line_ = line, value = "0"}
    identifyKeywords "system" line = Token {tokenType = System, line_ = line, value = "0"}
    identifyKeywords "sizeof" line = Token {tokenType = SizeOf, line_ = line, value = "0"}
    identifyKeywords "free" line = Token {tokenType = Free, line_ = line, value = "0"}
    identifyKeywords "malloc" line = Token {tokenType = Alloc, line_ = line, value = "0"}
    identifyKeywords "true" line = Token {tokenType = TrueLit, line_ = line, value = "0"}
    identifyKeywords "false" line = Token {tokenType = FalseLit, line_ = line, value = "0"}
    identifyKeywords "include" line = Token {tokenType = Include, line_ = line, value = "0"}
    identifyKeywords "return" line = Token {tokenType = Return, line_ = line, value = "0"}
    identifyKeywords "define" line = Token Define line "0"
    identifyKeywords buf line = Token {tokenType = Ident, line_ = line, value = buf}

    skipComments :: Int -> Int
    skipComments index =
      if (peek index 0) /= '\0' && (peek index 0) /= '\n'
        then skipComments (index + 1)
        else index

    readStr :: Int -> Int -> String -> (Token, Int)
    readStr index line buf =
      if (peek index 0) /= '\0' && (peek index 0) /= '"'
        then readStr (index + 1) line ((peek index 0) : buf)
        else (Token {tokenType = StringLit, line_ = line, value = reverse buf}, index + 1)

    charExp :: Int -> Int -> (Token, Int)
    charExp index line
      | (peek index 0) == '>' && (peek index 1) == '>' = (Token {tokenType = RShift, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '<' && (peek index 1) == '<' = (Token {tokenType = LShift, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '=' && (peek index 1) == '=' = (Token {tokenType = Equals, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '>' && (peek index 1) == '=' = (Token {tokenType = Grequal, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '=' && (peek index 1) == '<' = (Token {tokenType = Lequal, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '|' && (peek index 1) == '|' = (Token {tokenType = Or, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '&' && (peek index 1) == '&' = (Token {tokenType = And, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '!' && (peek index 1) == '=' = (Token {tokenType = Nequals, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '+' && (peek index 1) == '=' = (Token {tokenType = AddAssign, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '-' && (peek index 1) == '=' = (Token {tokenType = SubAssign, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '*' && (peek index 1) == '=' = (Token {tokenType = MultAssign, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '/' && (peek index 1) == '=' = (Token {tokenType = DivAssign, line_ = line, value = "0"}, index + 2)
      | (peek index 0) == '(' = (Token {tokenType = OpenParen, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == ')' = (Token {tokenType = CloseParen, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '{' = (Token {tokenType = OpenCurly, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '}' = (Token {tokenType = CloseCurly, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '[' = (Token {tokenType = OpenSquare, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == ']' = (Token {tokenType = CloseSquare, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == ';' = (Token {tokenType = Semi, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '=' = (Token {tokenType = Equal, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '+' = (Token {tokenType = Plus, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '-' = (Token {tokenType = Minus, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '*' = (Token {tokenType = Star, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '/' = (Token {tokenType = FSlash, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '!' = (Token {tokenType = Not, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '&' = (Token {tokenType = Ampersand, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '~' = (Token {tokenType = BNot, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '<' = (Token {tokenType = Lesser, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '>' = (Token {tokenType = Greater, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '|' = (Token {tokenType = BOr, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '%' = (Token {tokenType = Percentage, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == ':' = (Token {tokenType = Colon, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == '^' = (Token {tokenType = BXor, line_ = line, value = "0"}, index + 1)
      | (peek index 0) == ',' = (Token {tokenType = Comma, line_ = line, value = "0"}, index + 1)
      | otherwise = (EmptyToken, -1)

tokenizeSrc :: String -> [Token]
tokenizeSrc str = reverse (tokenize str 0 1 [])

 {-gprRegisters = --general purpose register Registers
  [ "rax", "eax", "ax", "al", "ah"
  , "rbx", "ebx", "bx", "bl", "bh"
  , "rcx", "ecx", "cx", "cl", "ch"
  , "rdx", "edx", "dx", "dl", "dh"
  , "rsi", "esi", "si", "sil"
  , "rdi", "edi", "di", "dil"
  , "rbp", "ebp", "bp", "bpl"
  , "rsp", "esp", "sp", "spl"
  , "r8",  "r8d",  "r8w",  "r8b"
  , "r9",  "r9d",  "r9w",  "r9b"
  , "r10", "r10d", "r10w", "r10b"
  , "r11", "r11d", "r11w", "r11b"
  , "r12", "r12d", "r12w", "r12b"
  , "r13", "r13d", "r13w", "r13b"
  , "r14", "r14d", "r14w", "r14b"
  , "r15", "r15d", "r15w", "r15b"
  ]-}

gprRegisters = --no longer just gpr but i dont want to change the name
  [ -- 8-bit registers
    "al", "cl", "dl", "bl"
  , "ah", "ch", "dh", "bh"
  , "spl", "bpl", "sil", "dil"
  , "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"

    -- 16-bit registers
  , "ax", "bx", "cx", "dx"
  , "sp", "bp", "si", "di"
  , "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"

    -- 32-bit registers
  , "eax", "ebx", "ecx", "edx"
  , "esp", "ebp", "esi", "edi"
  , "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"

    -- 64-bit registers
  , "rax", "rbx", "rcx", "rdx"
  , "rsp", "rbp", "rsi", "rdi"
  , "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"

    -- Segment registers
  , "cs", "ds", "ss", "es", "fs", "gs"

    -- Instruction pointer / flags
  , "rip", "eip", "ip"      -- Instruction pointer ; dont write to these
  , "rflags", "eflags", "flags" -- Flags register
  ]

