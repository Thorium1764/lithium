module Analyzer where

import qualified Data.Map as Map
import Parser
import Lexer

type Table = Map.Map String Type

analyze :: Either String NodeProg -> Either String (NodeProg, Table)
analyze ast = analyzeTree ast 0 Map.empty

analyzeTree :: Either String NodeProg -> Int -> Table -> Either String (NodeProg, Table)
analyzeTree ast index table = Left "WIP" --temp
 where
  peek :: Int -> Int -> Either String NodeStmt
  peek index offset =
   if index + offset >= length ast
      then pure EmptyStmt
      else (!! (index + offset)) <$> ast

  eitherFromMaybe :: Maybe Type -> String -> Either String Type
  eitherFromMaybe Nothing err = Left err
  eitherFromMaybe (Just t) _ = Right t
  
  predictType :: NodeExpr -> Maybe Type
  predictType expr = case expr of 
   TermExpr term -> predictTermType term
   BinExpr op e1 e2 -> checkType op e1 e2 --still missing

  predictTermType :: NodeTerm -> Maybe Type
  predictTermType term = case term of
   Identifier tok -> Map.lookup (value tok) table
   RegIdentifier tok -> Map.lookup (value tok) table
   Literal tok -> Just $ litToTypes $ tokenType $ tok
   ParenExpr expr -> predictType expr
   TermRead _ -> Just String_t
   TermOpen _ -> Just Int_t
   TermSystem _ -> Just String_t
   TermHeapAlloc _ -> Just Pointer_t
   TermStackAlloc _ -> Just Pointer_t
   TermCall _ -> Just Int_t
   UnExpr op expr -> typeUnEx op expr
   Sizeof _ -> Just Int_t

  typeUnEx :: UnOp -> NodeExpr -> Maybe Type --i need to refactor the Maybes to either
  typeUnEx op expr = case (op, expr) of
   (Deref, _) -> Just Any_t
   (Addr, _) -> Just Pointer_t
   (Neg, expr) -> if predictType expr == Just Int_t || predictType expr == Just Float_t || predictType expr == Just Double_t
                     then predictType expr
                     else Nothing
   (NotOp, expr) -> if predictType expr == Just Bool_t
                     then Just Bool_t
                     else Nothing
   (BNotOp, expr) -> predictType expr

  checkType :: BinOp -> NodeExpr -> NodeExpr -> Maybe Type
  checkType op e1 e2
   | op == Mod && type1 == Just Int_t && type2 == Just Int_t = pure (Int_t)
   | op == Add && elem type1 nums && elem type2 nums = max type1 type2
   | op == Sub && elem type1 nums && elem type2 nums = max type1 type2
   | op == Mult && elem type1 arit && elem type2 arit = max type1 type2
   | op == Div && elem type1 arit && elem type2 arit = max type1 type2
   | op == Gt && elem type1 nums && elem type2 nums = Just Bool_t
   | op == Lt && elem type1 nums && elem type2 nums = Just Bool_t
   | op == Geq && elem type1 nums && elem type2 nums = Just Bool_t
   | op == Leq && elem type1 nums && elem type2 nums = Just Bool_t
   | op == Eq = Just Bool_t
   | op == Neq = Just Bool_t
   | op == AndOp && type1 == Just Bool_t && type2 == Just Bool_t = Just Bool_t
   | op == OrOp && type1 == Just Bool_t && type2 == Just Bool_t = Just Bool_t
   | op == BAndOp = type1
   | op == BOrOp = type1
   | op == BXorOp = type1
   | op == LShiftOp && type2 == Just Int_t = Just Int_t
   | op == RShiftOp && type2 == Just Int_t = Just Int_t
   where
      type1 = predictType e1
      type2 = predictType e2
      nums = fmap Just [Int_t, Float_t, Double_t, Pointer_t]
      arit = fmap Just [Int_t,Float_t,Double_t]

litToTypes :: TokenTypes -> Type
litToTypes IntLit = Int_t
litToTypes FloatLit = Float_t
litToTypes CharLit = Char_t
litToTypes StringLit = String_t
litToTypes HexLit = Pointer_t
litToTypes TrueLit = Bool_t
litToTypes FalseLit = Bool_t















































