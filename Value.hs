module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Array [Value]
    | Return Value
    | Break (Maybe Id)
    | Function Id [Id] [Statement]
    | NaoDeclarado
    | Nil
    | Empty
    deriving (Eq)

--
-- Pretty Printer
--

--instance Eq Value where
--    (==) (Bool v1) (Bool v2) = (v1 == v2)

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
--  show (List head tail) = (show head)++(show tail)
  show Nil = "undefined"
  show NaoDeclarado = "Variavel nao Declarada"
  show (Function (Id name) args body) = name ++ "(" ++ showArgs args ++ ")"
  show (Array a) = "[ " ++ (showArray (Array a)) ++ " ]"
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

showArgs :: [Id] -> String
showArgs [] = ""
showArgs ((Id arg):args) = 
  if(args == []) then arg
  else arg ++ " , " ++ showArgs args

showArray :: Value -> String
showArray (Array []) = ""
showArray (Array [a]) = show a
showArray (Array (a:as)) = show a ++ ", " ++ showArray (Array as)

