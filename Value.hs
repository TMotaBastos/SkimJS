module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Array [Value]
    | Return Value
    | Break (Maybe Id)
    | NaoDeclarado
    | Nil
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
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
