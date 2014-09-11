module Semantic where
import Program

mainDefined :: Program -> Bool
mainDefined (Program x) = hasMain x

hasMain :: Functions -> Bool
hasMain EmptyFunctions = False
hasMain (Functions f fs) = if isMain f then True else hasMain fs

isMain :: Function -> Bool
isMain (Function (Id name) _ _ _) = if name == "main" then True else False
