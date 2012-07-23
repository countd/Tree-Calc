module Calc where

import Data.List
import Control.Applicative ((<|>))
import Control.Monad (guard)

{-
TODO:
Add support for unary operations, such as trig and log functions
-}

data Tree a = Nil | Node a (Tree a) (Tree a)
            deriving (Show)

type LexTree = Tree String

data TreeData = Num
              | Un 
              | Bin
                deriving (Show)

value :: a -> Tree a
value x = Node x Nil Nil

op :: String -> Tree String -> Tree String -> Tree String
op o x y = Node o x y

-- does the tree represent a number,
-- a binary operator or a unary operator?
treeType :: LexTree -> TreeData
treeType (Node _ Nil Nil) = Num
treeType (Node _ (Node _ _ _) (Node _ _ _)) = Bin
treeType _ = Un

dropSpaces :: String -> String
dropSpaces = dropWhile (== ' ')

breakUp' :: [String] -> String -> [String]
breakUp' acc [] = reverse acc
breakUp' acc cs = let (pref,leftOver) = span (/= ' ') cs
                  in breakUp' (pref:acc) (dropSpaces leftOver)

dropLast :: [a] -> [a]
dropLast = reverse . drop 1 . reverse
                      

carryParen :: String -> [String]
carryParen s
    | "(" `isPrefixOf` s = ["(", (drop 1 s)]
    | ")" `isSuffixOf` s = [(dropLast s), ")"]
    | otherwise = [s]

parenBreak :: [String] -> [String]
parenBreak = concat . map carryParen

breakUp :: String -> [String]
breakUp =  parenBreak . parenBreak . breakUp' []

parseBinOp :: String -> [String] -> Maybe LexTree
parseBinOp o e = do
  oper <- o `elemIndex` e
  let (x,y) = splitAt oper e
  xTree <- parse x
  yTree <- parse (drop 1 y) -- dropping the op
  return $ op o xTree yTree

parseMult :: [String] -> Maybe LexTree
parseMult = parseBinOp "*"

parseAdd :: [String] -> Maybe LexTree
parseAdd = parseBinOp "+"

parseSub :: [String] -> Maybe LexTree
parseSub = parseBinOp "-"

parseDiv :: [String] -> Maybe LexTree
parseDiv = parseBinOp "/"

isParen :: [String] -> Bool
isParen e 
    | ["("] `isPrefixOf` e = True
    | otherwise = False

parseParen :: [String] -> Maybe LexTree
parseParen e = do
  guard $ isParen e
  closing <- ")" `elemIndex` e
  let (x, y) = splitAt closing e
  xTree <- parse $ filter (not . null) $ drop 1 x
  return $ xTree

parseNum :: [String] -> Maybe LexTree
parseNum e = case length e of
               1 -> return $ value $ head e
               _ -> Nothing

parse :: [String] -> Maybe LexTree
parse e = parseAdd e <|> parseSub e <|> parseMult e <|> parseDiv e <|> parseParen e <|> parseNum e

applyBinOp :: (Num a, Fractional a) => String -> a -> a -> a
applyBinOp "+" x y = x + y
applyBinOp "*" x y = x * y
applyBinOp "-" x y = x - y
applyBinOp "/" x y = x / y

-- a funcion to 'apply' a tree,
-- i.e. read a number or apply an operator
apply :: LexTree -> Maybe Float
apply t@(Node x y z) = case treeType t of
                         Num -> Just $ read x
                         Bin -> do
                           y' <- apply y
                           z' <- apply z
                           return $ applyBinOp x y' z'
                         Un -> Nothing

eval :: String -> Maybe Float
eval s = (parse $ breakUp s) >>= apply

