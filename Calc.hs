module Calc where

import Data.List
import Control.Applicative ((<|>))
import Control.Monad (guard)

{-
TODO:
Add support for and '/'
Add support for non-natural numbers: floats, fracntions and negatives
Add support for unary operations, such as trig and log functions
-}

data Tree a = Nil | Node a (Tree a) (Tree a)
            deriving (Show)

type LexTree = Tree String

value :: a -> Tree a
value x = Node x Nil Nil

op :: String -> Tree String -> Tree String -> Tree String
op o x y = Node o x y

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
breakUp =  parenBreak . breakUp' []

parseMult :: [String] -> Maybe LexTree
parseMult e = do
  mult <- elemIndex "*" e
  let (x, y) = splitAt mult e
  xTree <- parse x
  yTree <- parse (drop 1 y) -- dropping the '*'
  return $ op "*" xTree yTree

parseAdd :: [String] -> Maybe LexTree
parseAdd e = do
  plus <- "+" `elemIndex` e
  let (x, y) = splitAt plus e
  xTree <- parse x
  yTree <- parse (drop 1 y)
  return $ op "+" xTree yTree

parseSub :: [String] -> Maybe LexTree
parseSub e = do
  minus <- "-" `elemIndex` e
  let (x,y) = splitAt minus e
  xTree <- parse x
  yTree <- parse (drop 1 y)
  return $ op "-" xTree yTree

isParen :: [String] -> Bool
isParen e 
    | ["("] `isPrefixOf` e = True
    | otherwise = False

parseParen :: [String] -> Maybe LexTree
parseParen e = do
  guard $ isParen e
  closing <- ")" `elemIndex` e
  let (x, y) = splitAt closing e
  xTree <- parse $ drop 1 x
  return $ xTree

parseNum :: [String] -> Maybe LexTree
parseNum e = case length e of
               1 -> return $ value $ head e
               _ -> Nothing

parse :: [String] -> Maybe LexTree
parse e = parseParen e <|> parseAdd e <|> parseSub e <|> parseMult e <|> parseNum e

applyBinOp :: String -> String -> String -> Integer
applyBinOp "+" x y = (read x) + (read y)
applyBinOp "*" x y = (read x) * (read y)
applyBinOp "-" x y = (read x) - (read y)

numberNode :: LexTree -> Bool
numberNode (Node _ Nil Nil) = True
numberNode _ = False

getVal :: LexTree -> String
getVal (Node x _ _) = x

reduceOp :: LexTree -> Maybe Integer
reduceOp (Node op x y)
    | numberNode x && numberNode y = Just $ applyBinOp op (getVal x) (getVal y)
    | otherwise = Nothing

reduceTree :: LexTree -> Maybe LexTree
reduceTree t@(Node val t1 t2)
    | numberNode t1 && numberNode t2 = reduceOp t >>= return . value . show
    | numberNode t = return t
    | otherwise = do
  reduced1 <- reduceTree t1
  reduced2 <- reduceTree t2
  reduceTree (Node val reduced1 reduced2)

eval :: String -> Maybe Integer
eval s = (parse $ breakUp s) >>= reduceTree >>= return . read . getVal

