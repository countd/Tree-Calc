module Calc where

import Data.List
import Control.Applicative ((<|>), (<*>), pure)
import Control.Monad (guard)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

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

unarOp :: String -> LexTree -> LexTree
unarOp o x = Node o x Nil

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

binSymbols :: [String]
binSymbols = [ "+"
             , "-"
             , "*"
             , "/"
             ]

unSymbols :: [String]
unSymbols = [ "sin"
            , "cos"
            , "tan"
            , "cot"
            ]

binParsers :: [([String] -> Maybe LexTree)]
binParsers = map parseBinOp binSymbols

nextElem :: Int -> [a] -> Maybe Int
nextElem idx xs = if (length xs - 1) > idx
                  then Just $ (idx + 1)
                  else Nothing

parseUnOp :: String -> [String] -> Maybe LexTree
parseUnOp o e = do
  oper <- o `elemIndex` e
  argStart <- nextElem oper e
  let (x, y) = splitAt argStart e
  arg <- parse y
  return $ unarOp o arg

unParsers :: [([String] -> Maybe LexTree)]
unParsers = map parseUnOp unSymbols

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

dispatch :: [([String] -> Maybe LexTree)]
dispatch = concat
    [ binParsers
    , [parseParen]
    , unParsers
    , [parseNum]
    ]

parse :: [String] -> Maybe LexTree
parse e = foldr (<|>) Nothing (dispatch <*> pure e)

binaryOps :: (Fractional a) => Map.Map String (a -> a -> a)
binaryOps = Map.fromList
            [ ("+", (+))
            , ("-", (-))
            , ("*", (*))
            , ("/", (/))
            ]

unaryOps :: (Floating a) => Map.Map String (a -> a)
unaryOps = Map.fromList
           [ ("sin", sin)
           , ("cos", cos)
           , ("tan", tan)
           , ("cot", cot)
           ] 
    where
      cot x = 1 / (tan x)

-- we assume the operation is in the dispatch table
-- (binaryOps) for now, since all is hardcoded
getBinOp :: (Fractional a) => String -> (a -> a -> a)
getBinOp o = fromJust $ Map.lookup o binaryOps

getUnOp :: (Floating a) => String -> (a -> a)
getUnOp o = fromJust $ Map.lookup o unaryOps

applyBinOp :: (Fractional a) => String -> a -> a -> a
applyBinOp o x y = (getBinOp o) x y

applyUnOp :: (Floating a) => String -> a -> a
applyUnOp o x = (getUnOp o) x

-- a funcion to 'apply' a tree,
-- i.e. read a number or apply an operator
apply :: LexTree -> Maybe Float
apply t@(Node x y z) = case treeType t of
                         Num -> Just $ read x
                         Bin -> do
                           y' <- apply y
                           z' <- apply z
                           return $ applyBinOp x y' z'
                         Un -> do
                           y' <- apply y
                           return $ applyUnOp x y'

eval :: String -> Maybe Float
eval s = (parse $ breakUp s) >>= apply

