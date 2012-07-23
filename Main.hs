module Main where

import Calc
import Data.List (unlines)

control :: [String]
control = [":q", "quit"]

isControl :: String -> Bool
isControl cs = cs `elem` control

greeting :: String
greeting = unlines
           [ "Welcome to TreeCalc 1.0!"
           , "This is a simple calculator, implemented in Haskell."
           , "Please note, that for the moment factorial function is implemented as 'fact', NOT as 'n!'."
           , "Available operaions: +, -, *, /, sin, cos, tan, cot, fact."
           , "Type :q or quit to exit."
           ]

prompt :: String
prompt = "> "

main :: IO ()
main = do
  putStr greeting
  interactionLoop

interactionLoop :: IO ()
interactionLoop = do
  putStr prompt
  input <- getLine
  if isControl input
     then react input
     else evaluate input
  
react :: String -> IO ()
react i = do
  if i `elem` [":q", "quit"]
  then return ()
  else interactionLoop

evaluate :: String -> IO ()
evaluate i = do
  case eval i of
    Nothing -> putStrLn "Malformed input."
    (Just x) -> putStrLn (show x)
  interactionLoop