module Main where

main :: IO()
main =
  putStrLn (show (insert 8 [5, 6, 79, 6778]))

len :: [Int] -> Int
len []
  = 0
len (x:xs)
  = 1 + len xs 

contains :: [String] -> String -> Bool
contains [] e
  = False
contains (x:xs) e
  | x == e = True
  | otherwise = contains xs e

set :: [String] -> Bool
set [] 
  = True
set (x:xs)
  | contains xs x = False
  | otherwise = set xs

largest :: [Int] -> Int
largest [x]
  = x
largest (x:xs)
  | x > largest xs = x
  | otherwise = largest xs

zipped :: ([String], [Int]) -> [(String, Int)]
zipped ([], [])
  = []
zipped (x:xs, y:ys)
  = (x,y) : zipped (xs, ys)

insert :: Int -> [Int] -> [Int]
insert e []
  = [e]
insert e (x:xs)
  | e < x = e : x : xs
  | otherwise = x : insert e xs

sort :: [Int] -> [Int]
sort []
  = []
sort (x:xs)
  | 












