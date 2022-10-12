module Main where

main :: IO()
main =
  print (memberSet 1 [2, 3, 4, 5, 6, 7])

memberSet :: Eq a => a -> [a] -> Bool
memberSet e []
  = False
memberSet e (x:xs)
  | x == e = True
  | otherwise = memberSet e xs

unionSet :: [Int] -> [Int] -> [Int]
unionSet [] s
  = s
unionSet (x:xs) us
  | memberSet x us = unionSet xs us
  | otherwise = x: unionSet xs us

intersectSet :: Eq a => [a] -> [a] -> [a]
intersectSet [] us
  = []
intersectSet (x:xs) us
  | memberSet x us = x : intersectSet xs us
  | otherwise = intersectSet xs us

takeList :: Int -> [Int] -> [Int]
takeList 0 e
  = []
takeList n []
  = []
takeList n (e:es)
  = e:takeList(n-1) es

dropList :: Int -> [Int] -> [Int]
dropList 0 e
  = e
dropList n []
  = []
dropList n (e:es)
  = dropList (n-1) es

split :: Int -> [Int] -> ([Int], [Int])
split 0 xs
  = ([0], xs)
split n []
  = ([], [])
split n (x:xs)
  = (x:as, bs) where (as, bs) = split (n-1) xs
