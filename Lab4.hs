main :: IO ()
main
  = putStrLn (show (qsort  [7,4,9,1,3,2]))

-- Computing the maximum of a list sequentially

seqmax :: [Int] -> Int
seqmax [x]
  = x
seqmax (x:xs)
  | x > seqmax xs = x
  | otherwise = seqmax xs

-- Now, let's rewrite the above function in parallel

dcmax :: [Int] -> Int
dcmax [x]
  = x
dcmax xs
  = max a b
    where
    mid      = length xs `div` 2
    (as, bs) = splitAt mid (xs)
    a        = dcmax as
    b        = dcmax bs

-- Sequential sort

seqsort :: [Int] -> [Int]
seqsort []
  = []
seqsort (x:xs)
  = insert x (seqsort xs)


insert :: Int -> [Int] -> [Int]
insert e []
  = [e]
insert e (x:xs)
  | e < x = e : x : xs
  | otherwise = x : insert e xs 

-- Quicksort (which is parallel)

-- part function that splits a list in 2 around a pivot

part :: (Int -> Bool) -> [Int] -> ([Int], [Int])
part p []
  = ([], [])
part p (x:xs)
  | p x = (x:as, bs)
  | otherwise = (as, x:bs)
  where
  (as, bs) = part p xs

-- actual quicksort algorithm

qsort :: [Int] -> [Int]
qsort []
  = []
qsort (x:xs)
  = par qbs qas ++ [x] ++ qsort bs
    where
    qas = qsort as
    qbs = qsort bs
    (as, bs) = part (<x) xs


par :: a -> b -> b
par x y
  = y
