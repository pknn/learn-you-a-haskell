zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (n * 3 + 1)

longCollatzChains :: Int
longCollatzChains = length $ filter (> 15) $ map (length . collatz) [1 .. 100]

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldr (\x acc -> (x == a) || acc) False

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' = foldr1 (&&)

or' :: [Bool] -> Bool
or' = foldr1 (||)

sqrtSumMoreThan1000 :: Int
sqrtSumMoreThan1000 = length $ takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1 ..]

oddSquareSumLessThan10000 :: Integer
oddSquareSumLessThan10000 = sum . takeWhile (< 10000) . filter odd $ map (^ 2) [1 ..]