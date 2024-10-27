lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- matching tuples

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

sndOf3 :: (a, b, c) -> b
sndOf3 (_, y, _) = y

trdOf3 :: (a, b, c) -> c
trdOf3 (_, _, z) = z

-- matching list

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

-- matching as

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, eat more!"
  | bmi <= 25.0 = "Looking good!"
  | bmi <= 30.0 = "You're overweight. Let's work out together!"
  | otherwise = "You're obese. Go see a doctor"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, eat more!"
  | weight / height ^ 2 <= 25.0 = "Looking good!"
  | weight / height ^ 2 <= 30.0 = "You're overweight. Let's work out together!"
  | otherwise = "You're obese. Go see a doctor"

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

-- where

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight, eat more!"
  | bmi <= normal = "Looking good!"
  | bmi <= fat = "You're overweight. Let's work out together!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstName
    (l : _) = lastName

-- let

cylinderSurfaceArea :: Double -> Double -> Double
cylinderSurfaceArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- case

head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "Can't call head on an empty list, dummy!"
  (x : _) -> x

describeList :: [a] -> String
describeList ls =
  "The list is a " ++ case ls of
    [] -> "empty"
    [x] -> "singleton list"
    _ -> "normal list"