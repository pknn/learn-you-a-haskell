import Data.Char
import Data.List
import Data.Map qualified as Map

countWords :: String -> [(String, Int)]
countWords str = zip (map head grouppedSortWords) (map length grouppedSortWords)
  where
    grouppedSortWords = group . sort . words $ str

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle hayStack = (any . isPrefixOf) needle $ tails hayStack

encode :: Int -> String -> String
encode offset = map (chr . (+ offset) . ord)

decode :: Int -> String -> String
decode offset = encode $ negate offset

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

phoneBook =
  [ ("betty", "555-2928"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-2928"),
    ("penny", "853-2492")
  ]

findKeyUnsafe :: (Eq k) => k -> [(k, v)] -> v
findKeyUnsafe key = snd . head . filter ((== key) . fst)

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key kv = Just (snd . head . filter ((== key) . fst) $ kv)

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v) : kv)
  | key == k = Just v
  | otherwise = findKey' key kv

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

phoneBookMap :: Map.Map String String
phoneBookMap = Map.fromList phoneBook

stringToDigits :: String -> [Int]
stringToDigits = map digitToInt . filter isDigit