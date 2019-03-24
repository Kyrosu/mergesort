
module Main where
main :: IO ()
main = return ()

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort (firsthalf l))(mergesort (secondhalf l))
    where
    firsthalf l = take ((length l) `div` 2) l
    secondhalf l = drop ((length l) `div` 2) l

merge :: [Int] -> [Int] -> [Int]
merge [] right = right
merge left [] = left

merge (l:left)(r:right)
 | l <= r = l:(merge left (r:right))
 | otherwise = r:(merge (l:left) right)