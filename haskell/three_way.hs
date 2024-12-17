-- import System.Random (randomRIO)

-- Merge function to merge three sorted lists
merge :: Ord a => [a] -> [a] -> [a] -> [a]
merge [] [] [] = []
merge left [] [] = left
merge [] middle [] = middle
merge [] [] right = right
merge left middle [] = mergeTwo left middle
merge left [] right = mergeTwo left right
merge [] middle right = mergeTwo middle right
merge left middle right = mergeTwo (mergeTwo left middle) right

-- Merge function to merge two sorted lists
mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo xs [] = xs
mergeTwo [] ys = ys
mergeTwo (x:xs) (y:ys)
  | x <= y = x : mergeTwo xs (y:ys)
  | otherwise = y : mergeTwo ys (x:xs)

-- 3-way Merge Sort function
threeWayMergeSort :: Ord a => [a] -> [a]
threeWayMergeSort [] = []
threeWayMergeSort [x] = [x]
threeWayMergeSort xs = merge (threeWayMergeSort left) (threeWayMergeSort middle) (threeWayMergeSort right)
  where
    n = length xs
    left = take ((n+1) `div` 3) xs
    middle = take ((n+1) `div` 3) (drop ((n+1) `div` 3) xs)
    right = drop (2 * ((n+1) `div` 3)) xs

