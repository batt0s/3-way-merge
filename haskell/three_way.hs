import System.Random (randomRIO)
import Data.List (sort)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Merge function to merge three sorted lists
merge :: Ord a => [a] -> [a] -> [a] -> [a]
merge [] [] [] = []
merge left [] [] = left
merge [] left [] = left
merge [] [] right = right
merge left middle [] = merge left middle []
merge left [] right = merge left [] right

merge (l:ls) (m:ms) (r:rs)
  | l <= m && l <= r = l : merge ls (m:ms) (r:rs)
  | m <= l && m <= r = m : merge (l:ls) ms (r:rs)
  | otherwise = r : merge (l:ls) (m:ms) rs

-- 3-way Merge Sort function
threeWayMergeSort :: Ord a => [a] -> [a]
threeWayMergeSort [] = []
threeWayMergeSort [x] = [x]
threeWayMergeSort xs = merge (threeWayMergeSort left) (threeWayMergeSort middle) (threeWayMergeSort right)
  where
    n = length xs
    left = take (n `div` 3) xs
    middle = take (n `div` 3) (drop (n `div` 3) xs)
    right = drop (2 * (n `div` 3)) xs

-- Function to test sorting and measure time
testSorting :: Int -> IO ()
testSorting size = do
    let arr = [1..size]  -- Create a simple range of numbers for testing
    startTime <- getCurrentTime
    let sortedArr = threeWayMergeSort arr
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Sorting completed in: " ++ show duration ++ " seconds"
    
main :: IO ()
main = do
    testSorting 1000
    testSorting 10000
    testSorting 100000
    testSorting 1000000
