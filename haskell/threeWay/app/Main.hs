module Main (main) where

import Lib

import System.Random (randomRIO)
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

-- Function to measure time
measureTime :: Int -> IO Double
measureTime size = do
    let arr = take size [1..]  -- Create a list of size 'size'
    startTime <- getCurrentTime  -- Start time
    let sortedArr = threeWayMergeSort arr  -- Sort the array
    endTime <- getCurrentTime  -- End time
    let duration = diffUTCTime endTime startTime  -- Calculate time difference
    return (realToFrac duration * 1000000)  -- Return time in microseconds

-- Main function to run the test for different sizes
main :: IO ()
main = do
    let sizes = [1000, 10000, 100000, 1000000]
    -- Run test for each size
    mapM_ runTest sizes
  where
    -- Run a test for each size and print the results
    runTest size = do
        putStrLn $ "Testing with array size: " ++ show size
        duration <- measureTime size
        putStrLn $ "Time taken for sorting: " ++ show duration ++ " µs"
