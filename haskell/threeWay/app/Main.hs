module Main (main) where

import Lib

import Data.Time.Clock (getCurrentTime, diffUTCTime)


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
