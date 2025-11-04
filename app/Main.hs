module Main (main) where

import GaussianSolver
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.List (intercalate)

-- вивід рівнянь
printEquations :: [[Double]] -> IO ()
printEquations matrix =
    mapM_ (putStrLn . formatRow) matrix
  where
    formatRow row =
      let coeffs = init row
          rhs = last row
          terms = zipWith formatTerm coeffs [1..]
          nonZeroTerms = filter (not . null) terms
      in intercalate " + " nonZeroTerms ++ " = " ++ show rhs
    formatTerm coeff i
      | coeff == 0 = ""
      | coeff == 1 = "x_" ++ show i
      | coeff == -1 = "-x_" ++ show i
      | otherwise = show coeff ++ "*x_" ++ show i

-- розв'язок
printSolution :: [Double] -> IO ()
printSolution solutions =
    mapM_ (uncurry (printf "x_%d = %.2f\n")) (zip [1 :: Int ..] solutions)

-- час виконання
timeIt :: NFData b => (a -> b) -> a -> IO Double
timeIt f x = do
    start <- getCurrentTime
    _ <- evaluate (rnf (f x))
    end <- getCurrentTime
    return $ realToFrac (diffUTCTime end start)

main :: IO ()
main = do
    let matrix = [ [ 1,  1, -2,  1,  3, 1],
                   [ 2, -1,  2,  2,  6, 2],
                   [ 3,  2, -4, -3, -9, 3],
                   [ 2, -1, -1,  1,  2, 4],
                   [ 1, -1,  3, -2, -5, 5] ]

    putStrLn "\ninitial matrix"
    printEquations matrix

    putStrLn "\nresult:"
    let solution = solveSequential matrix
    printSolution solution

    putStrLn "\nperformance check"
    let bigSystem = createSystem 500

    seqTime <- timeIt solveSequential bigSystem
    printf "sequential time: %.4f sec\n" seqTime

    parTime <- timeIt solveParallel bigSystem
    printf "parallel time:   %.4f sec\n" parTime

createSystem :: Int -> [[Double]]
createSystem n =
  [ [ fromIntegral (i * n + j + 1) | j <- [0..n-1] ] ++ [fromIntegral (i+1)] | i <- [0..n-1] ]
