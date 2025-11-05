module Main (main) where

import GaussianSolver
import Text.Printf (printf)
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
      | coeff == 1 = "x" ++ show i
      | coeff == -1 = "-x" ++ show i
      | otherwise = show coeff ++ "*x" ++ show i

-- розв'язок
printSolution :: [Double] -> IO ()
printSolution solutions =
    mapM_ (uncurry (printf "x_%d = %.2f\n")) (zip [1 :: Int ..] solutions)

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
