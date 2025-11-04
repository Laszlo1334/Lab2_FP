module GaussianSolver (
    solveSequential,
    solveParallel
) where

import Control.Parallel.Strategies
import Data.List (foldl', maximumBy)
import Data.Ord (comparing)

type Matrix = [[Double]]
type Row = [Double]
type Solution = [Double]

-- Функція для перестановки двох рядків
swapRows :: Int -> Int -> Matrix -> Matrix
swapRows r1 r2 m
  | r1 == r2  = m
  | otherwise = let row1 = m !! r1
                    row2 = m !! r2
                    m'   = updateAt r1 row2 m
                in updateAt r2 row1 m'

-- Функція для оновлення елемента в списку
updateAt :: Int -> a -> [a] -> [a]
updateAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs

-- Вибір опорного елемента (Pivoting)
pivot :: Int -> Matrix -> Matrix
pivot k m =
  let n = length m
      col = map (\r -> abs (r !! k)) (drop k m)
      (_, maxIdx) = maximumBy (comparing fst) (zip col [k..n-1])
  in if (m !! maxIdx !! k) == 0
     then error "Matrix is singular or requires more complex pivoting"
     else swapRows k maxIdx m

-- Прямий хід (послідовний)
forwardElimination :: Matrix -> Matrix
forwardElimination matrix = foldl' reduceColumn matrix [0 .. n - 1]
  where
    n = length matrix
    reduceColumn m k =
      let m' = pivot k m
          pivotRow = m' !! k
          pivotVal = pivotRow !! k
      in foldl' (\acc i ->
            let targetRow = acc !! i
                factor = targetRow !! k / pivotVal
                newRow = zipWith (\p t -> t - factor * p) pivotRow targetRow
            in updateAt i newRow acc
         ) m' [k + 1 .. n - 1]

-- Прямий хід (паралельний)
forwardEliminationParallel :: Matrix -> Matrix
forwardEliminationParallel matrix = foldl' reduceColumn matrix [0 .. n - 1]
  where
    n = length matrix
    reduceColumn m k =
      let m' = pivot k m
          pivotRow = m' !! k
          pivotVal = pivotRow !! k
          indices = [k + 1 .. n - 1]
          processRow i =
              let targetRow = m' !! i
                  factor = targetRow !! k / pivotVal
              in (i, zipWith (\p t -> t - factor * p) pivotRow targetRow)
          newRows = parMap rdeepseq processRow indices
      in foldl' (\acc (i, row) -> updateAt i row acc) m' newRows

-- Зворотний хід
backSubstitution :: Matrix -> Solution
backSubstitution u = go (n - 1) []
  where
    n = length u
    go i knownXs
      | i < 0     = knownXs
      | otherwise =
          let row      = u !! i
              (coeffs, [rhs]) = splitAt n row
              sumKnown = sum $ zipWith (*) (drop (i + 1) coeffs) knownXs
              diagCoeff = coeffs !! i
              x_i = (rhs - sumKnown) / diagCoeff
          in go (i - 1) (x_i : knownXs)

-- Послідовний розв'язувач
solveSequential :: Matrix -> Solution
solveSequential = backSubstitution . forwardElimination

-- Паралельний розв'язувач
solveParallel :: Matrix -> Solution
solveParallel = backSubstitution . forwardEliminationParallel
