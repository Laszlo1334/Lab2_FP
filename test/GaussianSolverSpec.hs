module GaussianSolverSpec (spec) where

import Test.Hspec
import GaussianSolver

-- Функція для порівняння чисел з плаваючою комою
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 1e-9

spec :: Spec
spec = do
    describe "GaussianSolver" $ do
        let matrix = [ [ 2,  1, -1,   8],
                       [-3, -1,  2, -11],
                       [-2,  1,  2,  -3] ]
        let expected = [2, 3, -1]

        it "solves a system sequentially" $ do
            let solution = solveSequential matrix
            and (zipWith (~=) solution expected) `shouldBe` True

        it "solves a system in parallel" $ do
            let solution = solveParallel matrix
            and (zipWith (~=) solution expected) `shouldBe` True
