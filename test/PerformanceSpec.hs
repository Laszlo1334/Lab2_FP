module PerformanceSpec (spec) where

import Test.Hspec
import GaussianSolver
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)

timeIt :: NFData b => (a -> b) -> a -> IO Double
timeIt f x = do
    start <- getCurrentTime
    _ <- evaluate (rnf (f x))
    end <- getCurrentTime
    return $ realToFrac (diffUTCTime end start)

createSystem :: Int -> [[Double]]
createSystem n =
  [ [ fromIntegral (i * n + j + 1) | j <- [0..n-1] ] ++ [fromIntegral (i+1)] | i <- [0..n-1] ]

spec :: Spec
spec = do
    describe "performance check" $ do
        it "compares sequential vs parallel" $ do
            putStrLn ""
            let bigSystem = createSystem 150

            seqTime <- timeIt solveSequential bigSystem
            printf "sequential time: %.4f sec\n" seqTime

            parTime <- timeIt solveParallel bigSystem
            printf "parallel time:   %.4f sec\n" parTime
            
            (seqTime > 0) `shouldBe` True
