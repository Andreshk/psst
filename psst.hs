module Main (main) where

import System.Environment (getArgs)
import System.Random (mkStdGen, randomRs, random, StdGen(..))
import System.IO (readFile, writeFile)
import Control.Monad (liftM)

main = do
    args <- getArgs
    if length args < 2 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (fileName, k) = (head args, read (head (tail args)) :: Int)
        (s:f) <- liftM lines $ readFile fileName
        let (seed, names, maxIdx) = (read s :: Int, f, (length f) - 1)
            gen = mkStdGen seed
            chosen = map (names!!) $ take k $ randomRs (0, maxIdx) gen
            (newSeed, _) = random gen :: (Int, StdGen)
        mapM_ putStrLn chosen
        writeFile fileName $ unlines $ (show newSeed):names
        

        