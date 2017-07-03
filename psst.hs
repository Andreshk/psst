module Main (main, psst) where

import Prelude hiding (lines, unlines, readFile, writeFile)
import System.Environment (getArgs, withArgs)
import System.Random (mkStdGen, randomRs, random, StdGen(..))
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Text (pack, unpack, lines, unlines)     -- requires "text" package
import Data.Text.Encoding (encodeUtf8, decodeUtf8)  -- requires "text" package 
import Control.Monad (liftM)

main = do
    args <- getArgs
    if length args < 2 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (fileName, k) = (head args, read (head (tail args)) :: Int)
        (s:f) <- liftM (lines . decodeUtf8) $ B.readFile fileName
        let (seed, names, maxIdx) = (read $ unpack s :: Int, f, (length f) - 1)
            gen = mkStdGen seed
            chosen = map (names!!) . take k $ randomRs (0, maxIdx) gen
            (newSeed, _) = random gen :: (Int, StdGen)
        mapM_ (putStrLn . unpack) chosen
        B.writeFile fileName . encodeUtf8 . unlines $ (pack $ show newSeed):names

-- User-friendly version - to be called from GHCI or WinGHCI
psst :: String -> Int -> IO ()
psst fileName k = withArgs [fileName, show k] main
