module Main (main, psst) where

import Prelude hiding (lines, unlines, readFile, writeFile)
import System.Environment (getArgs, withArgs)
import System.Random (StdGen(..), mkStdGen, randomRs, random)
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Text (Text(..), pack, unpack, lines, unlines)   -- requires "text" package
import Data.Text.Encoding (encodeUtf8, decodeUtf8)  -- requires "text" package 
import Control.Monad (liftM)

main = do
    (fileName, k) <- parseArgs
    (seed:names) <- liftM (lines . decodeUtf8) $ B.readFile fileName
    let gen = mkStdGen . read . unpack $ seed
        chosen = map (names!!) $ randomRs (0, (length names) - 1) gen
        newSeed = fst $ random gen :: Int
    mapM_ (putStrLn . unpack) (take k chosen)
    textToFile fileName (unlines $ (pack $ show newSeed):names)

-- User-friendly version - to be called from GHCI or WinGHCI
psst :: String -> Int -> IO ()
psst fileName k = withArgs [fileName, show k] main

parseArgs :: IO (String, Int)
parseArgs = do
    args <- getArgs
    if length args < 2 then error "Inappropriate number of arguments!"
    else let (a:b:_) = args in return (a, read b)

textToFile :: String -> Text -> IO ()
textToFile fileName text = B.writeFile fileName (encodeUtf8 text)

fileToText :: String -> IO Text
fileToText fileName = liftM decodeUtf8 $ B.readFile fileName