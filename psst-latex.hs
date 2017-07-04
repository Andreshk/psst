module Main (main) where

import Prelude hiding (lines, unlines, readFile, writeFile)
import System.Environment (getArgs)
import System.Random (StdGen(..), mkStdGen, randomRs, random)
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Text (Text(..), pack, unpack, lines, unlines, splitOn, append)  -- requires "text" package
import Data.Text.Encoding (encodeUtf8, decodeUtf8)  -- requires "text" package 
import Control.Monad (liftM)

main :: IO ()
main = do
    (fileName, templateName, resultName) <- parseArgs
    (seed:names) <- liftM lines $ fileToText fileName
    let gen = mkStdGen . read . unpack $ seed
        message = pack "Псст! Сесия е. Вземи си изпит!"
        chosen = map (names!!) $ randomRs (0, (length names) - 1) gen
        newSeed = fst $ random gen :: Int
    pieces <- liftM (splitOn (pack "--split")) $ fileToText templateName
    textToFile resultName (buildLatex pieces (message:chosen))
    textToFile fileName (unlines $ (pack $ show newSeed):names)
    putStrLn "Done!"

parseArgs :: IO (String, String, String)
parseArgs = do
    args <- getArgs
    if length args < 3 then error "Inappropriate number of arguments!"
    else let (a:b:c:_) = args in return (a,b,c)

buildLatex :: [Text] -> [Text] -> Text
buildLatex (str:[]) _ = str
buildLatex (x:xs) (y:ys) = x `append` y `append` (buildLatex xs ys)

textToFile :: String -> Text -> IO ()
textToFile fileName text = B.writeFile fileName (encodeUtf8 text)

fileToText :: String -> IO Text
fileToText fileName = liftM decodeUtf8 $ B.readFile fileName