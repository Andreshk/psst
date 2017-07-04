module Main (main) where

import Prelude hiding (lines, unlines, readFile, writeFile)
import System.Environment (getArgs, withArgs)
import System.Random (mkStdGen, randomRs, random, StdGen(..))
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Text (Text(..), pack, unpack, lines, unlines, splitOn, append)     -- requires "text" package
import Data.Text.Encoding (encodeUtf8, decodeUtf8)  -- requires "text" package 
import Control.Monad (liftM)

main :: IO ()
main = do
    args <- getArgs
    if length args < 3 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (fileName:templateName:resultName:_) = args
        (s:names) <- liftM (lines . decodeUtf8) $ B.readFile fileName
        let (seed, maxIdx) = (read $ unpack s :: Int, (length names) - 1)
            gen = mkStdGen seed
            message = pack "Псст! Сесия е. Вземи си изпит!"
            chosen = map (names!!) . take 17 $ randomRs (0, maxIdx) gen
            (newSeed, _) = random gen :: (Int, StdGen)
        latex <- liftM (splitOn (pack "--split") . decodeUtf8) $ B.readFile templateName
        --mapM_ (putStrLn . unpack) chosen
        B.writeFile resultName . encodeUtf8 $ buildLatex latex (message:chosen)
        B.writeFile fileName . encodeUtf8 . unlines $ (pack $ show newSeed):names
        putStrLn "Done!"

buildLatex :: [Text] -> [Text] -> Text
buildLatex (str:[]) [] = str
buildLatex (x:xs) (y:ys) = x `append` y `append` (buildLatex xs ys)
buildLatex _ _ = error "# of slots /= # replacements!!1!1"
