{-| Generate stats from PSL files, similar to 'samtools stats' for
    BAM files.  Each target sequence is listed with its length and 
    number of matching query sequences.
-}

import Bio.Alignment.PSL
import System.Environment (getArgs)
import Data.HashMap.Strict as H hiding (map)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
  fs <- getArgs
  ps <- concat `fmap` mapM readPSL fs
  L.putStr $ L.unlines $ map format $ toList $ countTargets ps

countTargets :: [PSL] -> HashMap L.ByteString Int 
countTargets ps = fromListWith (+) $ map (\p -> (tname p,1)) ps -- todo:tsize

format :: (L.ByteString, Int) -> L.ByteString
format (tn,p) = L.concat [tn, tab, L.pack (show p)]
  where tab = L.pack "\t"
