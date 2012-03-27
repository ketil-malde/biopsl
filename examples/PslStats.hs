{-| Generate stats from PSL files, similar to 'samtools stats' for
    BAM files.  Each target sequence is listed with its length and 
    number of matching query sequences.
-}

import Bio.Alignment.PSL
import System.Environment (getArgs)
import Data.HashMap.Strict as H hiding (map)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (foldl', intercalate)

main :: IO ()
main = do
  fs <- getArgs
  ps <- mapM readPSL fs
  putStrLn ("target\tlenght\t"++intercalate "\t" fs)
  putStr $ unlines $ format $ countTargets ps

-- | map of lengths, and map of count per input file
countTargets :: [[PSL]] -> [HashMap L.ByteString Int]
countTargets = go [] empty
  where go cs ls (ps:pss) = let (l,c) = count1 ls ps
                            in go (c:cs) l pss
        go cs ls [] = (ls:reverse cs)
        count1 ls ps = Data.List.foldl' ins1 (ls,empty) ps
        ins1 (l,c) p = l `seq` c `seq` (H.insert (tname p) (tsize p) l,H.insertWith (+) (tname p) 1 c)


format :: [HashMap L.ByteString Int] -> [String]
format (l:cs) = [unwords' $ (L.unpack s:show len:map (show . H.lookupDefault 0 s) cs) | (s,len) <- toList l]
format _ = error "format"

unwords' :: [String] -> String
unwords' = intercalate "\t"