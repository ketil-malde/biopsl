{-| Select only best hit from each sequence from a PSL file, 
    write to stdout 
-}

import Bio.Alignment.PSL
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  args <- getArgs
  ps <- case args of 
        [] -> (parsePSL `fmap` L.getContents)
        [f] -> readPSL f
        _ -> error "Usage: psluniq [pslfile]"
  L.putStr pslHeader
  L.putStr $ unparsePSL $ uniq ps
  
uniq :: [PSL] -> [PSL]
uniq [] = []
uniq (p:ps) = p : go (qname p) ps
  where go n (q:qs) | qname q == n = go n qs
                    | otherwise    = q:go (qname q) qs
        go _ [] = []