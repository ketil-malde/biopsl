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
uniq (p:ps) = go p ps
  
go :: PSL -> [PSL] -> [PSL]
go p1 (q:qs) | qname q /= qname p1 = p1 : go q qs
             | match q > match p1  = go q qs
             | otherwise           = go p1 qs
go p1 [] = [p1]
