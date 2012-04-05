{-| Select only best hit from each sequence from a PSL file, 
    write to stdout 
-}

import Bio.Alignment.PSL
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import Data.List (sortBy)

main :: IO ()
main = do
  args <- getArgs
  ps <- case args of 
        [] -> (parsePSL `fmap` L.getContents)
        [f] -> readPSL f
        _ -> error "Usage: psluniq [pslfile]"
  printPSL $ uniq ps
  
uniq :: [PSL] -> [PSL]
uniq [] = []
uniq (p:ps) 
  -- protein matches are sorted first by strand, thus we need to resort to sorting
  | L.length (strand p) > 1 = let (q:qs) = sortOn qname (p:ps) in go q qs
  | otherwise               = go p ps
  
go :: PSL -> [PSL] -> [PSL]
go p1 (q:qs) | qname q /= qname p1 = p1 : go q qs
             | match q > match p1  = go q qs
             | otherwise           = go p1 qs
go p1 [] = [p1]

sortOn :: Ord a1 => (a -> a1) -> [a] -> [a]
sortOn f = sortBy (\x y -> compare (f x) (f y))