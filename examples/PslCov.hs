-- PslCov: extract complete hits (more than x% of query covered)

module Main where

import Bio.Alignment.PSL
import System.Environment (getArgs)

main :: IO ()
main = do
  [p,f] <- getArgs
  printPSL . filter (isHit $ read p) =<< readPSL f
  
isHit :: Double -> PSL -> Bool
isHit p x = fromIntegral (match x) / fromIntegral (qsize x) > p
         

