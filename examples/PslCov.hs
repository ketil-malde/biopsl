-- PslCov: extract complete hits (more than x% of query covered)

module Main where

import Bio.Alignment.PSL
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [pq,pt,f] -> printPSL . filter (isHit (read pq) (read pt)) =<< readPSL f
    _ -> error ("Usage: pslcov p q pslfile\n"++
                "where p is the minimum query coverage\n"++
                "and q is the minium target coverage.")

isHit :: Double -> Double -> PSL -> Bool
isHit p q x = m / qs > p && m / ts > q
  where qs = fromIntegral $ qsize x
        ts = fromIntegral $ tsize x
        m  = fromIntegral $ match x
         

