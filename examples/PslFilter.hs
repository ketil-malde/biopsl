{-| Select a subset of PSL records according to specified criteria:
  e.g. pslfilter --identity-min --overhang-max ... --count-max
-}

{-# Language DeriveDataTypeable, OverloadedStrings #-}

import Bio.Alignment.PSL
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as L

data Options = Opts 
  { input :: Maybe FilePath
  , identity, queryCov, targetCov :: Maybe Double
  , overhang :: Maybe Int
  } deriving (Data,Typeable)

defs :: Options
defs = Opts 
  { input = Nothing &= args
  , identity = Nothing &= help "minimum identiy for matches" &= name "i"
  , overhang = Nothing &= help "maximum overhang for matches" &= name "h"
  , queryCov = Nothing &= help "minimum query coverage" &= name "q"
  , targetCov = Nothing &= help "minimum target coverage" &= name "t"
  } &= summary "pslfilter - extract a subset of PSL records"
    &= program "pslfilter"

main :: IO ()
main = do
  opts <- cmdArgs defs
  ps <- case input opts of 
        Nothing  -> (parsePSL `fmap` L.getContents)
        Just f -> readPSL f
  let idfilter = case identity opts of
        Nothing -> const True
        Just d  -> (\x -> local_identity x >= d)
      ohfilter = case overhang opts of  
        Nothing -> const True
        Just o  -> (\x -> (uncurry max $ overhangs x) < o)
      qcfilter = case queryCov opts of
          Nothing -> const True
          Just q -> (\x -> fromIntegral (match x) / fromIntegral (qsize x) >= q)
      tcfilter = case targetCov opts of
          Nothing -> const True
          Just t -> (\x -> fromIntegral (match x) / fromIntegral (tsize x) >= t)
  printPSL $ filter (idfilter .&. ohfilter .&. qcfilter .&. tcfilter ) ps


(.&.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(.&.) f g = \x -> f x && g x

-- | Calculate the smallest overhang, i.e. the unmatched parts
--   of either query or target on each side of the local alignment.
overhangs :: PSL -> (Int,Int)
overhangs p = case strand p of
  "+" -> (min (qstart p) (tstart p), min (qsize p-qend p) (tsize p-tend p))
  "-" -> (min (qstart p) (tsize p-tend p), min (qsize p-qend p) (tstart p))
  _ -> error ("I couldn't understand strand :"++show (strand p))

-- | Calculate the identity score of the matched region
local_identity :: PSL -> Double
local_identity p = fromIntegral (match p) / fromIntegral (match p + mismatch p)
