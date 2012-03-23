{-| This models the PSL format used by e.g. the alignment tool BLAT.  
    It is a simple, textual representation of (spliced) alignments,
    with tab-separated fields.

    See <http://genome.ucsc.edu/FAQ/FAQformat#format2> for details.
-}

{-# Options -funbox-strict-fields #-} -- makes no difference
module Bio.Alignment.PSL where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intersperse)

-- | This encodes a PSL record, corresponding to one line of the PSL file.

-- NB! laziness issues, if not fully evaluated, this will hang onto the input.
data PSL = PSL   { match, mismatch, repmatch, ncount
                 , qgapcount, qgaplength, tgapcount, tgaplength :: Int 
                 , strand :: ByteString
                 , qname :: ByteString, qsize, qstart, qend :: Int
                 , tname :: ByteString, tsize, tstart, tend :: Int
                 , blockcount :: Int, blocksizes, qstarts, tstarts :: [Int]
                 } deriving (Eq)

instance Show PSL where 
  show = B.unpack . unparsePSL . return

-- | Read and parse a PSL file.
readPSL :: FilePath -> IO [PSL]
readPSL f = parsePSL `fmap` B.readFile f

-- | Create a PSL file from a list of alignments.
writePSL :: FilePath -> [PSL] -> IO ()
writePSL f =  B.writeFile f . B.append pslHeader . unparsePSL 

printPSL :: [PSL] -> IO ()
printPSL ps = do
  B.putStr pslHeader
  B.putStr $ unparsePSL ps

-- | Parse a 'ByteString' as a PSL file (note that it must contain the PSL header).
parsePSL :: ByteString -> [PSL]
parsePSL s = map parseLine $ B.lines $ dropHeader
  where
    -- dropHeader requires strict adherence to BLAT output.  Perhaps a looser check is better?
    dropHeader | pslHeader `B.isPrefixOf` s = B.drop (B.length pslHeader) s
               | otherwise = error "PSL header mismatch: couldn't determine the header."
    parseLine l = let fs = B.split '\t' l
                      ri :: Int -> Int
                      ri i = maybe (error ("Can't parse field "++show i++" '"++B.unpack (fs!!i)++"' as an integer")) fst $ B.readInt (fs!!i)
                      rl j = map (\w -> maybe undefined fst (B.readInt w)) $ init $ B.split ',' (fs!!j)
                  in PSL (ri 0) (ri 1) (ri 2) (ri 3) (ri 4) (ri 5) (ri 6) (ri 7) (fs!!8) (fs!!9) (ri 10) (ri 11) (ri 12) (fs!!13) (ri 14) (ri 15) (ri 16) (ri 17) (rl 18) (rl 19) (rl 20)
                           

-- | Unparse a list of 'PSL' alignments encoding them into a 'ByteString' (not including PSL header).
unparsePSL :: [PSL] -> ByteString
unparsePSL = B.unlines . map format1
  where format1 :: PSL -> ByteString
        format1 (PSL m mm rm nc qc ql tgc tgl st qn qsz qst qed tn tsz tst ted bc bs qss tss) = 
          B.concat $ intersperse tab $ (map showint [m, mm, rm, nc, qc, ql, tgc, tgl]
                                        ++[st,qn]++map showint [qsz,qst,qed]
                                        ++[tn]   ++map showint [tsz,tst,ted]
                                        ++[showint bc]++map showlist [bs,qss,tss])
        tab = B.pack "\t"
        showint  = B.pack . show
        showlist = B.pack . concat . map (++",") . map show

-- | The PSL header (version 3), as a 'ByteString'.
pslHeader :: ByteString
pslHeader = B.pack ("psLayout version 3\n\n"
                    ++"match\tmis- \trep. \tN's\tQ gap\tQ gap\tT gap\tT gap\tstrand\tQ        \tQ   \tQ    \tQ  \tT        \tT   \tT    \tT  \tblock\tblockSizes \tqStarts\t tStarts\n"
                    ++"     \tmatch\tmatch\t   \tcount\tbases\tcount\tbases\t      \tname     \tsize\tstart\tend\tname     \tsize\tstart\tend\tcount\n"
                    ++"---------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
