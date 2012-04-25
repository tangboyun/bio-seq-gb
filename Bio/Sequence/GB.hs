-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Bio.Sequence.GB
       (
         readGB
       , toStr
       , fromByteString
       )
       where

import           Bio.Sequence.GB.Parser
import           Bio.Sequence.GB.Types
import           Bio.Sequence.SeqData
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B8
import           Data.Conduit                     (runResourceT,($$))
import           Data.Conduit.Attoparsec          (sinkParser)
import           Data.Conduit.Binary              (sourceFile)

readGB :: FilePath -> IO [GBRecord]
readGB fp = runResourceT $ sourceFile fp $$ sinkParser parseGBs

toStr :: GBRecord -> String
toStr = show

fromByteString :: ByteString -> Maybe GBRecord
fromByteString = maybeResult . parse (many space *> parseGB <* many space)
  
-- toSeq :: GBRecord -> Sequence a
-- toSeq gb = undefined

-- extractExons,extractUTR3',extractUTR5',extractGene,extractCDS,extractProtein :: GBRecord -> Maybe [Sequence a]
-- extractExons = undefined
-- extractUTR3' = undefined
-- extractUTR5' = undefined
-- extractGene = undefined
-- extractCDS = undefined
-- extractProtein = undefined

