{-# LANGUAGE OverloadedStrings #-}
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
module Bio.Sequence.GB.Types
       
       where
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack,append)
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (isDigit,toLower)
import           Data.List             (intercalate)
import           Data.List.Split       (splitEvery)
import           Data.Maybe            (fromMaybe)
import           Text.Printf           (printf)

getDEFINITION :: GBRecord -> ByteString
getDEFINITION = definitionToBS . definition
                
getACCESSION :: GBRecord -> ByteString
getACCESSION = accessionToBS . accession

getKEYWORDS :: GBRecord -> ByteString
getKEYWORDS = keywordsToBS . keywords

getSOURCE :: GBRecord -> ORGANISM
getSOURCE = sourceToOG . source

data GBRecord = GB {
   locus :: LOCUS
  ,definition :: DEFINITION
  ,accession :: ACCESSION
  ,version :: VERSION
  ,dblink :: Maybe DBLINK
  ,keywords :: KEYWORDS
  ,segment :: Maybe SEGMENT
  ,source :: SOURCE
  ,references :: Maybe [REFERENCE] -- e.g. NM_053042  has no reference
  ,comment :: Maybe COMMENT
  ,features :: [FEATURE]
  ,origin :: ORIGIN
  }
                

data GeneBankDivision = PRI -- ^ primate sequences
                      | ROD -- ^ rodent sequences
                      | MAM -- ^ other mammalian sequences
                      | VRT -- ^ other vertebrate sequences
                      | INV -- ^ invertebrate sequences
                      | PLN -- ^ plant, fungal, and algal sequences
                      | BCT -- ^ bacterial sequences
                      | VRL -- ^ viral sequences
                      | PHG -- ^ bacteriophage sequences
                      | SYN -- ^ synthetic sequences
                      | UNA -- ^ unannotated sequences
                      | EST -- ^ EST sequences (expressed sequence tags)
                      | PAT -- ^ patent sequences
                      | STS -- ^ STS sequences (sequence tagged sites)
                      | GSS -- ^ GSS sequences (genome survey sequences)
                      | HTG -- ^ HTG sequences (high-throughput genomic sequences)
                      | HTC -- ^ unfinished high-throughput cDNA sequencing
                      | ENV -- ^ environmental sampling sequences
                        deriving (Show,Read)
                                  
data MoleculeType = MoleculeType {
   molType :: !ByteString 
  ,topo :: !(Maybe Topology)
  }
                    
data Topology = Linear                    
              | Circular
                         deriving (Show,Read)
-- data Polymer = DNA
--              | RNA
--              | PRO
--              deriving (Show,Eq,Read)
                      
data LOCUS = LOCUS { 
   locusName :: !ByteString
  ,sequenceLength :: {-# UNPACK #-} !Int
  ,moleculeType :: !MoleculeType
  ,geneBankDivision :: !GeneBankDivision
  ,modificationDate :: !ByteString
  }
             
        
data VERSION = VERSION ByteString !GI
type Genus = ByteString
type Species = ByteString
data ORGANISM = ORGANISM !Genus !Species
data DBLINK = Project !ByteString
            | BioProject !ByteString
data REFERENCE = REFERENCE {
   author :: !(Maybe ByteString)
  ,consortium :: !(Maybe ByteString)
  ,title :: !(Maybe ByteString)
  ,journal :: !ByteString
  ,pubmed :: !(Maybe ByteString)
  ,remark :: !(Maybe ByteString)
  }
               
data FEATURE = FEATURE {
   feature :: !ByteString 
  ,locationDescriptor :: !ByteString
  ,values :: ![(ByteString,ByteString)]
  }


newtype SEGMENT = SEGMENT {
  segmentToBS :: ByteString
  }
                  
newtype CONTIG = CONTIG {
  contigToBS :: ByteString
  }
                 
newtype ORIGIN = ORIGIN { 
  originToBS :: ByteString
  }
                 
newtype DEFINITION = DEFINITION {
  definitionToBS :: ByteString
  }
                     
newtype ACCESSION = ACCESSION {
  accessionToBS :: ByteString
  }
                    
newtype GI = GI {
  giToBS :: ByteString
  }
             
newtype KEYWORDS = KEYWORDS {
  keywordsToBS :: ByteString
  }
                   
newtype SOURCE = SOURCE { 
  sourceToOG :: ORGANISM
  }
                 
newtype COMMENT = COMMENT { 
  commentToBS :: ByteString
  }

instance Show LOCUS where             
  show (LOCUS name len mole gbd date) = 
    "LOCUS" ++ "\t" ++
    unpack name ++ "\t" ++
    show len ++ 
    " " ++ locusStr ++ "\t" ++
    show gbd ++
    "\t" ++ unpack date
    where 
      locusStr = case mole of
        MoleculeType poly (Just str) -> 
          let unit = 
                if "NP_" `B8.isPrefixOf` name
                then "aa"
                else "bp"
          in unit ++ "\t" ++ unpack poly ++ "\t" ++ (map toLower $ show str)
        MoleculeType poly _ -> 
          let unit =
                if "NP_" `B8.isPrefixOf` name
                then "aa"
                else "bp"
          in unit ++ "\t" ++ unpack poly 

instance Show DEFINITION where
  show (DEFINITION str) = "DEFINITION\t" ++ unpack str

instance Show  ACCESSION where
  show (ACCESSION str) = "ACCESSION\t" ++ unpack str
  
instance Show VERSION where
  show (VERSION str1 (GI str2)) = "VERSION\t" ++ unpack str1 ++  
                                  "\t" ++ unpack str2 
instance Show DBLINK where
  show dbl =
    case dbl of
      Project str -> "DBLINK\t" ++ "Project: " ++ unpack str
      BioProject str -> "DBLINK\t" ++ "BioProject: " ++ unpack str

instance Show KEYWORDS where
  show (KEYWORDS str) = "KEYWORDS\t" ++ unpack str
  
instance Show SEGMENT where  
  show (SEGMENT str) = "SEGMENT\t" ++ unpack str
  
instance Show SOURCE where
  show (SOURCE (ORGANISM str1 str2)) = "SOURCE\n  " ++ "ORGANISM\t" ++ 
                                       unpack str1 ++ " " ++ unpack str2
                                       
instance Show REFERENCE where
  show art = 
    unpack $
    B8.intercalate "\n" $
    filter (not . B8.null) $
    map (fromMaybe "") $
    zipWith ($)  
    [fmap (s2 `append` "AUTHORS\t" `append`) . author
    ,fmap (s2 `append` "CONSRTM\t" `append`) . consortium
    ,fmap (s2 `append` "TITLE\t" `append`) . title
    ,fmap (s2 `append` "JOURNAL\t" `append`) . Just . journal
    ,fmap (s3 `append` "PUBMED\t" `append`) . pubmed
    ,fmap (s2 `append` "REMARK\t" `append`) . remark] $ 
    repeat art
    where 
      s2 = "  "
      s3 = "   "
      
instance Show COMMENT where
  show (COMMENT str) = "COMMENT\t" ++ unpack str
    
        
instance Show FEATURE where
  show (FEATURE na loc ps) =
    myShow na loc ps 
    where
      myShow str l vs = s5 ++ unpack str ++ "\t" ++ unpack l ++ "\n" ++ showPS vs
      s5 = replicate 5 ' '
      showPS ss = 
        B8.unpack $ B8.intercalate "\n" $ 
        map (\(k,v) -> 
              let v' = if (all isDigit $ unpack v) || (k == "number") || (k == "citation")
                       then v
                       else '"' `B8.cons` v `B8.snoc` '"'
                  s21 = B8.pack (replicate 21 ' ') `B8.snoc` '/'
              in if k /= "translation"
                 then s21 `B8.append` k `B8.snoc` '=' `B8.append` v'
                 else let (vh,vt) = B8.splitAt 45 v'
                          s' = '\n' `B8.cons` B8.pack (replicate 21 ' ')
                          vS = B8.intercalate s' $ 
                               vh:map B8.pack (splitEvery 58 $ B8.unpack vt)
                      in s21 `B8.append` k `B8.snoc` '=' `B8.append` vS
            ) ss

                       
instance Show ORIGIN where
  show (ORIGIN str) = "ORIGIN" ++ "\n" ++
                      intercalate "\n" 
                       (map (\(i,s) -> 
                             printf "%10d" (i * 60 + 1) ++ " " ++ s) $
                       zip ([0..]::[Int]) $ map unwords $ 
                       splitEvery 6 $ splitEvery 10 (unpack str)) ++ "\n//"

instance Show GBRecord where
  show gb =
    intercalate "\n" $
    filter (not . null) $
    map (fromMaybe "") $
    zipWith ($) 
    [fmap show . Just . locus
    ,fmap show . Just . definition
    ,fmap show . Just . accession
    ,fmap show . Just . version
    ,fmap show . dblink
    ,fmap show . Just . keywords
    ,fmap show . segment
    ,fmap show . Just . source
    ,fmap 
       (intercalate "\n" . map 
        (\(i,r) -> 
          "REFERENCE\t" ++ show i ++ "\n" ++ show r
        ) . zip ([1..]::[Int])) . references
    ,fmap show . comment
    ,fmap (("FEATURES\tLocation/Qualifiers\n" ++) . 
           intercalate "\n" . map show) . Just . features
    ,fmap show . Just . origin] $ 
    repeat gb
