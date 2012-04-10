{-# LANGUAGE OverloadedStrings,TypeSynonymInstances #-}
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
import           Data.Char             (isDigit)
import           Data.List             (intercalate)
import           Data.List.Split       (splitEvery)
import           Data.Maybe            (fromMaybe)
import           Text.Printf           (printf)

data GBRecord = GB {
   locus :: LOCUS
  ,definition :: DEFINITION
  ,accession :: ACCESSION
  ,version :: VERSION
  ,dblink :: Maybe DBLINK
  ,keywords :: KEYWORDS
  ,segment :: Maybe SEGMENT
  ,source :: SOURCE
  ,references :: [REFERENCE]
  ,comment :: Maybe COMMENT
  ,features :: [FEATURE]
  ,origin :: ORIGIN
  }
                
newtype SEGMENT = SEGMENT ByteString

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
                                  
data MoleculeType = MoleculeType !Polymer !(Maybe Topology)
data Topology = Linear                    
              | Circular
                         deriving (Show,Read)
data Polymer = DNA
             | RNA
             | PRO
             deriving (Show,Eq,Read)
                      
data LOCUS = LOCUS { 
   locusName :: !ByteString
  ,sequenceLength :: {-# UNPACK #-} !Int
  ,moleculeType :: !MoleculeType
  ,geneBankDivision :: !GeneBankDivision
  ,modificationDate :: !ByteString
  }
             
        
newtype DEFINITION = DEFINITION ByteString
newtype ACCESSION = ACCESSION ByteString
data VERSION = VERSION ByteString !GI
newtype GI = GI ByteString
newtype KEYWORDS = KEYWORDS ByteString
newtype SOURCE = SOURCE ORGANISM
newtype COMMENT = COMMENT ByteString
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
               
data FEATURE = Allele !LocDescriptor ![(ByteString,ByteString)]          -- ^ allele		Obsolete; see variation feature key
             | Attenuator !LocDescriptor ![(ByteString,ByteString)]      -- ^ attenuator	Sequence related to transcription termination
             | C_region !LocDescriptor ![(ByteString,ByteString)]        -- ^ C_region	Span of the C immunological feature
             | CAAT_signal !LocDescriptor ![(ByteString,ByteString)]     -- ^ CAAT_signal	`CAAT box' in eukaryotic promoters
             | CDS !LocDescriptor ![(ByteString,ByteString)]             -- ^ CDS		Sequence coding for amino acids in protein (includes stop codon)
             | Conflict !LocDescriptor ![(ByteString,ByteString)]        -- ^ conflict	Independent sequence determinations differ
             | D_loop !LocDescriptor ![(ByteString,ByteString)]          -- ^ D-loop      	Displacement loop
             | D_segment !LocDescriptor ![(ByteString,ByteString)]       -- ^ D_segment	Span of the D immunological feature
             | Enhancer !LocDescriptor ![(ByteString,ByteString)]        -- ^ enhancer	Cis-acting enhancer of promoter function
             | Exon !LocDescriptor ![(ByteString,ByteString)]            -- ^ exon		Region that codes for part of spliced mRNA
             | Gene !LocDescriptor ![(ByteString,ByteString)]            -- ^ gene            Region that defines a functional gene, possibly including upstream (promotor, enhancer, etc) and downstream control elements, and for which a name has been assigned.
             | GC_signal !LocDescriptor ![(ByteString,ByteString)]       -- ^ GC_signal	`GC box' in eukaryotic promoters
             | IDNA !LocDescriptor ![(ByteString,ByteString)]            -- ^ iDNA		Intervening DNA eliminated by recombination
             | Intron !LocDescriptor ![(ByteString,ByteString)]          -- ^ intron		Transcribed region excised by mRNA splicing
             | J_region !LocDescriptor ![(ByteString,ByteString)]        -- ^ J_region	Span of the J immunological feature
             | LTR !LocDescriptor ![(ByteString,ByteString)]             -- ^ LTR		Long terminal repeat
             | Mat_peptide !LocDescriptor ![(ByteString,ByteString)]     -- ^ mat_peptide	Mature peptide coding region (does not include stop codon)
             | Misc_binding !LocDescriptor ![(ByteString,ByteString)]    -- ^ misc_binding	Miscellaneous binding site
             | Misc_difference !LocDescriptor ![(ByteString,ByteString)] -- ^ misc_difference	Miscellaneous difference feature
             | Misc_feature !LocDescriptor ![(ByteString,ByteString)]    -- ^ misc_feature	Region of biological significance that cannot be described by any other feature
             | Misc_recomb !LocDescriptor ![(ByteString,ByteString)]     -- ^ misc_recomb	Miscellaneous recombination feature
             | Misc_RNA !LocDescriptor ![(ByteString,ByteString)]        -- ^ misc_RNA	Miscellaneous transcript feature not defined by other RNA keys
             | Misc_signal !LocDescriptor ![(ByteString,ByteString)]     -- ^ misc_signal	Miscellaneous signal
             | Misc_structure !LocDescriptor ![(ByteString,ByteString)]  -- ^ misc_structure	Miscellaneous DNA or RNA structure
             | Modified_base !LocDescriptor ![(ByteString,ByteString)]   -- ^ modified_base	The indicated base is a modified nucleotide
             | MRNA !LocDescriptor ![(ByteString,ByteString)]            -- ^ mRNA		Messenger RNA
             | Mutation !LocDescriptor ![(ByteString,ByteString)]        -- ^ mutation 	Obsolete: see variation feature key
             | N_region !LocDescriptor ![(ByteString,ByteString)]        -- ^ N_region	Span of the N immunological feature
             | Old_sequence !LocDescriptor ![(ByteString,ByteString)]    -- ^ old_sequence	Presented sequence revises a previous version
             | PolyA_signal !LocDescriptor ![(ByteString,ByteString)]    -- ^ polyA_signal	Signal for cleavage & polyadenylation
             | PolyA_site !LocDescriptor ![(ByteString,ByteString)]      -- ^ polyA_site	Site at which polyadenine is added to mRNA
             | Precursor_RNA !LocDescriptor ![(ByteString,ByteString)]   -- ^ precursor_RNA	Any RNA species that is not yet the mature RNA product
             | Prim_transcript !LocDescriptor ![(ByteString,ByteString)] -- ^ prim_transcript	Primary (unprocessed) transcript
             | Primer !LocDescriptor ![(ByteString,ByteString)]          -- ^ primer		Primer binding region used with PCR
             | Primer_bind !LocDescriptor ![(ByteString,ByteString)]     -- ^ primer_bind	Non-covalent primer binding site
             | Promoter !LocDescriptor ![(ByteString,ByteString)]        -- ^ promoter	A region involved in transcription initiation
             | Protein_bind !LocDescriptor ![(ByteString,ByteString)]    -- ^ protein_bind	Non-covalent protein binding site on DNA or RNA
             | RBS !LocDescriptor ![(ByteString,ByteString)]             -- ^ RBS		Ribosome binding site
             | Rep_origin !LocDescriptor ![(ByteString,ByteString)]      -- ^ rep_origin	Replication origin for duplex DNA
             | Repeat_region !LocDescriptor ![(ByteString,ByteString)]   -- ^ repeat_region	Sequence containing repeated subsequences
             | Repeat_unit !LocDescriptor ![(ByteString,ByteString)]     -- ^ repeat_unit	One repeated unit of a repeat_region
             | RRNA !LocDescriptor ![(ByteString,ByteString)]            -- ^ rRNA		Ribosomal RNA
             | S_region !LocDescriptor ![(ByteString,ByteString)]        -- ^ S_region	Span of the S immunological feature
             | Satellite !LocDescriptor ![(ByteString,ByteString)]       -- ^ satellite	Satellite repeated sequence
             | ScRNA !LocDescriptor ![(ByteString,ByteString)]           -- ^ scRNA		Small cytoplasmic RNA
             | Sig_peptide !LocDescriptor ![(ByteString,ByteString)]     -- ^ sig_peptide	Signal peptide coding region
             | SnRNA !LocDescriptor ![(ByteString,ByteString)]           -- ^ snRNA		Small nuclear RNA
             | Source !LocDescriptor ![(ByteString,ByteString)]          -- ^ source		Biological source of the sequence data represented by a GenBank record. Mandatory feature, one or more per record. For organisms that have been incorporated within the NCBI taxonomy database, an associated /db_xref="taxon:NNNN" qualifier will be present (where NNNNN is the numeric identifier assigned to the organism within the NCBI taxonomy database).
             | Stem_loop !LocDescriptor ![(ByteString,ByteString)]       -- ^ stem_loop	Hair-pin loop structure in DNA or RNA
             | STSite !LocDescriptor ![(ByteString,ByteString)]          -- ^ STS		Sequence Tagged Site; operationally unique sequence that identifies the combination of primer spans used in a PCR assay
             | TATA_signal !LocDescriptor ![(ByteString,ByteString)]     -- ^ TATA_signal	`TATA box' in eukaryotic promoters
             | Terminator !LocDescriptor ![(ByteString,ByteString)]      -- ^ terminator	Sequence causing transcription termination
             | Transit_peptide !LocDescriptor ![(ByteString,ByteString)] -- ^ transit_peptide	Transit peptide coding region
             | Transposon !LocDescriptor ![(ByteString,ByteString)]      -- ^ transposon	Transposable element (TN)
             | TRNA !LocDescriptor ![(ByteString,ByteString)]            -- ^ tRNA 		Transfer RNA
             | Unsure !LocDescriptor ![(ByteString,ByteString)]          -- ^ unsure		Authors are unsure about the sequence in this region
             | V_region !LocDescriptor ![(ByteString,ByteString)]        -- ^ V_region	Span of the V immunological feature
             | Variation !LocDescriptor ![(ByteString,ByteString)]       -- ^ variation 	A related population contains stable mutation
             | Hyphen !LocDescriptor ![(ByteString,ByteString)]          -- ^ - (hyphen)	Placeholder
             | Signal_10 !LocDescriptor ![(ByteString,ByteString)]       -- ^ -10_signal	`Pribnow box' in prokaryotic promoters
             | Signal_35 !LocDescriptor ![(ByteString,ByteString)]       -- ^ -35_signal	`-35 box' in prokaryotic promoters
             | Clip3' !LocDescriptor ![(ByteString,ByteString)]          -- ^ 3'clip		3'-most region of a precursor transcript removed in processing
             | UTR3' !LocDescriptor ![(ByteString,ByteString)]           -- ^ 3'UTR		3' untranslated region (trailer)
             | Clip5' !LocDescriptor ![(ByteString,ByteString)]          -- ^ 5'clip		5'-most region of a precursor transcript removed in processing
             | UTR5' !LocDescriptor ![(ByteString,ByteString)]           -- ^ 5'UTR		5' untranslated region (leader)
             | Region !LocDescriptor ![(ByteString,ByteString)] -- ^ Region
             | Protein !LocDescriptor ![(ByteString,ByteString)] -- ^ Protein

data LocDescriptor = Base {-# UNPACK #-} !Int
                   | Loc !(Maybe [Operator]) !Span
                   | Site {-# UNPACK #-} !Int {-# UNPACK #-} !Int 
                   | OneIn {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                     
data Range = Range {
   completeness :: {-# UNPACK #-} !Completeness 
  ,begin :: {-# UNPACK #-} !Int 
  ,end :: {-# UNPACK #-} !Int
  } 

data Span = Single !Range
          | Multiple ![Range]
            
data Completeness = PartialAt3' 
                  | PartialAt5'
                  | PartialAtBothEnd
                  | Complete
                    deriving (Eq,Show)
                    
data Operator = Complement
              | Join
              | Order
                deriving (Eq,Show)

newtype CONTIG = CONTIG ByteString
newtype ORIGIN = ORIGIN ByteString

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
                case poly of
                  PRO     -> "aa"
                  _       -> "bp"
          in unit ++ "\t" ++ show poly ++ "\t" ++ show str
        MoleculeType poly _ -> 
          let unit =
                case poly of
                  PRO     -> "aa"
                  _       -> "bp"
          in unit ++ "\t" ++ show poly 

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
    
instance Show Range where
  show (Range c b e) =
    case c of
      PartialAt5'      -> "<" ++ show b ++ ".." ++ show e
      PartialAt3'      -> show b ++ ".." ++ ">" ++ show e
      PartialAtBothEnd -> "<" ++ show b ++ ".." ++ ">" ++ show e
      _                -> show b ++ ".." ++ show e
      
instance Show Span where
  show sp =
    case sp of
      Single s    -> show s
      Multiple ss -> intercalate "," $ map show ss

instance Show LocDescriptor where
  show loc = 
    case loc of
      Base i     -> show i
      Site a b   -> show a ++ "^" ++ show b
      OneIn a b  -> show a ++ "." ++ show b
      Loc ops sp -> case ops of 
        Nothing                 -> show sp
        Just [Complement]       -> "complement(" ++ show sp ++ ")"
        Just [Join]             -> "join(" ++ show sp ++ ")"
        Just [Order]            -> "order(" ++ show sp ++ ")"
        Just [Complement,Join]  -> "complement(join(" ++ show sp ++ "))"
        Just [Complement,Order] -> "complement(order(" ++ show sp ++ "))"
        _                       -> error "Unrecognized LocDescriptor."
        
        
instance Show FEATURE where
  show feature =
    case feature of 
      Allele loc ps -> myShow loc ps "allele"
      Attenuator loc ps -> myShow loc ps "attenuator"
      C_region loc ps -> myShow loc ps "C_region"
      CAAT_signal loc ps -> myShow loc ps "CAAT_signal"
      CDS loc ps -> myShow loc ps "CDS"
      Conflict loc ps -> myShow loc ps "conflict"
      D_loop loc ps -> myShow loc ps "D-loop"
      D_segment loc ps -> myShow loc ps "D_segment"
      Enhancer loc ps -> myShow loc ps "enhancer"
      Exon loc ps -> myShow loc ps "exon"
      Gene loc ps -> myShow loc ps "gene"
      GC_signal loc ps -> myShow loc ps "GC_signal"
      IDNA loc ps -> myShow loc ps "iDNA"
      Intron loc ps -> myShow loc ps "intron"
      J_region loc ps -> myShow loc ps "J_region"
      LTR loc ps -> myShow loc ps "LTR"
      Mat_peptide loc ps -> myShow loc ps "mat_peptide"
      Misc_binding loc ps -> myShow loc ps "misc_binding"
      Misc_difference loc ps -> myShow loc ps "misc_difference"
      Misc_feature loc ps -> myShow loc ps "misc_feature"
      Misc_recomb loc ps -> myShow loc ps "misc_recomb"
      Misc_RNA loc ps -> myShow loc ps "misc_RNA"
      Misc_signal loc ps -> myShow loc ps "misc_signal"
      Misc_structure loc ps -> myShow loc ps "misc_structure"
      Modified_base loc ps -> myShow loc ps "modified_base"
      MRNA loc ps -> myShow loc ps "mRNA"
      Mutation loc ps -> myShow loc ps "mutation"
      N_region loc ps -> myShow loc ps "N_region"
      Old_sequence loc ps -> myShow loc ps "old_sequence"
      PolyA_signal loc ps -> myShow loc ps "polyA_signal"
      PolyA_site loc ps -> myShow loc ps "polyA_site"
      Precursor_RNA loc ps -> myShow loc ps "precursor_RNA"
      Prim_transcript loc ps -> myShow loc ps "prim_transcript"
      Primer loc ps -> myShow loc ps "primer"
      Primer_bind loc ps -> myShow loc ps "primer_bind"
      Promoter loc ps -> myShow loc ps "promoter"
      Protein_bind loc ps -> myShow loc ps "protein_bind"
      RBS loc ps -> myShow loc ps "RBS"
      Rep_origin loc ps -> myShow loc ps "rep_origin"
      Repeat_region loc ps -> myShow loc ps "repeat_region"
      Repeat_unit loc ps -> myShow loc ps "repeat_unit"
      RRNA loc ps -> myShow loc ps "rRNA"
      S_region loc ps -> myShow loc ps "S_region"
      Satellite loc ps -> myShow loc ps "satellite"
      ScRNA loc ps -> myShow loc ps "scRNA"
      Sig_peptide loc ps -> myShow loc ps "sig_peptide"
      SnRNA loc ps -> myShow loc ps "snRNA"
      Source loc ps -> myShow loc ps "source"
      Stem_loop loc ps -> myShow loc ps "stem_loop"
      STSite loc ps -> myShow loc ps "STS"
      TATA_signal loc ps -> myShow loc ps "TATA_signal"
      Terminator loc ps -> myShow loc ps "terminator"
      Transit_peptide loc ps -> myShow loc ps "transit_peptide"
      Transposon loc ps -> myShow loc ps "transposon"
      TRNA loc ps -> myShow loc ps "tRNA"
      Unsure loc ps -> myShow loc ps "unsure"
      V_region loc ps -> myShow loc ps "V_region"
      Variation  loc ps -> myShow loc ps "variation"
      Hyphen loc ps -> myShow loc ps "-"
      Signal_10 loc ps -> myShow loc ps "-10_signal"
      Signal_35 loc ps -> myShow loc ps "-35_signal"
      Clip3' loc ps -> myShow loc ps "3'clip"
      UTR3' loc ps -> myShow loc ps "3'UTR"
      Clip5' loc ps -> myShow loc ps "5'clip"
      UTR5' loc ps -> myShow loc ps "5'UTR"
      Region loc ps -> myShow loc ps "Region"
      Protein loc ps -> myShow loc ps "Protein"
    where
      myShow l vs str = s5 ++ str ++ "\t" ++ show l ++ "\n" ++ showPS vs
      s5 = replicate 5 ' '
      showPS ss = 
        B8.unpack $ B8.intercalate "\n" $ 
        map (\(k,v) -> 
              let v' = if all isDigit $ unpack v
                       then v
                       else '"' `B8.cons` v `B8.snoc` '"'
                  s21 = (B8.pack $ replicate 21 ' ') `B8.snoc` '/'
              in if k /= "translation"
                 then s21 `B8.append` k `B8.snoc` '=' `B8.append` v'
                 else let (vh,vt) = B8.splitAt 45 v'
                          s' = '\n' `B8.cons` (B8.pack $ replicate 21 ' ')
                          vS = B8.intercalate s' $ 
                               vh:(map B8.pack $ splitEvery 58 $ B8.unpack vt)
                      in s21 `B8.append` k `B8.snoc` '=' `B8.append` vS
            ) ss

                       
instance Show ORIGIN where
  show (ORIGIN str) = "ORIGIN" ++ "\n" ++
                      (intercalate "\n" $
                       map (\(i,s) -> 
                             printf "%10d" (i * 60 + 1) ++ " " ++ s) $
                       zip ([0..]::[Int]) $ map (intercalate " ") $ 
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
     (\rs -> 
       intercalate "\n" $ map 
       (\(i,r) -> 
         "REFERENCE\t" ++ show i ++ "\n" ++ show r
           ) $
       zip [1..] rs ) . Just . references
    ,fmap show . comment
    ,fmap (("FEATURES\tLocation/Qualifiers\n" ++) . 
           intercalate "\n" . map show) . Just . features
    ,fmap show . Just . origin] $ 
    repeat gb
