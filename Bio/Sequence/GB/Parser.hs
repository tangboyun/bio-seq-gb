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
module Bio.Sequence.GB.Parser
       where
import           Bio.Sequence.GB.Types
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (toUpper,isPrint,isAlphaNum,isAlpha)

parseGBs :: Parser [GBRecord]
parseGBs = do
  many1 (parseGB <* many space)

parseGB :: Parser GBRecord
parseGB = do
  loc <- parseLOCUS <* endOfLine
  def <- parseDEFINITION <* endOfLine
  acc <- parseACCESSION <* endOfLine
  ver <- parseVERSION <* endOfLine
  dbl <- optional $ parseDBLINK <* endOfLine
  key <- parseKEYWORDS <* endOfLine
  seg <- optional $ parseSEGMENT <* endOfLine
  sou <- parseSOURCE 
  arts <- many1 $ endOfLine *> parseARTICLE
  endOfLine
  com <- optional $ parseCOMMENT <* endOfLine
  fea <- string "FEATURES" *> manyTill anyChar (try endOfLine) *> 
         many1 (parseFEATURE <* endOfLine)
  ori <- parseORIGIN
  return $! GB loc def acc ver dbl key seg sou arts com fea ori
 
parseLOCUS :: Parser LOCUS
parseLOCUS = do
  string "LOCUS"
  skipSpace
  name <- takeWhile1 (not . isSpace)
  skipSpace
  len  <- decimal
  skipSpace
  string "bp" <|> string "aa"
  skipSpace
  poly <- many1 letter_ascii
  topo <- fmap (fmap ((zipWith ($) (toUpper:repeat id)) . B8.unpack)) $ 
          optional $ 
          many1 space *> (string "linear" <|> string "circular")
  skipSpace
  gbd <- many1 letter_ascii
  skipSpace
  date <- takeWhile1 (not . isSpace)
  return $! LOCUS name len (MoleculeType (read poly) (fmap read topo)) (read gbd) date
 
parseDEFINITION :: Parser DEFINITION
parseDEFINITION = do
  string "DEFINITION"
  skipSpace
  ls <- takeWhile1 (\c -> isPrint c || c == ' ')
  lss <- optional $
        many1 $ endOfLine *> many1 (char ' ') *> 
                takeWhile1 isPrint
  case lss of
    Just lss' -> return $! DEFINITION $ B8.intercalate " " $ ls : lss'
    _         -> return $! DEFINITION ls
      
parseACCESSION :: Parser ACCESSION
parseACCESSION = do
  string "ACCESSION"
  skipSpace
  access <- takeWhile1 isAlphaNum
  return $! ACCESSION $ access

parseVERSION :: Parser VERSION
parseVERSION = do
  string "VERSION"
  skipSpace
  str <- takeWhile1 (/= '.')
  char '.'
  ver <- decimal
  skipSpace
  gi <- parseGI
  return $! VERSION (str `B8.snoc` '.' `B8.append` (B8.pack $ show ver)) gi
  where
    parseGI = do
      str1 <- stringCI "GI:"
      str2 <- takeWhile1 isDigit
      return $! GI $ str1 `B8.append` str2

parseDBLINK :: Parser DBLINK
parseDBLINK = do
  string "DBLINK"
  skipSpace
  parseProject <|> parseBioProject
  where
    parseProject = do
      string "Project:"
      skipSpace 
      str <- takeWhile1 isDigit
      return $! Project str
    parseBioProject = do
      string "BioProject:"
      skipSpace
      string "PRJ"
      c1 <- char 'E' <|> char 'N' <|> char 'D'
      c2 <- letter_ascii
      str <- takeWhile1 isDigit
      return $! BioProject $! "PRJ" `B8.snoc` c1 `B8.snoc` c2 `B8.append` str
      
  
parseKEYWORDS :: Parser KEYWORDS
parseKEYWORDS = do
  string "KEYWORDS"
  skipSpace
  str <- parseMaybeMultiLines
  return $! KEYWORDS str

parseSEGMENT :: Parser SEGMENT
parseSEGMENT = do
  string "SEGMENT"
  skipSpace
  str <- parseMaybeMultiLines
  return $! SEGMENT str
  
parseSOURCE :: Parser SOURCE
parseSOURCE = do
  string "SOURCE"
  manyTill anyChar (try endOfLine)
  skipSpace
  string "ORGANISM"
  skipSpace 
  genus <- takeWhile1 isAlpha
  skipSpace
  spe <- takeWhile1 isAlpha
  many1 $ endOfLine *> string "  " *> many1 (char ' ') *> 
                takeWhile1 isPrint
  return $! SOURCE $ ORGANISM genus spe

parseARTICLE :: Parser REFERENCE
parseARTICLE = do
  string "REFERENCE"
  takeTill (\c -> c == '\n' || c == '\r')
  a <- optional $ endOfLine *> parseSubKeyword "AUTHORS" 
  c <- optional $ endOfLine *> parseSubKeyword "CONSRTM" 
  t <- optional $ endOfLine *> parseSubKeyword "TITLE" 
  j <- endOfLine *> parseSubKeyword "JOURNAL" 
  p <- optional $ endOfLine *> many1 space *> string "PUBMED" *> skipSpace *> many1 digit
  r <- optional $ endOfLine *> parseSubKeyword "REMARK" 
  return $! REFERENCE a c t j (fmap B8.pack p) r
  
parseMaybeMultiLines :: Parser ByteString
parseMaybeMultiLines = do
  a <- takeWhile1 isPrint
  as <- optional $
        many1 $ endOfLine *> string "   " *> many1 (char ' ') *>
        takeWhile1 isPrint
  case as of
    Just as' -> return $! B8.intercalate " " $ a : as'
    _        -> return a

parseSubKeyword :: ByteString -> Parser ByteString
parseSubKeyword keyword = do
  replicateM_ 2 (char ' ')
  string keyword
  skipSpace
  result <- parseMaybeMultiLines
  return $! result

parseCOMMENT :: Parser COMMENT
parseCOMMENT = do
  string "COMMENT"
  skipSpace
  str <- parseMaybeMultiLines
  return $! COMMENT str

parseFEATURE :: Parser FEATURE
parseFEATURE = do
  many1 $ char ' ' 
  str <- takeWhile1 $ not . isSpace
  skipSpace
  loc <- parseLOCDes
  endOfLine
  q <- parseQualifier
  qs_ <- optional $ many1 $ endOfLine *> parseQualifier
  let qs = case qs_ of
        Nothing -> [q]
        Just qs' -> q:qs'
  case str of
    "allele"          -> return $! Allele loc qs
    "attenuator"      -> return $! Attenuator loc qs
    "C_region"        -> return $! C_region loc qs
    "CAAT_signal"     -> return $! CAAT_signal loc qs
    "CDS"             -> return $! CDS loc qs
    "conflict"        -> return $! Conflict loc qs
    "D-loop"          -> return $! D_loop loc qs
    "D_segment"       -> return $! D_segment loc qs
    "enhancer"        -> return $! Enhancer loc qs
    "exon"            -> return $! Exon loc qs
    "gene"            -> return $! Gene loc qs
    "GC_signal"       -> return $! GC_signal loc qs
    "iDNA"            -> return $! IDNA loc qs
    "intron"          -> return $! Intron loc qs
    "J_region"        -> return $! J_region loc qs
    "LTR"             -> return $! LTR loc qs
    "mat_peptide"     -> return $! Mat_peptide loc qs
    "misc_binding"    -> return $! Misc_binding loc qs
    "misc_difference" -> return $! Misc_difference loc qs
    "misc_feature"    -> return $! Misc_feature loc qs
    "misc_recomb"     -> return $! Misc_recomb loc qs
    "misc_RNA"        -> return $! Misc_RNA loc qs
    "misc_signal"     -> return $! Misc_signal loc qs
    "misc_structure"  -> return $! Misc_structure loc qs
    "modified_base"   -> return $! Modified_base loc qs
    "mRNA"            -> return $! MRNA loc qs
    "mutation"        -> return $! Mutation loc qs
    "N_region"        -> return $! N_region loc qs
    "old_sequence"    -> return $! Old_sequence loc qs
    "polyA_signal"    -> return $! PolyA_signal loc qs
    "polyA_site"      -> return $! PolyA_site loc qs
    "precursor_RNA"   -> return $! Precursor_RNA loc qs
    "prim_transcript" -> return $! Prim_transcript loc qs
    "primer"          -> return $! Primer loc qs
    "primer_bind"     -> return $! Primer_bind loc qs
    "promoter"        -> return $! Promoter loc qs
    "protein_bind"    -> return $! Protein_bind loc qs
    "RBS"             -> return $! RBS loc qs
    "rep_origin"      -> return $! Rep_origin loc qs
    "repeat_region"   -> return $! Repeat_region loc qs
    "repeat_unit"     -> return $! Repeat_unit loc qs
    "rRNA"            -> return $! RRNA loc qs
    "S_region"        -> return $! S_region loc qs
    "satellite"       -> return $! Satellite loc qs
    "scRNA"           -> return $! ScRNA loc qs
    "sig_peptide"     -> return $! Sig_peptide loc qs
    "snRNA"           -> return $! SnRNA loc qs
    "source"          -> return $! Source loc qs
    "stem_loop"       -> return $! Stem_loop loc qs
    "STS"             -> return $! STSite loc qs
    "TATA_signal"     -> return $! TATA_signal loc qs
    "terminator"      -> return $! Terminator loc qs
    "transit_peptide" -> return $! Transit_peptide loc qs
    "transposon"      -> return $! Transposon loc qs
    "tRNA"            -> return $! TRNA loc qs
    "unsure"          -> return $! Unsure loc qs
    "V_region"        -> return $! V_region loc qs
    "variation"       -> return $! Variation loc qs
    "-"               -> return $! Hyphen loc qs
    "-10_signal"      -> return $! Signal_10 loc qs
    "-35_signal"      -> return $! Signal_35 loc qs
    "3'clip"          -> return $! Clip3' loc qs
    "3'UTR"           -> return $! UTR3' loc qs
    "5'clip"          -> return $! Clip5' loc qs
    "5'UTR"           -> return $! UTR5' loc qs
    "Region"          -> return $! Region loc qs
    "Protein"         -> return $! Protein loc qs
    _                 -> error "Unrecognized feature.\n"
  where
    parseLOCDes = 
      let parseBase = Base <$> decimal
          parseSite = do
            d1 <- decimal
            char '^'
            d2 <- decimal
            return $! Site d1 d2
          parseOneIn = do
            d1 <- decimal
            char '.'
            d2 <- decimal
            return $! OneIn d1 d2
          parseOneLoc = do
            p1 <- optional $ string "<"
            d1 <- decimal
            string ".."
            p2 <- optional $ string ">"
            d2 <- decimal
            case p1 of
              Nothing   -> case p2 of
                Nothing -> return $! Range Complete d1 d2
                _       -> return $! Range PartialAt3' d1 d2
              _         -> case p2 of
                Nothing -> return $! Range PartialAt5' d1 d2
                _       -> return $! Range PartialAtBothEnd d1 d2
          parseLOC = do
            prefix <- optional $ 
                      choice [string "complement" <* string "("
                             ,string "join" <* string "("
                             ,string "order" <* string "("]
            case prefix of 
              Nothing -> do
                l <- parseOneLoc
                return $! Loc Nothing $ Single l
              Just "complement" -> do
                prefix' <- optional $ 
                           choice [string "join" <* string "("
                                  ,string "order" <* string "("]
                case prefix' of
                  Nothing -> do      
                    l <- parseOneLoc
                    ls <- optional $ many1 $ 
                          many space *> string "," *> many space *> parseOneLoc
                    case ls of
                      Nothing -> do 
                        many space 
                        string ")" 
                        return $! Loc (Just [Complement]) $ Single l
                      Just ls' -> do
                        many space
                        string ")"
                        return $! Loc (Just [Complement]) $ Multiple $ l:ls'
                  Just "join" -> do
                    l <- parseOneLoc
                    ls <- optional $ many1 $ 
                          many space *> string "," *> many space *> parseOneLoc
                    case ls of
                      Nothing -> do 
                        many space 
                        string "))" 
                        return $! Loc (Just [Complement,Join]) $ Single l
                      Just ls' -> do
                        many space
                        string "))"
                        return $! Loc (Just [Complement,Join]) $ Multiple $ l:ls'
                  Just "order" -> do
                    l <- parseOneLoc
                    ls <- optional $ many1 $ 
                          many space *> string "," *> many space *> parseOneLoc
                    case ls of
                      Nothing -> do 
                        many space 
                        string "))" 
                        return $! Loc (Just [Complement,Order]) $ Single l
                      Just ls' -> do
                        many space
                        string "))"
                        return $! Loc (Just [Complement,Order]) $ Multiple $ l:ls'

              Just "join" -> do
                l <- parseOneLoc
                ls <- optional $ many1 $ 
                      many space *> string "," *> many space *> parseOneLoc
                case ls of
                  Nothing -> do 
                    many space 
                    string ")" 
                    return $! Loc (Just [Join]) $ Single l
                  Just ls' -> do
                    many space
                    string ")"
                    return $! Loc (Just [Join]) $ Multiple $ l:ls'
              Just "order" -> do
                l <- parseOneLoc
                ls <- optional $ many1 $ 
                      many space *> string "," *> many space *> parseOneLoc
                case ls of
                  Nothing -> do 
                    many space 
                    string ")" 
                    return $! Loc (Just [Order]) $ Single l
                  Just ls' -> do
                    many space
                    string ")"
                    return $! Loc (Just [Order]) $ Multiple $ l:ls'
      in choice [parseLOC
                ,parseBase
                ,parseSite
                ,parseOneIn]
    parseQualifier = do
      replicateM_ 21 $ char ' ' -- qualifier begin at Col 22
      char '/'
      k <- takeWhile1 (\c -> isAlpha c || c == '_')
      char '='
      c <- optional $ char '"'
      case c of
        Nothing -> do 
          num <- decimal
          return $! (k, B8.pack $! show num)
        _ -> do
          t <- takeWhile1 (\c -> isPrint c && c /= '"')
          ts' <- optional $ many1 $ endOfLine *>
                replicateM_ 21 (char ' ') *>
                takeWhile1 (\c -> isPrint c && c /= '"')
          char '"'
          case ts' of
            Just ts -> return $! (k, B8.concat $! t : ts)
            _       -> return $! (k, t)

parseCONTIG :: Parser CONTIG
parseCONTIG = do
  string "CONTIG"
  skipSpace
  str <- parseMaybeMultiLines
  return $! CONTIG str

parseORIGIN :: Parser ORIGIN
parseORIGIN = do
  string "ORIGIN"
  many $ satisfy (/= '\n')
  endOfLine
  seqStr <- manyTill anyChar (try $ string "//")
  return $! ORIGIN $! B8.pack $ filter isAlpha seqStr


