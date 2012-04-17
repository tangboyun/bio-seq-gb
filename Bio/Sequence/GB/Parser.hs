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
       -- (
       --   parseGBs
       -- , parseGB
       -- )
       where
import           Bio.Sequence.GB.Types
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as At
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (toUpper,isPrint,isAlpha)
import Data.Maybe

parseGBs :: Parser [GBRecord]
parseGBs = many1 $ many space *> parseGB

parseGB :: Parser GBRecord
parseGB = do
  loc  <- parseLOCUS <* endOfLine                <?> "Parsing error: LOCUS"
  def  <- parseDEFINITION <* endOfLine           <?> "Parsing error: DEFINITION"
  acc  <- parseACCESSION <* endOfLine            <?> "Parsing error: ACCESSION"
  ver  <- parseVERSION <* endOfLine              <?> "Parsing error: VERSION"
  dbl  <- (optional $ parseDBLINK <* endOfLine)  <?> "Parsing error: DBLINK"
  key  <- parseKEYWORDS <* endOfLine             <?> "Parsing error: KEYWORDS"
  seg  <- (optional $ parseSEGMENT <* endOfLine) <?> "Parsing error: SEGMENT"
  sou  <- parseSOURCE                            <?> "Parsing error: SOURCE"
  arts <- (many1 $ endOfLine *> parseARTICLE)    <?> "Parsing error: REFERENCE"
  endOfLine
  com  <- (optional $ parseCOMMENT <* endOfLine) <?> "Parsing error: COMMENT"
  (optional $ parsePRIMARY *> skipSpace)        <?> "Parsing error: PRIMARY" 
  fea  <- string "FEATURES" *> 
          manyTill anyChar (try endOfLine) *> 
          many1 (parseFEATURE <* endOfLine)     <?> "Parsing error: FEATURES"
  ori  <- parseORIGIN                            <?> "Parsing error: ORIGIN"
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
  topo' <- fmap (fmap ((zipWith ($) (toUpper:repeat id)) . B8.unpack)) $ 
          optional $ 
          many1 space *> (string "linear" <|> string "circular")
  skipSpace
  gbd <- many1 letter_ascii
  skipSpace
  date <- takeWhile1 (not . isSpace)
  return $! LOCUS name len (MoleculeType (B8.pack poly) (fmap read topo')) (read gbd) date
 
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
  access <- parseMaybeMultiLines -- in NM_001002009: ACCESSION   NM_001002009 NR_029372
                                -- in NM_002266: this damn field has multilines...
  return $! ACCESSION $ access

parseVERSION :: Parser VERSION
parseVERSION = do
  string "VERSION"
  skipSpace
  str <- takeWhile1 (/= '.')
  char '.'
  ver <- decimal :: Parser Integer
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
        (many $ satisfy isPrint)
  case as of
    Just as' -> return $! B8.intercalate " " $ a : map B8.pack as'
    _        -> return a

parseSubKeyword :: ByteString -> Parser ByteString
parseSubKeyword keyword = do
  replicateM_ 2 (char ' ')
  string keyword
  skipSpace
  result <- parseMaybeMultiLines
  return $! result

parsePRIMARY :: Parser ()
parsePRIMARY = do
  string "PRIMARY" *> skipSpace *> parseMaybeMultiLines *> return ()
  

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
  qs <- many $ endOfLine *> parseQualifier
  return $! FEATURE str loc $ q:qs      
  where
    parseLOCDes = do
      ss <- optional $ many1 $ 
            string "complement(" <|>
            string "join(" <|>
            string "order("
      case ss of
        Nothing -> takeWhile1 isPrint
        Just ss' -> do 
          cs <- many1 (satisfy (/= ')')) <* replicateM_ (length ss') (char ')')
          return $ B8.concat ss' `B8.append` B8.pack cs `B8.append` B8.pack ( replicate (length ss') ')')
    parseQualifier = do
      replicateM_ 21 $ char ' ' -- qualifier begin at Col 22
      char '/'
      k <- takeWhile1 (\c -> isAlpha c || c == '_')
      char '='
      c <- optional $ char '"'
      case c of
        Nothing -> do 
          num <- decimal :: Parser Integer -- in NM_000222,exon 1434..1627, /number=9b ,typo or what?
          ch <- optional $ many1 letter_ascii
          return $! (k, (B8.pack $ show num) `B8.append` (B8.pack $ fromMaybe "" ch))
        _ -> do
          t <- At.takeWhile (\ch -> isPrint ch && ch /= '"') -- can be empty
          ts' <- optional $ many1 $ endOfLine *>
                 replicateM_ 21 (char ' ') *>
                 At.takeWhile (\ch -> isPrint ch && ch /= '"') -- can be empty
          char '"'
          case ts' of
            Just ts -> return $! (k, B8.concat $! t : ts)
            _       -> return $! (k, t)

-- parseCONTIG :: Parser CONTIG
-- parseCONTIG = do
--   string "CONTIG"
--   skipSpace
--   str <- parseMaybeMultiLines
--   return $! CONTIG str

parseORIGIN :: Parser ORIGIN
parseORIGIN = do
  string "ORIGIN"
  many $ satisfy (/= '\n')
  endOfLine
  seqStr <- manyTill anyChar (try $ string "//")
  return $! ORIGIN $! B8.pack $ filter isAlpha seqStr


