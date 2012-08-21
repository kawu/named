{-# LANGUAGE OverloadedStrings #-}

module Text.Named.TeiNCP
( ID
, Cert (..)
, Ptr (..)
, Deriv (..)
, Para (..)
, Sent (..)
, NE (..)
, parseNamed
, readNamed
) where

import System.FilePath (takeBaseName)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup
import qualified Data.Named as Cano

type ID = L.Text

data Cert
    = High
    | Medium
    | Low
    deriving (Show)

data Ptr
    -- | Of "#id" form
    = Local
        { target    :: ID }
    -- | Of "loc#id" form
    | Global
        { target    :: ID
        , location  :: L.Text }
    deriving (Show)

data Deriv = Deriv
    { derivType :: L.Text 
    , derivFrom :: L.Text }
    deriving (Show)

-- | Paragraph.
data Para = Para
    { paraID    :: ID
    , sentences :: [Sent] }
    deriving (Show)

-- | Sentence.
data Sent = Sent
    { sentID    :: ID
    , names     :: [NE] }
    deriving (Show)

-- | A Seg element in a file. 
data NE = NE
    { neID      :: ID
    , derived   :: Maybe Deriv
    , neType    :: L.Text
    , subType   :: Maybe L.Text
    , orth      :: L.Text
    -- | Left base or Right when.
    , base      :: Either L.Text L.Text
    , cert      :: Cert
    , certComment   :: Maybe L.Text
    , ptrs      :: [Ptr] }
    deriving (Show)

-- | TEI NKJP ann_morphosyntax parser.
type P a = XmlParser L.Text a

namedP :: P [Para]
namedP = true //> paraP

paraP :: P Para
paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

sentP :: P Sent
sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> nameP)

nameP :: P NE
nameP = (tag "seg" *> getAttr "xml:id") `join` \neID -> do
    ne <- nameBodyP
    ptrs <- some namePtrP
        <|> failBad ("no targets specified for " ++ L.unpack neID)
    return $ ne { neID = neID, ptrs = ptrs }

nameBodyP :: P NE
nameBodyP = (tag "fs" *> hasAttr "type" "named") `joinR` do
    deriv <- optional derivP
    neType <- fSymP "type"
    subType <- optional (fSymP "subtype")
    orth <- fStrP "orth"
    base <- (Left  <$> fStrP "base")
        <|> (Right <$> fStrP "when")
    cert <- certP
    certComment <- optional (fStrP "comment")
    return $ NE { neType = neType, subType = subType, orth = orth, base = base
                , derived = deriv, cert = cert, certComment = certComment
                , neID = "", ptrs = [] }    -- ^ Should be supplied outside

derivP :: P Deriv
derivP = fP "derived" `joinR` ( fsP "derivation" `joinR` do
    Deriv <$> fSymP "derivType" <*> fStrP "derivedFrom" )
    
fP x  = tag "f"  *> hasAttr "name" x
fsP x = tag "fs" *> hasAttr "type" x

certP :: P Cert
certP =
    mkCert <$> fSymP "certainty"
  where
    mkCert "high"   = High
    mkCert "medium" = Medium
    mkCert "low"    = Low
    mkCert _        = Medium    -- ^ It should not happen!

namePtrP :: P Ptr
namePtrP = cut (tag "ptr" *> getAttr "target") >>= \x -> return $
    case L.break (=='#') x of
        (ptr, "")   -> Local ptr
        (loc, ptr)  -> Global
            { location = loc
            , target = (L.tail ptr) }

fStrP :: L.Text -> P L.Text
fStrP x =
    let checkName = tag "f" *> hasAttr "name" x
        -- | Body sometimes is empty.
        safeHead [] = ""
        safeHead xs = head xs
    in  safeHead <$> (checkName #> tag "string" /> text)

fSymP :: L.Text -> P L.Text
fSymP x =
    let checkName = tag "f" *> hasAttr "name" x
        p = cut (tag "symbol" *> getAttr "value")
    in  head <$> (checkName /> p)

parseNamed :: L.Text -> [Para]
parseNamed = parseXML namedP

-- | NKJP .tar.gz handling.  TODO: Move NCP parsing utilities
-- to another, common module.  It should also allow parsing
-- plain directories.

-- | Parse NCP .tar.gz corpus.
readNamed :: FilePath -> IO [(FilePath, [Para])]
readNamed tarPath = do
    map parseEntry . withBase "ann_named" <$> readTar tarPath

readTar :: FilePath -> IO [Tar.Entry]
readTar tar
    =  Tar.foldEntries (:) [] error
    .  Tar.read . GZip.decompress
   <$> BS.readFile tar

parseEntry :: Tar.Entry -> (FilePath, [Para])
parseEntry entry =
    (Tar.entryPath entry, parseNamed content)
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = L.decodeUtf8 binary

withBase :: String -> [Tar.Entry] -> [Tar.Entry]
withBase base = filter ((==base) . takeBaseName . Tar.entryPath)
