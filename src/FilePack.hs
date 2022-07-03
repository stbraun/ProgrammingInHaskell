{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | File archiving library.
module FilePack where

import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import qualified System.Posix.Types as PT
import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.&.), (.|.), shift)
import Text.Read (readEither)


data FileContents
    = StringFileContents        String
    | TextFileContents          Text.Text
    | ByteStringFileContents    BS.ByteString
    deriving (Eq, Read, Show)


data FileData a = FileData
    { packedFileName        :: FilePath
    , packedFileSize         :: Word32
    , packedFilePermissions :: PT.FileMode
    , packedFileData        :: a
    } deriving (Eq, Read, Show)


-- Packable does not hold the type information as a parameter,
-- but introduces it in the statement: forall a. Encode a => Packable
-- This allows us to use Packable with different types of FileData in a list.
data Packable = forall a. Encode a => Packable { getPackable :: FileData a }


newtype FilePack = FilePack [Packable]


addFileDataToPack :: Encode a => FileData a -> FilePack -> FilePack
addFileDataToPack a (FilePack as) = FilePack $ (Packable a) : as


infixr 6 .:
(.:) :: (Encode a) => FileData a -> FilePack -> FilePack
(.:) = addFileDataToPack


emptyFilePack :: FilePack
emptyFilePack = FilePack []


class Encode a where
    encode :: a -> BS.ByteString
    encode = BS.drop 4 . encodeWithSize
    encodeWithSize :: a -> BS.ByteString
    encodeWithSize a =
        let s = encode a
            l = fromIntegral $ BS.length s
        in word32ToByteSring l <> s
    {-# MINIMAL encode | encodeWithSize #-}


class Decode a where
    decode :: BS.ByteString -> Either String a


instance Encode BS.ByteString where
    encode = id

instance Decode BS.ByteString where
    decode = Right . id


instance Encode Text.Text where
    encode = encodeUtf8

instance Decode Text.Text where
    decode = Right . decodeUtf8


instance Encode String where
    encode = BC.pack

instance Decode String where
    decode = Right . BC.unpack


instance Encode Word16 where
    encode = word16ToByteSring
    encodeWithSize word =
        let (a, b) = word16ToBytes word
        in BS.pack [2, 0, 0, 0, a, b]

instance Decode Word16 where
    decode = bytestringToWord16


instance Encode Word32 where
    encode = word32ToByteSring
    encodeWithSize word =
        let (a, b, c, d) = word32ToBytes word
        in BS.pack [4, 0, 0, 0, a, b, c, d]

instance Decode Word32 where
    decode = bytestringToWord32


instance Encode PT.FileMode where
    encode (PT.CMode fMode) = encode fMode

instance Decode PT.FileMode where
    decode = fmap PT.CMode . decode


instance Encode a => Encode (FileData a) where
    encode FileData{..} =
        let
            encodedFileName         = encodeWithSize packedFileName
            encodedFileSize         = encodeWithSize packedFileSize
            encodedFilePermissions  = encodeWithSize packedFilePermissions
            encodedFileData         = encodeWithSize packedFileData
            encodedData = encodedFileName
                          <> encodedFileSize
                          <> encodedFilePermissions
                          <> encodedFileData
        in encode encodedData


-- | Encode a tuple.
instance (Encode a, Encode b) => Encode (a, b) where
    encode (a, b) =
        encode $ encodeWithSize a <> encodeWithSize b


-- | Encode a list of encodable values.
instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
    encode = encode . foldMap encodeWithSize


instance Encode Packable where
    encode (Packable p) = encode p


instance Encode FilePack where
    encode (FilePack p) = encode p


-- | Convert a 16bit word into 4 bytes.
word16ToBytes :: Word16 -> (Word8, Word8)
word16ToBytes word =
    let a = fromIntegral $ 0xff .&. word
        b = fromIntegral $ 0xff .&. (shift word (-8))
    in (a, b)


-- | Create a Word16 from a tuple of Word8.
word16FromBytes :: (Word8, Word8) -> Word16
word16FromBytes (a, b) =
    let a' = fromIntegral a
        b' = shift (fromIntegral b) 8
    in a' .|. b'


bytestringToWord16 :: BS.ByteString -> Either String Word16
bytestringToWord16 bytestring =
    case BS.unpack bytestring of
        [a,b] -> Right $ word16FromBytes (a,b)
        _otherwise ->
            let l = show $ BS.length bytestring
            in Left ("Expecting 2 bytes but got " <> l)


-- | Create new ByteString from Word16.
word16ToByteSring :: Word16 -> BS.ByteString
word16ToByteSring word =
    let (a, b) = word16ToBytes word
    in BS.pack [a, b]


-- | Cons Word16 to the beginning of an existing ByteString.
consWord16 :: Word16 -> BS.ByteString -> BS.ByteString
consWord16 word bytestring =
    let packedWord = word16ToByteSring word
    in packedWord <> bytestring


-- | Convert a 32bit word into 4 bytes.
word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
    let a = fromIntegral $ 0xff .&. word
        b = fromIntegral $ 0xff .&. (shift word (-8))
        c = fromIntegral $ 0xff .&. (shift word (-16))
        d = fromIntegral $ 0xff .&. (shift word (-24))
    in (a, b, c, d)


-- | Create a Word32 from a tuple of Word8.
word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
    let a' = fromIntegral a
        b' = shift (fromIntegral b) 8
        c' = shift (fromIntegral c) 16
        d' = shift (fromIntegral d) 24
    in a' .|. b' .|. c' .|. d'


bytestringToWord32 :: BS.ByteString -> Either String Word32
bytestringToWord32 bytestring =
    case BS.unpack bytestring of
        [a,b,c,d] -> Right $ word32FromBytes (a,b,c,d)
        _otherwise ->
            let l = show $ BS.length bytestring
            in Left ("Expecting 4 bytes but got " <> l)


-- | Create new ByteString from Word32.
word32ToByteSring :: Word32 -> BS.ByteString
word32ToByteSring word =
    let (a, b, c, d) = word32ToBytes word
    in BS.pack [a, b, c, d]


-- | Cons Word32 to the beginning of an existing ByteString.
consWord32 :: Word32 -> BS.ByteString -> BS.ByteString
consWord32 word bytestring =
    let packedWord = word32ToByteSring word
    in packedWord <> bytestring


-- Test code
testEncodeValue :: BS.ByteString
testEncodeValue =
    let
        a = FileData
            { packedFileName = "a"
            , packedFileSize = 543
            , packedFilePermissions = 0755
            , packedFileData = "foo" :: String }
        b = FileData
            { packedFileName = "b"
            , packedFileSize = 10
            , packedFilePermissions = 0644
            , packedFileData = ["hello","world"] :: [Text.Text] }
        c = FileData
            { packedFileName = "c"
            , packedFileSize = 8192
            , packedFilePermissions = 0644
            , packedFileData = (0,"zero") :: (Word32,String) }
    in encode $ a .: b .: c .: emptyFilePack

