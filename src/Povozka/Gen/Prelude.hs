{-# LANGUAGE FunctionalDependencies #-}
module Povozka.Gen.Prelude where

-- {-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot, FunctionalDependencies #-}

import Data.Binary qualified
import Data.Binary.Get qualified
import Data.Binary.Put qualified
import Data.ByteString qualified
import Data.Vector qualified

import Control.Monad qualified
import Data.Bits qualified
import Data.Word qualified

import Prelude hiding (String)

newtype Long' = Long' Data.Word.Word64 deriving (Show)

instance Data.Binary.Binary Long' where
  get = Long' <$> Data.Binary.Get.getWord64le
  put (Long' x) = Data.Binary.Put.putWord64le x

newtype Int' = Int' Int deriving (Show)

instance Data.Binary.Binary Int' where
  get = Int' . fromIntegral <$> Data.Binary.Get.getInt32le
  put (Int' x) = Data.Binary.Put.putInt32le (fromIntegral x)

newtype Int128' = Int128' Data.ByteString.ByteString deriving (Show)

instance Data.Binary.Binary Int128' where
  get = Int128' <$> Data.Binary.Get.getByteString 16
  put (Int128' x) = Data.Binary.Put.putByteString x

newtype Int256' = Int256' Data.ByteString.ByteString deriving (Show)

instance Data.Binary.Binary Int256' where
  get = Int256' <$> Data.Binary.Get.getByteString 32
  put (Int256' x) = Data.Binary.Put.putByteString x

newtype Bytes' = Bytes' Data.ByteString.ByteString deriving (Show)

type String' = Bytes'

instance Data.Binary.Binary Bytes' where
  get = do
    fstByte <- Data.Binary.Get.getWord8
    case fstByte of
      254 -> do
        b1 <- fromIntegral <$> Data.Binary.Get.getWord8
        b2 <- fromIntegral <$> Data.Binary.Get.getWord8
        b3 <- fromIntegral <$> Data.Binary.Get.getWord8
        let sz = b1 + b2 * 16 + b3 * 256
        s <- Data.Binary.Get.getByteString sz
        _ <- Data.Binary.Get.getByteString ((4 - (sz `mod` 4)) `mod` 4)
        pure $ Bytes' s
      _ -> do
        s <- Data.Binary.Get.getByteString (fromIntegral fstByte)
        _ <- Data.Binary.Get.getByteString ((4 - (fromIntegral fstByte `mod` 4)) `mod` 4)
        pure $ Bytes' s

  put (Bytes' s) = do
    if size <= 253
      then Data.Binary.Put.putWord8 (fromIntegral size)
      else do
        Data.Binary.Put.putWord8 254
        Data.Binary.Put.putWord8 (fromIntegral (size Data.Bits..&. 0xff))
        Data.Binary.Put.putWord8 (fromIntegral ((size `Data.Bits.unsafeShiftR` 8) Data.Bits..&. 0xff))
        Data.Binary.Put.putWord8 (fromIntegral ((size `Data.Bits.unsafeShiftR` 16) Data.Bits..&. 0xff))
    Data.Binary.Put.putByteString s
    Data.Binary.Put.putByteString pad
    where
      size = Data.ByteString.length s
      headerSize = if size <= 253 then 1 else 4
      padSize = (4 - ((size + headerSize) `mod` 4)) `mod` 4
      pad = Data.ByteString.replicate padSize 0

newtype Vector' a = Vector' (Data.Vector.Vector a) deriving (Show)

instance Data.Binary.Binary a => Data.Binary.Binary (Vector' a) where
  get = do
    size <- Data.Binary.Get.getWord32le
    elements <- Control.Monad.replicateM (fromIntegral size) Data.Binary.get
    pure $ Vector' $ Data.Vector.fromList elements
  put (Vector' v) = do
    let size = fromIntegral $ Data.Vector.length v
    Data.Binary.Put.putWord32le size
    Control.Monad.forM_ (Data.Vector.toList v) Data.Binary.put

newtype Vector a = Vector (Vector' a) deriving (Show)

instance Data.Binary.Binary a => Data.Binary.Binary (Vector a) where
  get = do
    sig <- Data.Binary.Get.getWord32le
    case sig of
      0x15c4b51c -> Vector <$> Data.Binary.get
      _ -> fail $ "expected Vector (0x15c4b51c), got " <> show sig
  put (Vector v) = Data.Binary.Put.putWord32le 0x15c4b51c >> Data.Binary.put v

class TLFunctionʼ i o | i -> o