module AI.Data.MNIST 
(
 getTrainingImgs
,getTrainingLabels
,getTrainingSet
,getTestImgs
,getTestLabels
,getTestSet
,flattenSet
,flattenImage
) where

import qualified Data.Array.Repa as R
import Data.Array.Repa(Z(..), (:.)(..), transpose)
import Data.Array.Repa.ByteString(fromByteString)

import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip(decompress)
import System.FilePath
import Data.Binary
import Data.Binary.Get(getByteString)
import Data.Word

import Data.Either
import Control.Monad
import Control.Applicative((<$>), (<*>))

type IntImage = R.Array R.DIM2 Int
type Image = R.Array R.DIM2 Double

newtype Images = Images { unImages :: [Image] }
newtype Labels = Labels { unLabels :: [Int] }

{-# INLINE openTrainingImgs #-}
openTrainingImgs   loc = decompress <$> B.readFile (loc </> "train-images-idx3-ubyte.gz")
{-# INLINE openTrainingLabels #-}
openTrainingLabels loc = decompress <$> B.readFile (loc </> "train-labels-idx1-ubyte.gz")

{-# INLINE openTestImgs #-}
openTestImgs   loc = decompress <$> B.readFile (loc </> "t10k-images-idx3-ubyte.gz")
{-# INLINE openTestLabels #-}
openTestLabels loc = decompress <$> B.readFile (loc </> "t10k-labels-idx1-ubyte.gz")

instance Binary Images where
    put = undefined -- we dont want any serializing
    get = do
      magic <- (get :: Get Word32)
      case magic of
        0x00000803 -> do
                   numImgs <- fromIntegral <$> (get :: Get Word32)
                   h <- fromIntegral <$> (get :: Get Word32)
                   w <- fromIntegral <$> (get :: Get Word32)
                   let imgSize = w * h
                       getImg = (R.map ((/256.0) . realToFrac) . fromByteString (Z :. h :. w)) <$> getByteString imgSize
                   imgs <- sequence $ replicate numImgs getImg
                   return $ Images imgs
        _ ->  error "Error decoding MNIST image stream, bad magic id!"

instance Binary Labels where
    put = undefined -- we dont want any serializing
    get = do
      magic <- (get :: Get Word32)
      case magic of
        0x00000801 -> do
                   numLabels <- fromIntegral <$> (get :: Get Word32)
                   labels <- sequence $ replicate numLabels (fromIntegral <$> (get :: Get Word8))
                   return $ Labels labels
        _ ->  error "Error decoding MNIST label stream, bad magic id!"

{-# INLINE getImgs #-}
getImgs :: (FilePath -> IO B.ByteString) -> FilePath -> IO [Image]
getImgs f loc = unImages . decode <$> f loc

{-# INLINE getLabels #-}
getLabels :: (FilePath -> IO B.ByteString) -> FilePath -> IO [Int]
getLabels f loc = unLabels . decode <$> f loc

{-# INLINE getSet #-}
getSet :: (FilePath -> IO [Image]) -> (FilePath -> IO [Int]) -> FilePath -> IO [(Image, Int)]
getSet fi fl loc = zip <$> fi loc <*> fl loc

getTrainingImgs :: FilePath -> IO [Image]
getTrainingImgs = getImgs openTrainingImgs

getTrainingLabels :: FilePath -> IO [Int]
getTrainingLabels = getLabels openTrainingLabels

getTrainingSet :: FilePath -> IO [(Image, Int)]
getTrainingSet = getSet getTrainingImgs getTrainingLabels

getTestImgs :: FilePath -> IO [Image]
getTestImgs = getImgs openTestImgs

getTestLabels :: FilePath -> IO [Int]
getTestLabels = getLabels openTestLabels

getTestSet :: FilePath -> IO [(Image, Int)]
getTestSet = getSet getTestImgs getTestLabels

flattenSet :: [(Image, Int)] -> [(Image, Int)]
flattenSet = map (\(m, n) -> (flattenImage m, n))

flattenImage :: Image -> Image
flattenImage m = R.reshape (Z :. (w * h) :. 1) m
    where (Z :. w :. h) = R.extent m
