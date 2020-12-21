module Main where


import Control.Parallel
import Data.Bits
import qualified Data.ByteString as BS
import Data.List (minimumBy)
import Data.Word (Word8)
import System.Environment
import System.IO (FilePath)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename1, filename2] -> countErrors filename1 filename2
    _ -> putStrLn "usage: error-count <filename> <filename>"


countErrors :: FilePath -> FilePath -> IO ()
countErrors filename1 filename2 = do
  file1 <- BS.readFile filename1
  file2 <- BS.readFile filename2
  let len1 = BS.length file1
      len2 = BS.length file2
  if len1 == len2
  then putStrLn $ errorCountDisplay len1 (leastError file1 file2)
  else putStrLn "Files must be the same length"


maxOffset :: Int
maxOffset = 1000


samplesPerSecond :: Int
samplesPerSecond = 44100


sampleSize :: Int
sampleSize = 5 * samplesPerSecond


leastError :: BS.ByteString -> BS.ByteString -> Int
leastError a b =
  let f (_,erra) (_,errb) = compare erra errb
      bestOffset = fst (minimumBy f (tryOffsets (BS.take sampleSize a) (BS.take sampleSize b)))
  in snd (tryOffset a b bestOffset)


tryOffsets :: BS.ByteString -> BS.ByteString -> [(Int, Int)]
{-tryOffsets a b = (w `par` x `par` y `par` z) `seq` (w ++ x ++ y ++ z)
  where w = tryOffset a b <$> [negate maxOffset .. negate hmo]
        x = tryOffset a b <$> [negate hmo .. 0]
        y = tryOffset a b <$> [0 .. hmo]
        z = tryOffset a b <$> [hmo .. maxOffset]
        hmo = maxOffset `div` 2-}
tryOffsets a b = tryOffset a b <$> [-maxOffset..maxOffset]


tryOffset :: BS.ByteString -> BS.ByteString -> Int -> (Int, Int)
tryOffset a b x =
   if x <= 0
   then let x' = abs x in (x, bitsOfDifference (BS.take (len - x') a) (BS.drop x' b))
   else (x, bitsOfDifference (BS.drop x a) (BS.take (len - x) b))
  where len = BS.length a


bitsOfDifference :: BS.ByteString -> BS.ByteString -> Int
bitsOfDifference a b = sum $ zipWith wordBitsOfDifference (BS.unpack a) (BS.unpack b)


wordBitsOfDifference :: Word8 -> Word8 -> Int
wordBitsOfDifference a b = popCount $ xor a b


errorCountDisplay :: Int -> Int -> String
errorCountDisplay totalBytes bitsOfError =
  let totalBits = 8 * totalBytes
      accuracy = 100 * (1 - fromIntegral bitsOfError / fromIntegral totalBits)
  in show bitsOfError <> " bits of error out of " <> show totalBits <> " total bits; " <> show accuracy <> "% accuracy"
