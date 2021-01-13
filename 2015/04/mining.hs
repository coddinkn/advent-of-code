import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (ByteString, pack, take, all)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.List (find)
import Data.Maybe (fromJust)
import Prelude hiding (take, all)

hasFirstNZeros :: Int -> ByteString -> Bool
hasFirstNZeros n = all (== '0') . take n

generateMD5 :: ByteString -> ByteString
generateMD5 = toStrict . toLazyByteString . byteStringHex . hash

makeByteString :: String -> Int -> ByteString
makeByteString prefix n = pack $ prefix ++ show n 

hasFirstNZeroMD5 :: String -> Int -> Int -> Bool
hasFirstNZeroMD5 prefix n number = 
    let md5Hash = generateMD5 $ makeByteString prefix number
    in hasFirstNZeros n md5Hash

findFirstNZeroMD5 :: String -> Int -> Int
findFirstNZeroMD5 prefix n = fromJust $ find (hasFirstNZeroMD5 prefix n) [1 ..]

main :: IO ()
main = do
    prefix <- filter (/= '\n') <$> readFile "input.txt"
    putStr "Part 1: "
    print $ findFirstNZeroMD5 prefix 5
    putStr "Part 2: "
    print $ findFirstNZeroMD5 prefix 6
