module Lib (alice, bob) where

import           Data.Word
import           Data.Bits
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as NSBS
import qualified Data.Store as S
import qualified Data.Vector.Unboxed as U

strToHostAddress :: String -> NS.HostAddress
strToHostAddress xs = NS.tupleToHostAddress (ys :: (Word8, Word8, Word8, Word8))
    where ys = read $ "(" ++ loop xs ++ ")"
          loop [] = []
          loop (t:ts) = if t == '.' then ',':(loop ts) else t:(loop ts)

a = 255 -- in base 2 this is 11111111
b = 170 -- in base 2 this is 10101010
c = a `xor` b -- 01010101_2 = 85_10

-- `bs size` is a bytestring of _size_ * 1024 bytes.
-- This bytestring represents array with all numbers occur even number of times, except _a_ and _b_
-- which occur odd number of times. Folding of this array with `xor` function should yield _c_.
bs :: Int -> BS.ByteString
bs size = S.encode y
    where n = size * 512 - 1
          x = U.enumFromN 0 n
          y :: U.Vector Word8
          y = U.singleton a U.++ x U.++ U.reverse x U.++ U.singleton b

decode :: BS.ByteString -> U.Vector Word8
decode = f . S.decode
    where f (Left _) = U.fromList []
          f (Right v) = v

-- alice is a sender
alice :: Int -> String -> Int -> IO ()
alice port to size = putStrLn "I'm Alice." >>
    NS.socket NS.AF_INET NS.Stream 0 >>= -- IPv4 TCP socket
    \sock -> NS.connect sock (NS.SockAddrInet (fromIntegral port) (strToHostAddress to)) >>
    putStrLn "Sending data..." >>
    NSBS.sendAll sock (bs size) >>
    putStrLn "Data is sent!" >>
    NS.close sock >>
    putStrLn "Bye!"

-- bob is a receiver
bob :: Int -> IO ()
bob port = putStrLn "I'm Bob." >>
    NS.socket NS.AF_INET NS.Stream 0 >>=
    \sock -> NS.bind sock (NS.SockAddrInet (fromIntegral port) NS.iNADDR_ANY) >>
    NS.listen sock 1 >>
    NS.accept sock >>=
    \(sock', _) -> NSBS.recv sock' 4096 >>=
    return . decode >>=
    \x -> print x >> --putStrLn "Data received," >>
    let d = U.foldr xor 0 x in
    if d == c then putStrLn "    ok." else putStrLn "    inconsistent!" >>
    putStrLn "Bye!"
