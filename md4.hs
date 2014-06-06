{-# LANGUAGE NoMonomorphismRestriction #-}
module MD4(md4) where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString as BS (length)

f x y z = x .&. y .|. (complement x) .&. z
g x y z = x .&. y .|. x .&. z .|. y .&. z
h x y z = x `xor` y `xor` z

abcd f a b c d = f a b c d
dabc f a b c d = f d a b c
cdab f a b c d = f c d a b
bcda f a b c d = f b c d a

store1 x (a,b,c,d) = (x,b,c,d)
store2 x (a,b,c,d) = (a,x,c,d)
store3 x (a,b,c,d) = (a,b,x,d)
store4 x (a,b,c,d) = (a,b,c,x)

get1 (x,_,_,_) = x
get2 (_,x,_,_) = x
get3 (_,_,x,_) = x
get4 (_,_,_,x) = x

op f n k s x a b c d =
  rotateL (a + (f b c d) + (x!!k) + n) s

op1 = op f 0
op2 = op g 0x5a827999
op3 = op h 0x6ed9eba1

params1 = [ 0, 3,  1, 7,  2, 11,  3, 19
          , 4, 3,  5, 7,  6, 11,  7, 19
          , 8, 3,  9, 7, 10, 11, 11, 19
          ,12, 3, 13, 7, 14, 11, 15, 19]

params2 = [0, 3, 4, 5,  8, 9, 12, 13
          ,1, 3, 5, 5,  9, 9, 13, 13
          ,2, 3, 6, 5, 10, 9, 14, 13
          ,3, 3, 7, 5, 11, 9, 15, 13]

params3 = [0, 3,  8, 9, 4, 11, 12, 15
          ,2, 3, 10, 9, 6, 11, 14, 15
          ,1, 3,  9, 9, 5, 11, 13, 15
          ,3, 3, 11, 9, 7, 11, 15, 15]

apply x op p k s = p go (gets get1, modify . store1)
                        (gets get2, modify . store2)
                        (gets get3, modify . store3)
                        (gets get4, modify . store4)
  where go (a, store) (b,_) (c,_) (d,_) =
           store =<< (op k s x <$> a <*> b <*> c <*> d)

on app = go
  where go [] = return ()
        go (k1:s1:k2:s2:k3:s3:k4:s4:r)
             = app abcd k1 s1
            >> app dabc k2 s2
            >> app cdab k3 s3
            >> app bcda k4 s4
            >> go r

proc x = do
    (aa,bb,cc,dd) <- get
    go op1 params1
    go op2 params2
    go op3 params3
    modify $ \(a,b,c,d) -> (a+aa, b+bb, c+cc, d+dd)
  where go op params = apply x op `on` params

md4 s = output $ execState (go (prep s)) (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)
  where go [] = return ()
        go s = proc (take 16 s) >> go (drop 16 s)

prep = getWords . pad

pad bs = runPut $ do
  putByteString bs
  putWord8 0x80
  replicateM_ (mod (55 - BS.length bs) 64) (putWord8 0)
  putWord64le (fromIntegral (BS.length bs) * 8)

getWords = runGet words
  where words = isEmpty >>= (\e -> if e then return [] else (:) <$> getWord32le <*> words)

output (a,b,c,d) = runPut (mapM_ putWord32le [a,b,c,d])
