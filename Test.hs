import Crypto.Hash.MD4
import Data.Digest.Pure.MD4
import Data.ByteString.Lazy
import Test.QuickCheck

instance Arbitrary ByteString where arbitrary = fmap pack $ vector =<< choose (0, 100000)

main :: IO ()
main = quickCheck $ \ bs -> md4 bs == hashlazy bs
