import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Digest.Pure.MD4
import System.Environment
import Numeric
import Control.Monad
import Control.Arrow

main = getArgs >>= \ args -> forM_ args $ \ path -> do
        bs <- L.readFile path
        putStr path
        putChar '\t'
        S.foldl (\m w -> m >> putStr (showHex w ""))
                (return ())
                (md4 bs)
        putStrLn ""
