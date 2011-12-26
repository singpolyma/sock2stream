import Network
import System.IO
import Control.Monad (unless)
import Control.Concurrent.CHP
import Data.Binary
import qualified Data.ByteString.Lazy as LZ

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main :: IO ()
main = withSocketsDo . runCHP_ $ do
	sock <- liftIO_CHP $ listenOn (UnixSocket "/tmp/test")
	listen sock 0

-- TODO: Replace incrementing IDs with ID server
listen :: Socket -> Word32 -> CHP ()
listen sock next = do
	handle <- doAccept
	listen sock (next + 1) <|*|> handle2stdout next handle
	where
	doAccept = liftIO_CHP $ do
		(handle,_,_) <- accept sock
		hSetBinaryMode handle True
		return handle

handle2stdout :: Word32 -> Handle -> CHP ()
handle2stdout id handle = liftIO_CHP doChunk
	where
	doChunk = do
		chunk <- LZ.hGetNonBlocking handle 2048
		let length = fromIntegral (LZ.length chunk)
		unless (length < 1) $ do
			LZ.putStr $ encode id
			LZ.putStr $ encode (length :: Word32)
			LZ.putStr chunk
		doChunk
