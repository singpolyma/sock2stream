import Network
import System.IO
import Control.Monad (forever,when)
import Control.Concurrent.CHP
import Data.Binary
import qualified Data.ByteString.Lazy as LZ

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Connection ID and data chunk
data Msg = Msg Word32 LZ.ByteString

main :: IO ()
main = withSocketsDo . runCHP_ $ do
	(stdoutRecv, stdoutSend) <- newChannelRW
	sock <- liftIO_CHP $ listenOn (UnixSocket "/tmp/test")
	stdoutServer stdoutRecv <|*|> listen sock stdoutSend

stdoutServer :: Chanin Msg -> CHP ()
stdoutServer stdoutRecv = forever $ do
	(Msg id chunk) <- readChannel stdoutRecv
	liftIO_CHP $ do
		let length = fromIntegral $ LZ.length chunk
		LZ.putStr $ encode id
		LZ.putStr $ encode (length :: Word32)
		LZ.putStr chunk

-- TODO: Replace incrementing IDs with ID server
listen :: Socket -> Shared Chanout Msg -> CHP ()
listen sock stdoutSend = listen' 0
	where
	listen' next = do
		handle <- doAccept
		listen' (next + 1) <|*|> handle2stdout next handle stdoutSend
	doAccept = liftIO_CHP $ do
		(handle,_,_) <- accept sock
		hSetBinaryMode handle True
		return handle

handle2stdout :: Word32 -> Handle -> Shared Chanout Msg -> CHP ()
handle2stdout id handle stdoutSend = forever $ do
	chunk <- liftIO_CHP $ LZ.hGetNonBlocking handle 2048
	when (LZ.length chunk > 0) $
		claim stdoutSend (`writeChannel` Msg id chunk)
