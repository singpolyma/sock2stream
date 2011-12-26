import Network
import System.IO
import Control.Exception
import Control.Monad (forever,when)
import Control.Concurrent.CHP
import Data.Binary
import qualified Data.ByteString.Lazy as LZ

import Data.Map (Map)
import qualified Data.Map as Map

type ConnID = Word32
data OutMsg = OutMsg ConnID LZ.ByteString
data ConnMsg = RegMsg ConnID Handle | URegMsg ConnID | WriteMsg ConnID LZ.ByteString

main :: IO ()
main = withSocketsDo . runCHP_ $ do
	(connRecv, connSend) <- newChannelRW
	(stdoutRecv, stdoutSend) <- newChannelRW
	sock <- liftIO_CHP $ listenOn (UnixSocket "/tmp/test")
	connectionManager connRecv <|*|> stdinServer connSend <|*|>
		stdoutServer stdoutRecv <|*|> listen sock connSend stdoutSend

stdoutServer :: Chanin OutMsg -> CHP ()
stdoutServer stdoutRecv = forever $ do
	(OutMsg id chunk) <- readChannel stdoutRecv
	liftIO_CHP $ do
		let length = fromIntegral $ LZ.length chunk
		LZ.putStr $ encode id
		LZ.putStr $ encode (length :: Word16)
		LZ.putStr chunk

connectionManager :: Chanin ConnMsg -> CHP ()
connectionManager connRecv = manager Map.empty
	where
	manager handles = do
		value <- readChannel connRecv
		case value of
			RegMsg id handle -> manager $ Map.insert id handle handles
			URegMsg id -> manager $ Map.delete id handles
			WriteMsg id chunk ->
				case Map.lookup id handles of
					Just handle -> do
						liftIO_CHP $ LZ.hPutStr handle chunk
						manager handles
					Nothing -> manager handles -- Maybe error msg?

stdinServer :: Shared Chanout ConnMsg -> CHP ()
stdinServer nameSend = forever $ do
	chunk <- liftIO_CHP $ LZ.hGet stdin 6
	-- Got 6 bytes: connection tag and length, now get data
	let (id, len) = LZ.splitAt 4 chunk
	let length = fromIntegral (decode len :: Word16)
	body <- liftIO_CHP $ LZ.hGet stdin length
	claim nameSend (`writeChannel` WriteMsg (decode id) body)

-- TODO: Replace incrementing IDs with ID server
listen :: Socket -> Shared Chanout ConnMsg -> Shared Chanout OutMsg -> CHP ()
listen sock connSend stdoutSend = listen' 0
	where
	listen' next = do
		handle <- doAccept
		listen' (next + 1) <|*|> do
			-- Tell connectionManager about this new connection
			first <- embedCHP $
				claim connSend (`writeChannel` RegMsg next handle)
			-- Handle new connection
			connection <- embedCHP $ handleConnection next handle stdoutSend
			-- Tell stdinServer we are closing up
			final <- embedCHP $ claim connSend (`writeChannel` URegMsg next)
			-- Now that we've set it up, run it safely
			liftIO_CHP $ bracket_ first final connection
	doAccept = liftIO_CHP $ do
		(handle,_,_) <- accept sock
		hSetBinaryMode handle True
		return handle

handleConnection :: ConnID -> Handle -> Shared Chanout OutMsg -> CHP ()
handleConnection id handle stdoutSend = forever $ do
	chunk <- liftIO_CHP $ LZ.hGetNonBlocking handle 2048
	when (LZ.length chunk > 0) $
		claim stdoutSend (`writeChannel` OutMsg id chunk)
