module Main where

import Network
import System (getArgs)
import System.IO
import System.Directory (removeFile)
import System.Console.GetOpt
import Control.Exception
import Control.Monad (forever,when,liftM)
import Control.Concurrent.CHP
import Data.Binary
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Map as Map

type ConnID = Word32
data OutMsg = OutMsg ConnID LZ.ByteString
data ConnMsg = RegMsg ConnID Handle | URegMsg ConnID | WriteMsg ConnID LZ.ByteString

data Flag = Listen String | RListen String

main :: IO ()
main = do
	_ <- configureHandle stdin
	_ <- configureHandle stdout

	(flags, _, errors) <- liftM (getOpt RequireOrder [
		Option ['l'] ["listen"] (ReqArg Listen "SOCK") "Listen on SOCK",
		Option ['r'] ["reverse"] (ReqArg RListen "SOCK") "Connect to SOCK"
		]) getArgs

	if length errors > 0 then mapM_ putStrLn errors else
		start (last flags)

start :: Flag -> IO ()
start (Listen path) = withSocketsDo $ bracket
	(listenOn (UnixSocket path))
	(\sock -> do
		sClose sock
		removeFile path)
	(\sock -> runCHP_ $ do
		(connRecv, connSend) <- newChannelRW
		(stdoutRecv, stdoutSend) <- newChannelRW
		listenManager connRecv <|*|> stdinServer connSend <|*|>
			stdoutServer stdoutRecv <|*|> listen sock connSend stdoutSend)
start (RListen path) = withSocketsDo . runCHP_ $ do
	(connRecv, connSend) <- newChannelRW
	(stdoutRecv, stdoutSend) <- newChannelRW
	reverseManager (UnixSocket path) connRecv stdoutSend <|*|>
		stdinServer connSend <|*|> stdoutServer stdoutRecv

configureHandle :: Handle -> IO Handle
configureHandle handle = do
	hSetBinaryMode handle True
	hSetBuffering handle NoBuffering
	return handle

stdoutServer :: Chanin OutMsg -> CHP ()
stdoutServer stdoutRecv = forever $ do
	(OutMsg id chunk) <- readChannel stdoutRecv
	liftIO_CHP $ do
		let length = fromIntegral $ LZ.length chunk
		LZ.putStr $ encode id
		LZ.putStr $ encode (length :: Word16)
		LZ.putStr chunk

reverseManager :: PortID -> Chanin ConnMsg -> Shared Chanout OutMsg -> CHP ()
reverseManager port connRecv stdoutSend = manager Map.empty
	where
	manager handles = do
		(WriteMsg id chunk) <- readChannel connRecv
		-- Zero length chunk means connection closed
		if LZ.length chunk < 1 then close id handles else
			case Map.lookup id handles of
				Just handle -> do
					-- TODO: If this fails, remove from handles
					liftIO_CHP $ LZ.hPutStr stderr chunk
					r <- liftIO_CHP $ try $ LZ.hPutStr handle chunk
					case r of
						Right () -> manager handles
						Left (SomeException _) -> do
							-- Tell other process connection has closed
							claim stdoutSend (`writeChannel` OutMsg id LZ.empty)
							manager (Map.delete id handles)
				Nothing -> do -- New connection
					handle <- newHandle
					liftIO_CHP $ LZ.hPutStr handle chunk
					handleConnection id handle stdoutSend <|*|>
						manager (Map.insert id handle handles)
	newHandle = liftIO_CHP $ do
		handle <- connectTo "localhost" port
		configureHandle handle
	close id handles = do
		case Map.lookup id handles of
			Just handle -> liftIO_CHP $ hClose handle
			Nothing -> return ()
		manager (Map.delete id handles)

listenManager :: Chanin ConnMsg -> CHP ()
listenManager connRecv = manager Map.empty
	where
	manager handles = do
		value <- readChannel connRecv
		case value of
			RegMsg id handle -> manager $ Map.insert id handle handles
			URegMsg id -> manager $ Map.delete id handles
			WriteMsg id chunk ->
				case Map.lookup id handles of
					Just handle ->
						-- Zero length chunk means connection closed
						if LZ.length chunk < 1 then do
							liftIO_CHP $ hClose handle
							manager (Map.delete id handles)
						else do
							liftIO_CHP $ LZ.hPutStr handle chunk
							manager handles
					Nothing -> manager handles -- Maybe error msg?

stdinServer :: Shared Chanout ConnMsg -> CHP ()
stdinServer connSend = forever $ do
	chunk <- liftIO_CHP $ LZ.hGet stdin 6
	-- Got 6 bytes: connection tag and length, now get data
	let (id, len) = LZ.splitAt 4 chunk
	let length = fromIntegral (decode len :: Word16)
	body <- liftIO_CHP $ LZ.hGet stdin length
	claim connSend (`writeChannel` WriteMsg (decode id) body)

-- TODO: Replace incrementing IDs with ID server
listen :: Socket -> Shared Chanout ConnMsg -> Shared Chanout OutMsg -> CHP ()
listen sock connSend stdoutSend = listen' 0
	where
	listen' next = do
		handle <- doAccept
		listen' (next + 1) <|*|> do
			-- Tell listenManager about this new connection
			first <- embedCHP $
				claim connSend (`writeChannel` RegMsg next handle)
			-- Handle new connection
			connection <- embedCHP $ handleConnection next handle stdoutSend
			final <- embedCHP $ do
				-- Tell other process connection has closed
				claim stdoutSend (`writeChannel` OutMsg next LZ.empty)
				-- Tell listenManager we are closing up
				claim connSend (`writeChannel` URegMsg next)
			-- Now that we've set it up, run it safely
			liftIO_CHP $ bracket_ first final connection
	doAccept = liftIO_CHP $ do
		(handle,_,_) <- accept sock
		configureHandle handle

handleConnection :: ConnID -> Handle -> Shared Chanout OutMsg -> CHP ()
handleConnection id handle stdoutSend = forever $ do
	chunk <- liftIO_CHP $ LZ.hGetNonBlocking handle 2048
	when (LZ.length chunk > 0) $
		claim stdoutSend (`writeChannel` OutMsg id chunk)
