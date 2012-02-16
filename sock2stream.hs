module Main where

import Network
import System (getArgs)
import System.IO
import System.Directory (removeFile)
import System.Console.GetOpt
import Control.Exception (try, bracket, SomeException(..))
import Control.Monad (forever,when,liftM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
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

(<|*|>) :: IO a -> IO b -> IO ()
a <|*|> b = do
	_ <- forkIO (a >> return ())
	_ <- b
	return ()

start :: Flag -> IO ()
start (Listen path) = withSocketsDo $ bracket
	(listenOn (UnixSocket path))
	(\sock -> do
		sClose sock
		removeFile path)
	(\sock -> do
		connQ <- newChan
		stdoutQ <- newChan
		listenManager connQ <|*|> stdinServer connQ <|*|>
			stdoutServer stdoutQ <|*|> listen sock connQ stdoutQ)
start (RListen path) = withSocketsDo $ do
	conn <- newChan
	stdout <- newChan
	reverseManager (UnixSocket path) conn stdout <|*|>
		stdinServer conn <|*|> stdoutServer stdout

configureHandle :: Handle -> IO Handle
configureHandle handle = do
	hSetBinaryMode handle True
	hSetBuffering handle NoBuffering
	return handle

stdoutServer :: Chan OutMsg -> IO ()
stdoutServer stdoutQ = forever $ do
	(OutMsg id chunk) <- readChan stdoutQ
	let length = fromIntegral $ LZ.length chunk
	LZ.putStr $ encode id
	LZ.putStr $ encode (length :: Word16)
	LZ.putStr chunk

reverseManager :: PortID -> Chan ConnMsg -> Chan OutMsg -> IO ()
reverseManager port connQ stdoutQ = manager Map.empty
	where
	manager handles = do
		(WriteMsg id chunk) <- readChan connQ
		-- Zero length chunk means connection closed
		if LZ.length chunk < 1 then close id handles else
			case Map.lookup id handles of
				Just handle -> do
					-- TODO: If this fails, remove from handles
					r <- try $ LZ.hPutStr handle chunk
					case r of
						Right () -> manager handles
						Left (SomeException _) -> do
							-- Tell other process connection has closed
							writeChan stdoutQ (OutMsg id LZ.empty)
							manager (Map.delete id handles)
				Nothing -> do -- New connection
					handle <- newHandle
					LZ.hPutStr handle chunk
					handleConnection id handle stdoutQ <|*|>
						manager (Map.insert id handle handles)
	newHandle = do
		handle <- connectTo "localhost" port
		configureHandle handle
	close id handles = do
		case Map.lookup id handles of
			Just handle -> hClose handle
			Nothing -> return ()
		manager (Map.delete id handles)

listenManager :: Chan ConnMsg -> IO ()
listenManager connQ = manager Map.empty
	where
	manager handles = do
		value <- readChan connQ
		case value of
			RegMsg id handle -> manager $ Map.insert id handle handles
			URegMsg id -> manager $ Map.delete id handles
			WriteMsg id chunk ->
				case Map.lookup id handles of
					Just handle ->
						-- Zero length chunk means connection closed
						if LZ.length chunk < 1 then do
							hClose handle
							manager (Map.delete id handles)
						else do
							LZ.hPutStr handle chunk
							manager handles
					Nothing -> manager handles -- Maybe error msg?

stdinServer :: Chan ConnMsg -> IO ()
stdinServer connQ = forever $ do
	chunk <- LZ.hGet stdin 6
	-- Got 6 bytes: connection tag and length, now get data
	let (id, len) = LZ.splitAt 4 chunk
	let length = fromIntegral (decode len :: Word16)
	body <- LZ.hGet stdin length
	writeChan connQ (WriteMsg (decode id) body)

-- TODO: Replace incrementing IDs with ID server
listen :: Socket -> Chan ConnMsg -> Chan OutMsg -> IO ()
listen sock connQ stdoutQ = listen' 0
	where
	listen' next = do
		(handle,_,_) <- accept sock
		_ <- configureHandle handle
		listen' (next + 1) <|*|> do
			-- Tell listenManager about this new connection
			writeChan connQ (RegMsg next handle)
			-- Handle new connection
			r <- try $ handleConnection next handle stdoutQ
			case r of
				Right () -> return ()
				Left (SomeException _) -> do
					-- Tell other process connection has closed
					writeChan stdoutQ (OutMsg next LZ.empty)
					-- Tell listenManager we are closing up
					writeChan connQ (URegMsg next)

handleConnection :: ConnID -> Handle -> Chan OutMsg -> IO ()
handleConnection id handle stdoutQ = forever $ do
	chunk <- LZ.hGetNonBlocking handle 2048
	when (LZ.length chunk > 0) $
		writeChan stdoutQ (OutMsg id chunk)
