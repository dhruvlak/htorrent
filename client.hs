import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

data Env = Env {
		connectedPeers :: [String]
		,files :: [(String,[String])]

	} deriving (Show,Read)

type ClientM a = StateT Env IO a

emptyEnv = Env {connectedPeers = [], files=[] }  

tracker_ip = "127.0.0.1"
tracker_port = 10116

main :: IO ()
main = evalStateT mainLoop emptyEnv

doIO :: IO a -> ClientM a
doIO = lift

mainLoop :: ClientM ()
mainLoop = do  
	doIO $ putStrLn "htorrent :"
	s <- doIO getLine
	interpretResult <- doIO $ interpret s
	if (interpretResult == 1) then
		do
			x <- doIO $ connectToTracker
			put x
	else
		doIO $ putStrLn "Unknown input"
	currentState <- get
	doIO $ print ("current state:"++ (show currentState))
	mainLoop

interpret :: String -> IO Int
interpret s = if s == "connect" then
			return 1
		else
		do
			putStrLn "Wrong Input" 
			return 0
	
connectToTracker::IO Env
connectToTracker = do
		--putStrLn s 
		trackerAddr <- inet_addr tracker_ip
		client_sock <- socket AF_INET Stream defaultProtocol
		connect client_sock (SockAddrInet tracker_port trackerAddr)
		getEnv client_sock ""

getEnv :: Socket -> String -> IO Env
getEnv clientSock msgReceived = do
					m <- (recv clientSock 100)
					print m
					if ( last m == '$' ) then
						return (read (msgReceived ++ (filter (\x -> x/='$') m)))
					else getEnv clientSock (msgReceived++ m) 
					
