import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

data Env = Env {
		connectedPeers :: [String]
		,files :: (String,[String])

	} deriving (Show)

tracker_ip = "127.0.0.1"
tracker_port = 10116


main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do  
	putStr "htorrent>"
	s <- getLine
	interpret s

interpret :: String -> IO ()
interpret s = if s == "connect" then
			connectToTracker
		else
			putStrLn "wrong input" 
		
	
connectToTracker::IO ()
connectToTracker = do
		--putStrLn s 
		trackerAddr <- inet_addr tracker_ip
		client_sock <- socket AF_INET Stream defaultProtocol
		connect client_sock (SockAddrInet tracker_port trackerAddr)
		getEnv client_sock "Env:"

getEnv :: Socket -> String -> IO ()
getEnv clientSock msgReceived = do
					m <- (recv clientSock 1024)
					print m
					if (m == "$#finish#$") then
						print msgReceived
					else getEnv clientSock (msgReceived++ m) 
					
