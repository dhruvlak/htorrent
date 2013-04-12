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
	clientSock <- doIO (socket AF_INET Stream defaultProtocol)
	
	interpretResult <- doIO $ interpret s
	if (interpretResult == 0) then
		doIO $ putStrLn "Unknown input"
	else
		if (interpretResult ==1 || interpretResult == 2 || interpretResult ==3)	then 
			do
				env <- get
				x <- doIO (connectToTracker interpretResult env clientSock)
				put x
		else
			doIO $ putStrLn "we are working on this option"
	
	newEnv <- get
	doIO $ print ("current state:"++ (show newEnv))
	mainLoop

interpret :: String -> IO Int
interpret s = if s == "connect" then
			return 1
		else
		 	if s == "seed" then
				return 2 
			
			else
			  do
				putStrLn "Wrong Input" 
				return 0
	
connectToTracker::Int ->Env-> Socket ->IO Env
connectToTracker requestType env clientSock= 
		--putStrLn s 
		if (requestType == 1) then
		 do
			trackerAddr <- inet_addr tracker_ip
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			getEnv clientSock ""
			
		else if( requestType ==2) then
		 do
			trackerAddr <- inet_addr tracker_ip
			putStrLn "enter file name:"
			fileName <- getLine
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			updateTrackerEnv clientSock fileName
			return env
			
		      else
				return env
			
getEnv :: Socket -> String -> IO Env
getEnv clientSock msgReceived =do
				trackerAddr <- inet_addr tracker_ip
 			        sendTo clientSock "$getEnv$" (SockAddrInet tracker_port trackerAddr) 
				getEnvLoop clientSock msgReceived			
	
getEnvLoop :: Socket -> String -> IO Env
getEnvLoop clientSock msgReceived = do
					--trackerAddr <- inet_addr tracker_ip
					--sendTo clientSock "$getEnv$" (SockAddrInet tracker_port trackerAddr) 
					m <- (recv clientSock 100)
					print m
					if ( last m == '$' ) then
						do
							sClose clientSock
							return (read (msgReceived ++ (filter (\x -> x/='$') m)))
					else getEnvLoop clientSock (msgReceived++ m) 

updateTrackerEnv clientSock fileName = do	
					trackerAddr <- inet_addr tracker_ip
					sendTo clientSock fileName (SockAddrInet tracker_port trackerAddr) 
					
