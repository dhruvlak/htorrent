import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

import Data.Map as M

type ListM a = StateT Env IO a

data Env = Env {
		connectedPeers :: [String]
		,files :: (String,[String])
		} deriving (Show)


getIpEth0 (n:ns) = if ((name n) == "wlan0")
                          then show (ipv4 n)
                          else getIpEth0 ns

getIpEth0 [] = "127.0.0.1"

ipEth :: IO HostAddress
ipEth = do
	ni <- getNetworkInterfaces
	putStrLn (getIpEth0 ni)
	inet_addr (getIpEth0 ni)
	

listen_port = 10116
message = "hello, world"

main = do 
	sock  <- socket AF_INET Stream defaultProtocol
	haddr <- ipEth
	bindSocket sock (SockAddrInet listen_port haddr)
	acceptLoop sock `finally` sClose sock

	--putStr haddr

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
			fromCli <- newEmptyMVar
			updatedEnv <- newEmptyMVar
			maintainEnvTid <- (forkIO (maintainEnv fromCli updatedEnv))
			--checkPeersTid <- (forkIO (checkPeers updatedEnv))
			listen sock 2
			forever (do
					accepted_sock <- accept sock 
					forkIO (worker accepted_sock fromCli)
				)

worker :: (Socket,SockAddr) -> MVar String -> IO ()
worker (sock, (SockAddrInet pn ha) ) m = 
	do
		ha_1 <- inet_ntoa ha
		--runStateT (updateEnv ha_1) >>= putStr . fst
		--runEnv (updateEnv ha_1)		
		--hand <- socketToHandle sock ReadWriteMode
		--hSetBuffering hand LineBuffering
		sClose sock
		putMVar m (ha_1 ++ ":" ++ show(pn))


------------------------------------------------------------------------


------------------------------------------------------------------------

maintainEnv :: MVar String -> MVar Env ->IO ()
maintainEnv m u = runListM (updateListLoop m u)

updateListLoop :: MVar String -> MVar Env -> ListM ()
updateListLoop m u = do 
			s <- doIO (takeMVar m)
			x <- (updateList s u)
			updateListLoop m u
			

updateList :: String ->MVar Env-> ListM ()
updateList a mvarUpdatedEnv = do 
		x <- get
		put (updatedEnv x)
		doIO (print (updatedEnv x))
		doIO ( forkIO (do  v <- tryTakeMVar mvarUpdatedEnv
				   putMVar mvarUpdatedEnv (updatedEnv x)))
		return ()
		
		where updatedEnv s = (Env {connectedPeers = (a:(connectedPeers s)), files = (files s)} )


--ListM a = StateT [String] IO a	

runListM :: ListM a -> IO a
runListM l = evalStateT l emptyEnv

emptyEnv = Env {connectedPeers = [], files = ("Hey",["12","23"])}

doIO :: IO a-> ListM a
doIO = lift 

--printList :: ListM a -> IO ()

