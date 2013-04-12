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


getIpEth0 (n:ns) = if ((name n) == "en0")
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
			mvarEnv <- newEmptyMVar
			maintainEnvTid <- (forkIO (maintainEnv fromCli mvarEnv))
			--checkPeersTid <- (forkIO (checkPeers updatedEnv))
			listen sock 2
			forever (do
					accepted_sock <- accept sock 
					forkIO (worker accepted_sock fromCli mvarEnv)
				)

worker :: (Socket,SockAddr) -> MVar String ->MVar Env -> IO ()
worker (sock, (SockAddrInet pn ha) ) m u = 
	do
		ha_1 <- inet_ntoa ha
		print ha_1
		--sClose sock
		tryTakeMVar u
		putMVar m (ha_1 ++ ":" ++ show(pn))
		--takeMVar u
		--tryTakeMVar u
		newEnv <- takeMVar u
		sendNum <- send sock (show (newEnv))
		send sock "$#finish#$"
		sClose sock
		print ("send:"++ show (sendNum))
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
updateList a mvarEnv = do 
		x <- get
		put (modifiedEnv x)
		doIO (print (modifiedEnv x))
		doIO ( forkIO (do  v <- tryTakeMVar mvarEnv
				   putMVar mvarEnv (modifiedEnv x)))
		return ()
		
		where modifiedEnv s = (Env {connectedPeers = (a:(connectedPeers s)), files = (files s)} )


--ListM a = StateT [String] IO a	

runListM :: ListM a -> IO a
runListM l = evalStateT l emptyEnv

emptyEnv = Env {connectedPeers = [], files = ("Hey",["12","23"])}

doIO :: IO a-> ListM a
doIO = lift 

--printList :: ListM a -> IO (		)

