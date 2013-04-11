
import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

import Data.Map as M

--type Env = [String]

getIpEth0 (n:ns) = if ((name n) == "eth0")
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
			updatedList <- newEmptyMVar
			maintainEnvTid <- (forkIO (maintainEnv fromCli updatedList))
			listen sock 2
			forever (do
					accepted_sock <- accept sock 
					forkIO (worker accepted_sock fromCli updatedList)
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
		putMVar m ha_1 

maintainEnv :: MVar String ->IO ()
maintainEnv m u = runListM (updateListLoop m u)

updateListLoop :: MVar String -> MVar String -> ListM ()
updateListLoop m u = do 
			v <- doIO (takeMVar m)
			updateList v
			maintainEnv m u

updateList :: CalcM ()	
updateList = StateT  


ListM a = StateT [String] IO a	

runListM :: ListM a -> IO a
runListM l = evalStateT l []

doIO :: IO a-> ListM a
doIO = lift


