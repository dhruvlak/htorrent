import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

import Data.Map as M

--type Env = [String]
type Env a = StateT [String] IO a 

doIO :: IO a -> Env a
doIO = lift

runEnv :: Env a-> IO a
runEnv env = evalStateT env ["asdh"]

getIpEth0 (n:ns) = if ((name n) == "eh0")
                          then show (ipv4 n)
                          else getIpEth0 ns

getIpEth0 [] = "127.0.0.1"

ipEth :: IO HostAddress
ipEth = do
	ni <- getNetworkInterfaces
	inet_addr(getIpEth0 ni)


listen_port = 10116
message = "hello, world"


main = do 
	sock  <- socket AF_INET Stream defaultProtocol
	haddr <- ipEth
	bindSocket sock (SockAddrInet listen_port haddr)
	acceptLoop sock `finally` sClose sock

	--putStr haddr

acceptLoop :: Socket -> IO ()
acceptLoop sock = forever (listen sock 1 >> (accept sock >>= forkIO . worker))

worker :: (Socket,SockAddr) -> IO ()
worker (sock, (SockAddrInet pn ha) ) = 
	do
		ha_1 <- inet_ntoa ha
		runEnv (updateEnv ha_1)		

updateEnv::String->Env ()
updateEnv host = modify (addClient host)

addClient::String -> [String] -> [String]
addClient host s = host:s
addClinet host [] = [host]  
	


