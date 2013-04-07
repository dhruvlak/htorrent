import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
import Network
import System.IO

--putStr ( show (checkinterface ni ) )

getIpEth0 (n:ns) = if ((name n) == "eth0")
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
acceptLoop sock = forever $ accept sock >>= forkIO . worker

worker :: (Handle, HostName, PortNumber) -> IO ()
worker (hand, host, port ) = do
		

	


