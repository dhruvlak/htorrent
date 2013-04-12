import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State

import Data.Map as M

type TrackerM a = StateT Env IO a

data Env = Env {
		connectedPeers :: [String]
		,files :: [(String,[String])]
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
	

listenPort = 10116

message = "hello, world"

main = do 
	sock  <- socket AF_INET Stream defaultProtocol
	haddr <- ipEth
	bindSocket sock (SockAddrInet listenPort haddr)
	acceptLoop sock `finally` sClose sock

	--putStr haddr

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
			mvarFile <- newEmptyMVar
			mvarSeeder <- newEmptyMVar
			mvarEnv <- newEmptyMVar
			maintainEnvTid <- (forkIO (maintainEnv mvarFile mvarSeeder mvarEnv))
			--checkPeersTid <- (forkIO (checkPeers updatedEnv))
			listen sock 2
			forever (do
					accepted_sock <- accept sock 
					forkIO (worker sock accepted_sock mvarFile mvarSeeder mvarEnv)
				)

worker :: Socket -> (Socket,SockAddr) -> MVar String->MVar String ->MVar Env -> IO ()
worker serverSock (clientSock, (SockAddrInet pn ha) ) mvarFile mvarSeeder mvarEnv = 
	do
		clientIP <- inet_ntoa ha
		--print ha_1
		requestType <- (recv clientSock 100)
		--sClose sock
		if (requestType == "$getEnv$") then
		   do	
			tryTakeMVar mvarEnv
			putMVar mvarFile ("")
			putMVar mvarSeeder ("")
		--takeMVar u
		--tryTakeMVar u
			newEnv <- takeMVar mvarEnv
			sendNum <- send clientSock ((show (newEnv)) ++ "$")
		--send sock "$#finish#$"
			sClose clientSock
			print ("send:"++ show (sendNum))
		else 
		  do
			tryTakeMVar mvarEnv
			putMVar mvarFile requestType
			putMVar mvarSeeder clientIP
			print ("updating:")
			sClose clientSock

------------------------------------------------------------------------



------------------------------------------------------------------------

maintainEnv :: MVar String -> MVar String-> MVar Env ->IO ()
maintainEnv mvarFile mvarSeeder mvarEnv = runTrackerM (updateListLoop mvarFile mvarSeeder mvarEnv)

updateListLoop :: MVar String-> MVar String -> MVar Env -> TrackerM ()
updateListLoop mvarFile mvarSeeder mvarEnv = do 
			fileName <- doIO (takeMVar mvarFile)
			seeder <- doIO (takeMVar mvarSeeder)
			x <- (updateList fileName seeder mvarEnv)
			updateListLoop mvarFile mvarSeeder mvarEnv
			

updateList :: String -> String ->MVar Env-> TrackerM ()
updateList fileName seeder mvarEnv = do 
		env <- get
		put (modifiedEnv env fileName seeder)
		doIO (print (modifiedEnv env fileName seeder))
		doIO ( forkIO (do  v <- tryTakeMVar mvarEnv
				   putMVar mvarEnv (modifiedEnv env fileName seeder)
			      )
		     )
		return ()
		
		where modifiedEnv env fileName seeder =  Env {connectedPeers = (connectedPeers env), files = (modifyFileList (files env) fileName seeder)} 


modifyFileList::[(String,[String])] -> String -> String -> [(String,[String])]
modifyFileList (x:xs) fileName seeder = if (fst x) == fileName then
						((fileName,(addUnique seeder (snd x))):xs)
					else (x:(modifyFileList xs fileName seeder))
modifyFileList [] fileName seeder = [(fileName,[seeder])]
--ListM a = StateT [String] IO a	

addUnique :: String ->[String] -> [String]
addUnique s (x:xs) = if (x==s) then (x:xs)
			else
				(x:(addUnique s xs))
addUnique s [] = [s]


runTrackerM :: TrackerM a -> IO a
runTrackerM l = evalStateT l emptyEnv

emptyEnv = Env {connectedPeers = [], files = [("Hey",["12","23"])]}

doIO :: IO a-> TrackerM a
doIO = lift 

--printList :: ListM a -> IO (		)

