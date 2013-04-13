import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IOi
import Control.Monad.State
import Control.OldException


data Env = Env {
		files :: [FileInfo]
		,seedingList :: [String]
		,dowloadList :: [(String,String)]
	} deriving (Show,Read)


data FileInfo = FileInfo {
	Seeders :: [String]
	,fName :: String
}deriving (Show,Read)


type ClientM a = StateT Env IO a

emptyEnv = Env { files=[], seedingList = [] , dowloadList =[] }  
tracker_ip = "127.0.0.1"
tracker_port = 10116

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	evalStateT mainLoop emptyEnv

doIO :: IO a -> ClientM a
doIO = lift

mainLoop :: ClientM ()
mainLoop = do  
	mvarSeedingList <- newEmptyMVar
	s <- doIO  (do 
			putStr "htorrent>"
			getLine
	       )
	env1 <- get
	doIO {forkIO (seedingWorker mvarSeedingList (seedingList env1))}
	clientSock <- doIO (socket AF_INET Stream defaultProtocol)
	
	interpretResult <- doIO $ interpret s
	if (interpretResult == 0) then
		doIO $ putStrLn "Unknown input"
	else
		if (interpretResult ==1 || interpretResult == 2)	then 
			do
				env <- get
				x <- doIO (connectToTracker interpretResult env clientSock mvarSeedingList)
				put x
		else if (interpretResult ==3 )
			downloadComputation env
		
		else	
		doIO $ putStrLn "we are working on this option"
	
	newEnv <- get
	doIO $ print ("current state:"++ (show newEnv))
	mainLoop


downloadComputation :: ClientM()
downloadComputation env =
		 fileName <- doIO ( do
				putStr "enter file:" 
				getLine
				)
		NewfileName <- doIO (do
				putStr "enter New File:"
				getLine
				)
		 if (chechFileInEnv fileName env) then			 	
			downloadThreadId <-doIO (dowloadWorker fileName (files env))
			put Env { files =files env, seedingList = seedingList env, downloadList = ((fileName,downloadThreadId):(downloadList env)) }
		 else	
			downloadComputation env		

checkFIleInEnv :: String -> [FileInfo] -> Bool
checkFileInEnv fileName  (x:xs) = if (fName x) == filename then
					True
				  else checkFileInEnv fileName xs
checkFileInEnv fileName xs = False

downloadWorker :: String -> [FileInfo] -> IO ()
downloadWorker fileName fileInfoList  = do 
					seedersList <- fName_seeders fileName fileInfoList
					if ( seedersList == [] ) then
						print "no Seeder"
					else
					
					canDownload <- tryConnectToSeederList fileName seedersList
					if canDownload then
						print "downloaded : " ++ fileName
					else
						print "cannot connect to any seeder" 

fName_seeders :: String -> [FileInfo] -> [String]
fName_seeders fileName (x:xs) = if (fileName == (fName x)) then
					seeders x
				else
					fName_seeders fileName xs
fName_seeder fileName [] = []

tryConnectToSeederList :: String -> [String] -> IO Bool
tryConnectToSeederList fileName (x:xs) = do
					 downloadSock <- doIO (socket AF_INET Stream defaultProtocol)
					 connectedOrNot <- try ( connect downloadSock (Sock) )
					 case connectedOrNot of
					 Right a -> do
							send downloadSock fileName
						    	s<-recv downloadSock 100
							if (s == "yes")
								do
									putStrLn "s:yes"
									return True
							else
								do
									putStrLn "s:no"
									return True

					Left a -> tryConnectToSeederList fileName xs
tryConnectToSeederList fileName [] = return False
  
	

getIpEth0 (n:ns) = if ((name n) == "en0")
                          then show (ipv4 n)
                          else getIpEth0 ns

getIpEth0 [] = "127.0.0.1"

ipEth :: IO HostAddress
ipEth = do
	ni <- getNetworkInterfaces
	putStrLn (getIpEth0 ni)
	inet_addr (getIpEth0 ni)
	

seedingWorker :: MVar [String]->[String] -> IO ()
seedingWorker mvarSeedingList = do 
					dowloadInProgress <- newEmptyMVar
					seedingSock <- socket AF_INET Stream defaultProtocol
					seederAddr <- ipEth
					bindSocket seedingSock (SockAddrInet seedingPort seederAddr)
					seedingAccepThreadId <- (forkIO (acceptSeeding seedingSock seedingFileList downloadInProgress))
					seedingListFromMVar <- takeMVar mvarSeedingList
					takeMVar dowloadInProgress
					killThread seedingAcceptThreadId
					seedingWorker mvarSeedingList seedingListFromMVar	

acceptSeeding :: Socket -> [String] -> MVar Bool ->IO ()
acceptSeeding seedingSock seedingFileList dInProgress= listen seedingSock 1
				acceptedSock <- accept seedingSock
				fileNameFromCli <- (recv acceptedSock 100)
				if (elem fileNameFromCli seedingFileList ) then
					send "yes" acceptedSock				

					putMVar True dInProgress
				else 
					send "No" acceptedSock
				sClose seedingSock

interpret :: String -> IO Int
interpret s = if s == "connect" then
			return 1
		else
		 	if s == "seed" then
				return 2 
			
			else if s=="download" then
				return 3
			  
				else if s=="refresh" then
					return 1
				else
			   	
					do
						putStrLn "Wrong Input" 
						return 0
	
connectToTracker::Int ->Env-> Socket -> MVar [String]->IO Env
connectToTracker requestType env clientSock mvarSeedingList= 
		--putStrLn s 
		if (requestType == 1) then
		 do
			trackerAddr <- inet_addr tracker_ip
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			getEnv env clientSock ""
			
		else if( requestType ==2) then
		 do
			trackerAddr <- inet_addr tracker_ip
			putStr "enter file name:"
			fileName <- getLine
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			updateTrackerEnv clientSock fileName 
			putMVar (addUnique fileName (seedingList env)) mvarSeedingList
			return Env {files =files env, seedingList = (addUnique fileName (seedingList env)), downloadList = (downloadList env)}
			
		      else
				return env
			
getEnv :: Env -> Socket -> String -> IO Env
getEnv env clientSock msgReceived =do
				trackerAddr <- inet_addr tracker_ip
 			        sendTo clientSock "$getEnv$" (SockAddrInet tracker_port trackerAddr) 
				getEnvLoop  env clientSock msgReceived			
	
getEnvLoop :: Env -> Socket -> String -> IO Env
getEnvLoop env clientSock msgReceived = do
					--trackerAddr <- inet_addr tracker_ip
					--sendTo clientSock "$getEnv$" (SockAddrInet tracker_port trackerAddr) 
					m <- (recv clientSock 100)
					print m
					if ( last m == '$' ) then
						do
							sClose clientSock
							return Env{files = (read (msgReceived ++ (filter (\x -> x/='$') m)))
								  , dowloadList = (dowloadList env)
								  , seedingList =(seedingList env)
								  }

					else getEnvLoop env clientSock (msgReceived++ m) 

updateTrackerEnv clientSock fileName = do	
					trackerAddr <- inet_addr tracker_ip
					sendTo clientSock fileName (SockAddrInet tracker_port trackerAddr) 
					

addUnique :: String ->[String] -> [String]
addUnique s (x:xs) = if (x==s) then (x:xs)
                        else
                                 (x:(addUnique s xs))
addUnique s [] = [s]
 

