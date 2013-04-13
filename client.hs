import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
--import Network
import System.IO
import Control.Monad.State
import Control.OldException


data Env = Env {
		files :: [FileInfo]
		,seedingList :: [String]
		,downloadList :: [(String,ThreadId)]
	} deriving (Show)


data FileInfo = FileInfo {
	seeders :: [String]
	,fName :: String
}deriving (Show,Read)


type ClientM a = StateT Env IO a

emptyEnv = Env { files=[], seedingList = [] , downloadList =[] }  
tracker_ip = "127.0.0.1"
tracker_port = 10116

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	evalStateT (inputLoop True) emptyEnv

doIO :: IO a -> ClientM a
doIO = lift
 

inputLoop :: Bool -> ClientM ()
inputLoop seedingOrNot = do  
	mvarSeedingList <- doIO newEmptyMVar
	env1 <- get
			
	
	if (seedingOrNot) then
		do
			doIO (print "i am seeding")
			doIO (forkIO (seedingWorker mvarSeedingList (seedingList env1)))
			--doIO (print "i am seeding")
			return ()
	else
		return ()

	s <- doIO  (do 
			putStr "htorrent>"
			getLine
	       )

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
		else if (interpretResult ==3 ) then
			do
				env <- get
				downloadComputation env
		
		else	
			doIO $ putStrLn "we are working on this option"
	
	newEnv <- get
	doIO $ print ("current state:"++ (show newEnv))
	inputLoop False


downloadComputation :: Env -> ClientM ()
downloadComputation env = do
		 fileName <- doIO ( do
				putStr "enter file:" 
				getLine
				)
		 newfileName <- doIO (do
				putStr "enter New File:"
				getLine
				)
		 if (checkFileInEnv fileName (files env)) then			 	
			do
			downloadThreadId <-doIO (forkIO (downloadWorker fileName (files env)))
			put Env { files =files env, seedingList = seedingList env, downloadList = ((fileName,downloadThreadId):(downloadList env)) }
		 else	
			downloadComputation env		

checkFileInEnv :: String -> [FileInfo] -> Bool
checkFileInEnv fileName  (x:xs) = if (fName x) == fileName then
					True
				  else checkFileInEnv fileName xs
checkFileInEnv fileName xs = False

downloadWorker :: String -> [FileInfo] -> IO ()
downloadWorker fileName fileInfoList  = do 
					seedersList <- (fName_seeders fileName fileInfoList)
					if (seedersList == []) then
						print "no Seeder"
					else
						do	
						canDownload <- tryConnectToSeederList fileName seedersList
						if canDownload then
							print ("downloaded : " ++ fileName)
						else
							print "cannot connect to any seeder" 

fName_seeders :: String -> [FileInfo] -> IO [String]
fName_seeders fileName (x:xs) = if (fileName == (fName x)) then
					return (seeders x)
				else
					fName_seeders fileName xs
fName_seeder fileName [] = return []

tryConnectToSeederList :: String -> [String] -> IO Bool
tryConnectToSeederList fileName (x:xs) = do
					 downloadSock <- (socket AF_INET Stream defaultProtocol)
					 seederAddr <- inet_addr x
					 connectedOrNot <- Control.OldException.try ( connect downloadSock (SockAddrInet tracker_port seederAddr ))
					 case connectedOrNot of
					 	Right a -> do
							send downloadSock fileName
						    	s<-recv downloadSock 100
							if (s == "yes") then
								do
									putStrLn "s:yes"
									return True
							else
								do
									putStrLn "s:no"
									return True

						Left a -> tryConnectToSeederList fileName xs
tryConnectToSeederList fileName [] = return False
  
	

getIpEth0 (n:ns) = if ((name n) == "e0")
                          then show (ipv4 n)
                          else getIpEth0 ns

getIpEth0 [] = "127.0.0.1"

ipEth :: IO HostAddress
ipEth = do
	ni <- getNetworkInterfaces
	--putStrLn (getIpEth0 ni)
	inet_addr (getIpEth0 ni)
	

seedingPort = 1729
seedingWorker :: MVar [String]->[String] -> IO ()
seedingWorker mvarSeedingList seedingFileList=do 
				seedingSock <- socket AF_INET Stream defaultProtocol
				seederAddr <- ipEth
				bindSocket seedingSock (SockAddrInet seedingPort seederAddr)
				seedingWorkerLoop seedingSock mvarSeedingList seedingFileList

seedingWorkerLoop :: Socket -> MVar [String] -> [String]->IO ()
seedingWorkerLoop seedingSock mvarSeedingList seedingFileList = do 
					downloadInProgress <- newEmptyMVar
					--seedingListFromMVar <- takeMVar mvarSeedingList
					
					seedingAcceptThreadId <- (forkIO (acceptSeeding seedingSock seedingFileList downloadInProgress))
					seedingListFromMVar <- takeMVar mvarSeedingList
					--takeMVar downloadInProgress
					killThread seedingAcceptThreadId
					--sClose seedingSock
					seedingWorkerLoop seedingSock mvarSeedingList seedingListFromMVar	
					

acceptSeeding :: Socket -> [String] -> MVar Bool ->IO ()
acceptSeeding seedingSock seedingFileList dInProgress = do 
				listen seedingSock 1
				acceptedSock <- accept seedingSock
				fileNameFromCli <- (recv (fst acceptedSock) 100)
				if (elem fileNameFromCli seedingFileList ) then
				   do	
					send (fst acceptedSock) "yes"				
					--putMVar dInProgress True
					return ()
				else 
				   do
					send (fst acceptedSock) "no"
					return ()

				sClose seedingSock
				acceptSeeding seedingSock seedingFileList dInProgress

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
			env2 <- getEnv env clientSock ""
			sClose clientSock
			return env2

		else if( requestType ==2) then
		 do
			trackerAddr <- inet_addr tracker_ip
			putStr "enter file name:"
			fileName <- getLine
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			updateTrackerEnv clientSock fileName 
			putMVar mvarSeedingList (addUnique fileName (seedingList env)) 
			sClose clientSock
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
								  , downloadList = (downloadList env)
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
 

