import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception
import System.Environment
import System.IO
import Control.Monad.State
import Control.OldException
import Data.Maybe
import System.Exit

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
 	hSetBuffering stdout (BlockBuffering (Just 4))
	evalStateT (inputLoop True) emptyEnv

doIO :: IO a -> ClientM a
doIO = lift
 

inputLoop :: Bool -> ClientM ()
inputLoop seedingOrNot = do  
	mvarSeedingList <- doIO newEmptyMVar
	env1 <- get
	seedingSock <-doIO( socket AF_INET Stream defaultProtocol)
	
	if (seedingOrNot) then
		do
			--doIO (print "i am seeding")
			doIO (forkIO (seedingWorker mvarSeedingList (seedingList env1) seedingSock))
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
		if (interpretResult ==1 || interpretResult == 2 )	then 
			do
				env <- get
				x <- doIO (connectToTracker interpretResult env clientSock mvarSeedingList) 
				put x
		else if (interpretResult == 4)then 
			do
				env <- get
                                x <- doIO (connectToTracker interpretResult env clientSock mvarSeedingList)
                                doIO (sClose clientSock)
				doIO (sClose seedingSock)
				doIO (exitSuccess)
						
		else if (interpretResult ==3 ) then
			do
				env <- get
				downloadComputation env
		
			else
				return ()
		
	newEnv <- get
	--doIO $ print ("current state:"++ (show newEnv))
	inputLoop False


downloadComputation :: Env -> ClientM ()
downloadComputation env = do
		 fileName <- doIO ( do
				putStr "enter file:" 
				getLine
				)
		 
		 newFileName <- doIO (do
				putStr "enter New File:"
				getLine
				)
		 checkNF <-doIO ( Control.OldException.try( openFile newFileName AppendMode))
		 case checkNF of
		 	Right a ->
			  
		 	  if (checkFileInEnv fileName (files env)) then			 	
				do
				
				downloadThreadId <-doIO (forkIO (downloadWorker a fileName (files env)))
				put Env { files =files env, seedingList = seedingList env, downloadList = ((fileName,downloadThreadId):(downloadList env)) }
		 	  else	
				do
				doIO (hClose a)
				doIO (print ("file not in Env" ++fileName))
				--doIO (putStr "htorrent>")

			Left a ->
				doIO ( putStrLn (show a))
				--return ()

checkFileInEnv :: String -> [FileInfo] -> Bool
checkFileInEnv fileName  (x:xs) = if (fName x) == fileName then
					True
				  else checkFileInEnv fileName xs
checkFileInEnv fileName xs = False

downloadWorker :: Handle -> String -> [FileInfo] -> IO ()
downloadWorker hand fileName fileInfoList  = do 
					seedersList <- (fName_seeders fileName fileInfoList)
					if (seedersList == []) then
						print "no Seeder"
					else
						do	
						canDownload <- (tryConnectToSeederList fileName seedersList hand)
						if canDownload then
							do 
							putStrLn ("downloaded : " ++ fileName)
							--putStr "htorrent>"
						else
							do
							print "cannot connect to any seeder"
							--putStr "htorrent>" 
					hClose hand

fName_seeders :: String -> [FileInfo] -> IO [String]
fName_seeders fileName (x:xs) = if (fileName == (fName x)) then
					return (seeders x)
				else
					fName_seeders fileName xs
fName_seeder fileName [] = return []



tryConnectToSeederList :: String -> [String] -> Handle  -> IO Bool
tryConnectToSeederList fileName (x:xs) hand = do
					 downloadSock <- (socket AF_INET Stream defaultProtocol)
					 seederAddr <- inet_addr x
					 connectedOrNot <- Control.OldException.try ( connect downloadSock (SockAddrInet seedingPort seederAddr ))
					 case connectedOrNot of
					 	Right a -> do
							--print "right"
							send downloadSock fileName
						    	s<-(recv downloadSock 3)
							print ("response : "++s)
							if (s == "yes") then
								do
									sockHandle <- (socketToHandle downloadSock ReadMode)
									recvAndWrite hand (sockHandle)
									return True
							else
								do
									--putStrLn "s:no"
									return False

						Left a -> tryConnectToSeederList fileName xs hand
tryConnectToSeederList fileName [] hand = return False
  
	
recvAndWrite :: Handle -> Handle -> IO ()
recvAndWrite hand handleSock =do
				s <- hGetContents handleSock
				hPutStr hand s
				return ()

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
seedingWorker :: MVar [String] -> [String] -> Socket -> IO ()
seedingWorker mvarSeedingList seedingFileList seedingSock =do 
				seederAddr <- ipEth
				bindSocket seedingSock (SockAddrInet seedingPort seederAddr)
				listen seedingSock 5
				seedingWorkerLoop seedingSock mvarSeedingList seedingFileList

seedingWorkerLoop :: Socket -> MVar [String] -> [String]->IO ()
seedingWorkerLoop seedingSock mvarSeedingList seedingFileList = do 
					
					downloadInProgress <- newEmptyMVar
					--seedingListFromMVar <- takeMVar mvarSeedingList
					
					seedingAcceptThreadId <- (forkIO (acceptSeeding seedingSock seedingFileList downloadInProgress))
					seedingListFromMVar <- takeMVar mvarSeedingList
					--takeMVar downloadInProgress
					--killThread seedingAcceptThreadId
					--sClose seedingSock
					seedingWorkerLoop seedingSock mvarSeedingList seedingListFromMVar `Control.Exception.finally` (sClose seedingSock) 	
					
bufferSize = 1024			
acceptSeeding :: Socket -> [String] -> MVar Bool ->IO ()
acceptSeeding seedingSock seedingFileList dInProgress = do 
				
				acceptedSock <- accept seedingSock
				--print "accepted"
				fileNameFromCli <- (recv (fst acceptedSock) 100)
				if (elem fileNameFromCli seedingFileList ) then
				   do	
					send (fst acceptedSock) "yes"				
					--putMVar dInProgress True
					--print fileNameFromCli
					fileHand <- Control.OldException.try (openFile fileNameFromCli ReadMode)
					case fileHand of
					 Right a ->
					   do
						readFileAndSend a bufferSize (fst acceptedSock)
						--return ()
						sClose (fst acceptedSock)
				 	 	--hClose a
					 Left a ->
						sClose (fst acceptedSock)
					 --hClose fileHand
				else 
				   do
					send (fst acceptedSock) "noo"
					return ()
					sClose (fst acceptedSock)
				--sClose seedingSock
				acceptSeeding seedingSock seedingFileList dInProgress

readFileAndSend:: Handle -> Int -> Socket-> IO ()
readFileAndSend a buf sock = do
				h <- hGetContents a
				sendBytes <- (send sock h)
				sClose sock
				putStrLn ("seeder sent:" ++ (show sendBytes))
				return ()

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
					else if s=="close" then
						return 4
						else
							return 0
	
connectToTracker::Int ->Env-> Socket -> MVar [String]->IO Env
connectToTracker requestType env clientSock mvarSeedingList= 
		--putStrLn s 
		if (requestType == 1) then
		 do
			trackerAddr <- inet_addr tracker_ip
			connect clientSock (SockAddrInet tracker_port trackerAddr)
			env2 <- getEnv_ env clientSock ""
			sClose clientSock
			return env2

		else if( requestType ==2) then
		 do
			trackerAddr <- inet_addr tracker_ip
			putStr "enter file name:"
			fileName <- getLine
			checkNF <- Control.OldException.try(openFile fileName ReadMode) 
			case checkNF of 
			 Right a -> do
				hClose a
				connect clientSock (SockAddrInet tracker_port trackerAddr)
				updateTrackerEnv clientSock fileName 
				putMVar mvarSeedingList (addUnique fileName (seedingList env)) 
				sClose clientSock
				return Env {files =files env, seedingList = (addUnique fileName (seedingList env)), downloadList = (downloadList env)}
			 Left a -> do
				putStrLn (show(a))
				return env
		      	
		else if (requestType == 4) then
                  do
                         trackerAddr <- inet_addr tracker_ip
                         connect clientSock (SockAddrInet tracker_port trackerAddr)
                         --env2 <- getEnv env clientSock ""
			 sendTo clientSock "$close$" (SockAddrInet tracker_port trackerAddr)
                         sClose clientSock
                         return env

		else
			return env

getEnv_ :: Env -> Socket -> String -> IO Env
getEnv_ env clientSock msgReceived =do
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
 

