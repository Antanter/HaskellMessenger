import System.Exit (exitSuccess)
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad (forever, void)
import Text.Printf (printf)

runServer :: String -> IO ()
runServer myName = do
  addr <- resolveServer "0"
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1

  SockAddrInet port _ <- getSocketName sock
  printf "Server started at port %d\n" (fromIntegral port :: Int)

  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "New connection from: " ++ show peer
    putStrLn "Establish it? (yes/no):"
    decision <- getLine
    if decision == "yes"
      then do
        putStrLn "Connection established"
        handle <- socketToHandle conn ReadWriteMode
        hSetBuffering handle LineBuffering

        peerName <- hGetLine handle
        putStrLn $ "Connected with: " ++ peerName

        hPutStrLn handle myName
        hFlush handle

        void $ forkIO $ forever $ do
          msg <- hGetLine handle
          putStrLn $ "\r" ++ peerName ++ ": " ++ msg
          putStr "> "
          hFlush stdout

        inputLoop handle
      else do
        putStrLn "Connection refused."
        close conn

runClient :: String -> String -> String -> IO ()
runClient myName host port = do
  addr <- resolveClient host port
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering

  putStrLn "Connected to server"

  hPutStrLn handle myName
  hFlush handle

  peerName <- hGetLine handle
  putStrLn $ "Connected with: " ++ peerName

  void $ forkIO $ forever $ do
    msg <- hGetLine handle
    putStrLn $ "\r" ++ peerName ++ ": " ++ msg
    putStr "> "
    hFlush stdout

  inputLoop handle

resolveServer :: ServiceName -> IO AddrInfo
resolveServer port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) Nothing (Just port)

resolveClient :: HostName -> ServiceName -> IO AddrInfo
resolveClient host port = do
  let hints = defaultHints { addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

inputLoop :: Handle -> IO ()
inputLoop handle = do
  putStrLn "Type messages. Press ESC to quit."
  let loop = do
        putStr "> "
        hFlush stdout
        line <- getLine
        if line == "\ESC"
          then do
            putStrLn "Exiting chat..."
            hClose handle
            exitSuccess
          else do
            hPutStrLn handle line
            hFlush handle
            loop
  loop

main :: IO ()
main = do
  putStrLn "Hello! Please, introduce yourself:"
  name <- getLine
  putStrLn ("Hi again, " ++ name ++ "!")

  putStrLn "Provide the IPv4 address to connect (or press Enter to wait for connections):"
  addr <- getLine

  if null addr
    then do
      void $ forkIO (runServer name)
      threadDelay 1000000
      putStrLn "Waiting for connections..."
      forever $ threadDelay 1000000
    else do
      putStrLn "Provide port:"
      port <- getLine
      runClient name addr port
