{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Process
import Control.Monad
import Control.Concurrent
import Control.Exception
import qualified Control.Exception as CE
import Network.Socket
import System.Random
import System.IO
import qualified Data.ByteString as BS
import Text.Printf (printf)
import System.Mem

main :: IO ()
main = do
  listenPort' : targetPort' : sshArgs <- getArgs
  let listenPort = read listenPort' :: Int
      targetPort = read targetPort' :: Int
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  addr <- inet_addr "127.0.0.1"
  bindSocket sock $ SockAddrInet (fromIntegral listenPort) addr
  listen sock 5
  putStrLn $ "listening for connections on port " ++ show listenPort
  forever $ do
    (conn, connaddr) <- accept sock
    hConn <- socketToHandle conn ReadWriteMode
    putStrLn $ "connection accepted: " ++ show connaddr
    forkIO $ do
      tunnelPort :: Int <- randomRIO (50000, 55000)
      handleConnection hConn tunnelPort targetPort sshArgs

handleConnection :: Handle -> Int -> Int -> [String] -> IO ()
handleConnection hConn tunnelPort targetPort sshArgs = do
    (_, _, _, p) <- createProcess (proc "ssh" $ ["-v"] ++ [
      "-L" ++ show tunnelPort ++ ":127.0.0.1:" ++ show targetPort, "-n"] ++
      sshArgs)
    finally forwardToTunnel $ do
      putStrLn "terminating ssh"
      terminateProcess p
      _ <- waitForProcess p
      putStrLn "ssh exited"
      performGC -- a good moment to finalize handles
  
  where forwardToTunnel = do
          hTunn <- tryTunnelConnection tunnelPort 15
          _ <- forkIO $ forward "hTunn->hConn" hTunn hConn
          forward "hConn->hTunn" hConn hTunn
          
        forward desc h1 h2 =
          forever $ do
            bs <- BS.hGetSome h1 4096
            putStrLn $ printf "%s %d bytes" desc (BS.length bs)
            if BS.null bs then
                error "no more data to tunnel"
            else do
                BS.hPut h2 bs
                hFlush h2

tryTunnelConnection :: Int -> Int -> IO Handle
tryTunnelConnection _    0        = error "can't connect to tunnel"
tryTunnelConnection port attempts = do
  threadDelay (1*1000*1000)
  tunn <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "127.0.0.1"
  CE.catch (do connect tunn $ SockAddrInet (fromIntegral port) addr
               hTunn <- socketToHandle tunn ReadWriteMode
               putStrLn "got hTunn" 
               return hTunn)
        (\(_::SomeException) -> do
            sClose tunn
            tryTunnelConnection port (attempts-1))

