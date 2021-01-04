--import Network (listenOn,accept,Socket,PortID(..))
import Network.Socket
import System.IO
import System.Environment
import Control.Concurrent
import Network.Socket.ByteString (recv, sendAll)
import Data.List.Split


takeLast n ss = reverse $ take n $ reverse ss

getClientAddr hints addr port = getAddrInfo (Just hints) (Just addr) (Just port)

getServerAddr hints host port = getAddrInfo (Just hints) host (Just port)

makeSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

runClient s_addr port calc = do 
                   let hints = defaultHints {addrSocketType = Stream}
                   addr <- head <$> getClientAddr hints s_addr port
                   sock <- makeSocket addr
                    --Network.Socket.bind sock (addrAddress addr)
                   connect sock $ addrAddress addr
                   -- getSocketName sock
                   putStr "uwaaa"

runServer port = withSocketsDo $ do
                   sock <- socket AF_INET Stream defaultProtocol
                   bind sock (SockAddrInet port  0)
                   listen sock 10
                   waitConnection sock
                   close sock

waitConnection sock = do
          (conn,_) <- accept sock
          forkIO $ putStrLn "yes!" >> rcvMsg conn
          waitConnection sock

rcvMsg conn = do 
          msg <- Network.Socket.ByteString.recv conn 1000
	  putStr "yes"

main = do cs <- getContents
          let as = splitOn " " cs
          let net = (take 4 as) !! 0
          let port = (take 4 as) !! 1
          let addr  = (take 4 as) !! 2
          let calc  = (take 4 as) !! 3
          if net == "net_cli" then runClient addr port calc
                              else if net == "net_ser" then runServer $ read port
                              else putStr "none" 
