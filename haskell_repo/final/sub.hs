import Network.Socket 
import Control.Concurrent
import Network.Socket.ByteString (recv,sendAll) 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as S
import Data.List.Split


takeLast n ss = reverse $ take n $ reverse ss

getClientAddr hints addr port = getAddrInfo (Just hints) (Just addr) (Just port)

getServerAddr hints host port = getAddrInfo (Just hints) host (Just port)

makeSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

runClient s_addr port calc = withSocketsDo $ do 
                   let hints = defaultHints {addrSocketType = Stream}
                   addr <- head <$> getClientAddr hints s_addr port
                   sock <- makeSocket addr
                   connect sock $ addrAddress addr
                   -- getSocketName soc
                   --msgSend sock "aaa"
		   msgSend sock calc
		   close sock

msgSend sock msg = do 
          sendAll sock $ B8.pack msg
	  res <- Network.Socket.ByteString.recv sock 1000
	  B8.putStrLn res


runServer port = withSocketsDo $ do
                   sock <- socket AF_INET Stream defaultProtocol
                   bind sock (SockAddrInet port  0)
                   listen sock 10
                   waitConnection sock
                   close sock

waitConnection sock = do
          (conn,_) <- accept sock
          forkIO $ rcvMsg conn
          waitConnection sock

rcvMsg conn = do 
          --msg <- Network.Socket.recv conn 100
          msg <- Network.Socket.ByteString.recv conn 1000
          B8.putStrLn msg
          --do calc
          --send result
	  let res = "2"
	  sendAll conn $ B8.pack res
         --send conn "yes"

main = do cs <- getContents
          let as = take 4 $ splitOn " " cs
          let net = as !! 0
          let port = as !! 1
          let addr  = as !! 2
          let calc  =  as !! 3
          if net == "net_cli" then runClient addr port calc
                              else if net == "net_ser" then runServer $ read port
                              else putStr "none" 
