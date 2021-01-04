import Network
import System.IO
import System.Environment
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)


--doNet port = do let addr = "192.168.10.1"
--                putStr port
--                host <- connectTo addr $ PortNumber $ toEnum $ read port
--                putStr port

--runClient port = do let addr = "192.168.10.1"
--runClient port = do let addr = "10.0.0.1"
--                    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--                    connect sock $ addrAddress addr
		    --return sock
--		    putStr "yes"

--makeSocket addr = return socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--
takeLast n ss = reverse $ take n $ reverse ss

getAddr hints addr port = getAddrInfo (Just hints) (Just addr) (Just port)

makeSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

runClient port = do let hints = defaultHints {addrFlags = [AI_NUMERICHOST,AI_NUMERICSERV],addrSocketType = Stream}
                    let pre_addr = "127.0.0.1"
		    addr:_ <-  getAddr hints pre_addr port
                    sock <- makeSocket addr
		    Network.Socket.bind sock (addrAddress addr)
		    connect sock $ (addrAddress addr)
		    -- getSocketName sock
                    putStr port

runServer port = putStr port

main = do cs <- getContents
          let as = cs
          let net = take 7 $ as
	  let port = takeLast 4 $ take 12 $ as
	  if net == "net_cli" then runClient port 
                              else if net == "net_ser" then runServer port
                              else putStr "none" 
