{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Network.SMTP.Server where
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.List as LC
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as BC
import Network.SMTP.Server.CStateT
import Control.Monad.IO.Class
import Data.HashMap.Strict
import Data.Attoparsec
import Data.Conduit.Attoparsec
import qualified Data.Conduit as C

import Network.SMTP.Types
import Network.SMTP.Render
import Network.SMTP.Parser

import System.IO

data SMTPState = SState { buffer :: BS.ByteString
                        , stateTable :: HashMap BS.ByteString BS.ByteString
                        }
type SMTPServer = ResourceT IO

smtpServer :: Application IO
smtpServer src sink = do
  LC.sourceList [ renderReply $ SMTPReply 220 (Just "localhost") ["hello"]] $$ sink
  src $$ C.sequence (sinkParser smtpCommand)
      =$ LC.map ((`BS.append` "\r\n") . BS.pack . show)
      =$ sink

main :: IO ()
main = do
  runTCPServer (ServerSettings 10025 HostAny) smtpServer

hoge :: IO ()
hoge = runResourceT $ do
  BC.sourceHandle stdin $$ C.sequence (sinkParser smtpCommand)
                        =$ LC.mapM_ (liftIO . print)

genReply :: Conduit SMTPCommand IO SMTPReply
genReply = conduitState () (const $ const $ return $ StateProducing () [SMTPReply 250 Nothing []])
             (const $ return [])

tcpLines :: Monad m => Conduit BS.ByteString m BS.ByteString
tcpLines = conduitState id push close
  where
    push front bs' = return $ StateProducing leftover ls
      where
        bs = front bs'
        (leftover, ls) = getLines id bs

    getLines front bs
        | BS.null bs = (id, front [])
        | BS.null y  = (BS.append x, front [])
        | otherwise  = getLines (front . ((x:).("\r\n":))) (BS.drop 2 y)
      where
        (x, y) = BS.breakSubstring "\r\n" bs

    close front
        | BS.null bs = return []
        | otherwise = return [bs]
      where
        bs = front BS.empty
