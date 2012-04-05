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
import Data.HashMap.Strict hiding (foldr, null)
import Data.List
import Data.Attoparsec
import qualified Data.Attoparsec as A
import qualified Data.Conduit as C
import Control.Exception.Lifted
import Prelude hiding (catch)
import Control.Applicative
import Data.Conduit.Attoparsec

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
  src $$ tcpLines
      =$ conduitParser smtpCommand
      =$ genReply
      =$ LC.map renderReply
      =$ sink

main :: IO ()
main = runTCPServer (ServerSettings 10025 HostAny) smtpServer

conduitParser :: MonadIO m => Parser a -> Conduit BS.ByteString m (Either ParseError a)
conduitParser p0 = conduitState (parse p0) push close
  where
    push p c | BS.null c = return $ StateProducing p []
    push p c = do
      liftIO $ putStr "input: " >> print c
      case p c of
        A.Done leftover x -> do
          liftIO $ putStr "\tparse done. lo: " >> print leftover
          return $ StateProducing (parse p0 . BS.append leftover) [Right x]
        A.Fail l cxt msg  -> do
          liftIO $ putStr "\tparse failed: " >> print l
          return $ StateProducing (parse p0) [Left $ ParseError cxt msg]
        A.Partial p' -> do
          liftIO $ putStrLn "\tConsuming...: "
          return $ StateProducing p' []
    close p =
        case feed (p BS.empty) BS.empty of
          A.Done lo y      -> return [Right y]
          A.Fail _ cxt msg -> return [Left $ ParseError cxt msg]
          A.Partial _      -> return [Left DivergentParser]

genReply :: Conduit (Either ParseError SMTPCommand) IO SMTPReply
genReply = conduitState () (const $ \hoge -> return $ StateProducing () [SMTPReply 250 Nothing [BS.pack $ show hoge]])
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
        | otherwise  = getLines (front . ((x `BS.append` "\r\n"):)) (BS.drop 2 y)
      where
        (x, y) = BS.breakSubstring "\r\n" bs

    close front
        | BS.null bs = return []
        | otherwise = return [bs]
      where
        bs = front BS.empty
