{-# LANGUAGE DeriveDataTypeable #-}
module Network.SMTP.Types where
import qualified Data.Text as T
import Data.ByteString.Char8 ()
import Data.ByteString (ByteString)
import Data.Data

type Domain = ByteString
type Address = ByteString
data SMTPCommand = HELO Domain
                 | EHLO Domain
                 | MAIL Path [Param]
                 | RCPT Path [Param]
                 | DATA ByteString
                 | RSET
                 | VRFY ByteString
                 | EXPN ByteString
                 | HELP (Maybe ByteString)
                 | NOOP (Maybe ByteString)
                 | QUIT
                 | Extension ByteString [ByteString]
                   deriving (Typeable, Data, Eq, Ord, Show)

data Path = Path [Domain] Mailbox
          | Empty
            deriving (Show, Eq, Ord, Typeable, Data)
data Mailbox = Mailbox { mbLocalPart :: ByteString
                       , mbDomain    :: ByteString
                       } deriving (Show, Eq, Ord, Typeable, Data)

type Param = (ByteString,ByteString)
type EHLOLine = (ByteString, [ByteString])
data SMTPReply = SMTPReply { replyCode    :: Int
                           , replyDomain  :: Maybe Domain
                           , replyMessage :: [ByteString]
                           }
                 deriving (Show, Eq, Ord, Data, Typeable)

succeeded :: SMTPReply -> Bool
succeeded r =
  let s = replyCode r `div`100
  in s == 2 || s == 3
