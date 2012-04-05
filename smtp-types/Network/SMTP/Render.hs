{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports #-}
module Network.SMTP.Render where
import qualified Data.ByteString.Char8 as BS
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.Monoid
import Data.List
import Data.Char

import Network.SMTP.Types

initTail :: [a] -> ([a], a)
initTail []                = error "empty list: initTail"
initTail (x:xs)            = initTail' x xs
  where initTail' x []     = ([], x)
        initTail' y (z:zs) = let (ys, l) = initTail' z zs
                             in (y:ys, l)

replyBuilder :: SMTPReply -> Builder
replyBuilder reply@SMTPReply{replyCode = code}
  | [] <- replyMessage reply     = mconcat [ fromShow code
                                           , maybe mempty (fromByteString . (BS.cons ' ')) $ replyDomain reply
                                           , fromByteString "\r\n"
                                           ]
  | [msg] <- replyMessage reply  = mconcat [ fromShow code
                                           , maybe mempty (fromByteString . (BS.cons ' ')) $ replyDomain reply
                                           , fromChar ' '
                                           , fromByteString msg, fromByteString "\r\n"
                                           ]
  | otherwise =
      let (x:xs, y) = initTail $ replyMessage reply
      in mconcat $
           [ fromShow code, fromChar '-'
           , maybe mempty (fromByteString . (`BS.snoc` ' ')) $ replyDomain reply
           , fromByteString x, fromByteString "\r\n"
           ]
           ++  concatMap (\msg -> [fromShow code, fromChar '-', fromByteString msg, fromByteString "\r\n"]) xs
           ++ [fromShow code, fromChar ' ', fromByteString y, fromByteString "\r\n"]

renderReply :: SMTPReply -> BS.ByteString
renderReply = toByteString . replyBuilder

fromPath :: Path -> Builder
fromPath Empty = fromByteString "<>"
fromPath (Path als mb) = mconcat (map fromByteString (intersperse "," als)) `mappend` fromMailbox mb

fromMailbox :: Mailbox -> Builder
fromMailbox (Mailbox local dom) = fromByteString local
                                  `mappend` fromChar '@'
                                  `mappend` fromByteString dom

fromSMTPString :: BS.ByteString -> Builder
fromSMTPString bs = fromChar '"' `mappend` fromByteString (BS.concatMap escape bs) `mappend` fromChar '"'
  where
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\r' = "\\r"
    escape '\n' = "\\n"
    escape c    = BS.singleton c

commandBuilder :: SMTPCommand -> Builder
commandBuilder (HELO dom) = fromByteString "HELO " `mappend` fromByteString dom `mappend` fromByteString "\r\n"
commandBuilder (EHLO dom) = fromByteString "EHLO " `mappend` fromByteString dom `mappend` fromByteString "\r\n"
commandBuilder (MAIL from ps)
    = mconcat (fromByteString "MAIL FROM:"
              : fromPath from
              : concatMap (\(a,b) -> [fromChar ' ', fromByteString a, fromChar '=', fromByteString b]) ps)
      `mappend` fromByteString "\r\n"
commandBuilder (RCPT from ps)
    = mconcat (fromByteString "RCPT FROM:"
              : fromPath from
              : concatMap (\(a,b) -> [fromChar ' ', fromByteString a, fromChar '=', fromByteString b]) ps)
      `mappend` fromByteString "\r\n"
commandBuilder (DATA body) = fromByteString "DATA\r\n"
                               `mappend` fromByteString body
                               `mappend` fromByteString "\r\n.\r\n"
commandBuilder RSET        = fromByteString "RSET\r\n"
commandBuilder (VRFY name) = fromByteString "VRFY " `mappend` fromSMTPString name
                                                    `mappend` fromByteString "\r\n"
commandBuilder (EXPN name) = fromByteString "EXPN " `mappend` fromSMTPString name
                                                    `mappend` fromByteString "\r\n"
commandBuilder (HELP name) = fromByteString "HELP"
                               `mappend` maybe mempty (mappend (fromChar ' ') . fromSMTPString) name
                               `mappend` fromByteString "\r\n"
commandBuilder (NOOP comm) = fromByteString "NOOP"
                               `mappend` maybe mempty (mappend (fromChar ' ') . fromSMTPString) comm
                               `mappend` fromByteString "\r\n"
commandBuilder QUIT        = fromByteString "QUIT" `mappend` fromByteString "\r\n"
commandBuilder (Extension cmd [])
    = fromByteString (BS.map toUpper cmd) `mappend` fromByteString "\r\n"
commandBuilder (Extension cmd xs)
    = mconcat $ fromByteString (BS.map toUpper cmd) : map fromByteString  (intersperse " " xs)

renderCommand :: SMTPCommand -> BS.ByteString
renderCommand = toByteString . commandBuilder
