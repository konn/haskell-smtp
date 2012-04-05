{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Network.SMTP.Parser where
import Data.Attoparsec.Char8
import Control.Applicative
import Data.ByteString (ByteString)
import Prelude hiding (takeWhile)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)

import Network.SMTP.Types
import Network.SMTP.Parser.TH

range :: Int -> Int -> Parser a -> Parser [a]
range min max p = (++) <$> count min p <*> sub (max - min)
  where
    sub 0 = pure []
    sub n = (:) <$> p <*> sub (n-1)
        <|> pure []

countSep :: Parser b -> Int -> Parser a -> Parser [a]
countSep sep n p | n <= 0    = pure []
                 | otherwise = (:) <$> p <*> count (n - 1) (sep *> p)

sepRange :: Parser b -> Int -> Int -> Parser a -> Parser [a]
sepRange sep min max p = (++) <$> countSep sep min p <*> sub (max - min)
  where
    sub 0 = pure []
    sub n = (:) <$ sep <*> p <*> sub (n - 1)
        <|> pure []

domain, subDomain, address, ipv4, ipv6, gad :: Parser ByteString
domain  = BS.intercalate "." <$> subDomain `sepBy1` char '.'
subDomain = BS.cons <$> (digit <|> satisfy isAlpha_ascii)
                    <*> takeWhile (\c -> c == '-' || isDigit c || isAlpha_ascii c )
address = char '[' *> (try ipv4 <|> try ipv6 <|> gad) <* char ']'
ipv4    = BS.intercalate "." <$> ((:) <$> snum <*> count 3 (char '.' *> snum))
  where
    snum = BS.pack <$> range 1 3 digit
ipv6    = stringCI "IPv6:" *> choice [try full, try comp, try v4full, v4comp]
  where
    v6hex  = BS.pack <$> range 1 4 (satisfy $ inClass "0-9a-fA-F")
    full   = BS.intercalate ":" <$> ((:) <$> v6hex <*> count 7 (char ':' *> v6hex))
    comp   = (BS.append .) . BS.append
             <$> option "" (BS.intercalate ":" <$> sepRange (char ':') 1 6 v6hex)
             <*> string "::"
             <*> option "" (BS.intercalate ":" <$> sepRange (char ':') 1 6 v6hex)
    v4full = (BS.append .) . BS.append <$> (BS.intercalate ":" <$> countSep (char ':') 6 v6hex)
                                       <*> string ":" <*> ipv4
    v4comp = do
      prefix <- option "" (BS.intercalate ":" <$> sepRange (char ':') 1 4 v6hex)
      cc <- string "::"
      infx <- option "" (BS.snoc <$>(BS.intercalate ":" <$> sepRange (char ':') 1 4 v6hex) <*> char ':')
      v4   <- ipv4
      return $ BS.concat [prefix, cc, infx, v4]
gad     = (BS.append .) . BS.snoc
      <$> takeWhile (\a -> isAlpha_ascii a || isDigit a || a == '-')
      <*> char ':'
      <*> takeWhile1 (\c -> ('!' <= c && c <= 'Z') || ('^' <= c && c <= '~'))

revPath, forwardPath, rcptPath :: Parser Path
mailPath = stringCI "FROM:" *> revPath
rcptPath = stringCI "TO:" *>
           choice [ try $ Path []
                               <$  char '<'
                               <*> (Mailbox <$> stringCI "Postmaster"
                               <*  char '@'
                               <*> domain <* char '>')
                  , try $ Path []
                            <$  char '<'
                            <*> (Mailbox <$> stringCI "Postmaster" <*> pure "")
                            <* char '>'
                  , forwardPath
                  ]

revPath = Empty  <$ string "<>"
      <|> forwardPath
forwardPath = Path <$  char '<'
                   <*> option [] ((char '@' *> domain) `sepBy1` char ',' <* char ':')
                   <*> mailbox
                   <*  char '>'

mailbox :: Parser Mailbox
mailbox = Mailbox <$> locPart <* char '@' <*> (domain <|> address)
  where
    locPart = BS.intercalate "." <$> atom `sepBy` char '.'
          <|> quotedString

quotedString :: Parser ByteString
quotedString = BS.pack <$  char '"'
                       <*> many ((char '\\' *> satisfy isPrintable) <|> satisfy others)
                       <* char '"'
  where
    others c = ' ' == c || c == '!' || ('#' <= c && c <= '[') || (']' <= c && c <= '~')

isPrintable c = ' ' <= c && c <= '~'
atom    = takeWhile1 $ inClass "a-zA-Z0-9!#$%&'*+/=?^_`{|}~-"
param   = (,) <$> (BS.cons <$> satisfy (\c -> isAlpha_ascii c || isDigit c )
                           <*> takeWhile (\c -> isAlpha_ascii c || isDigit c || c == '-'))
              <* char '='
              <*> takeWhile1 (inClass "!-<>-~")
params :: Parser [(ByteString, ByteString)]
params = param `sepBy` many1 (char ' ')
smtpString = atom <|> quotedString
optString = optional smtpString

ehloDomain = domain <|> address

defineCommand 'EHLO "EHLO" [ 'ehloDomain ]
defineCommand 'HELO "HELO" [ 'domain ]
defineCommand 'MAIL "MAIL" [ 'mailPath, 'params ]
defineCommand 'RCPT "RCPT" [ 'rcptPath, 'params ]
defineCommand 'RSET "RSET" []
defineCommand 'VRFY "VRFY" [ 'smtpString ]
defineCommand 'EXPN "EXPN" [ 'smtpString ]
defineCommand 'HELP "HELP" [ 'optString ]
defineCommand 'NOOP "NOOP" [ 'optString ]
defineCommand 'QUIT "QUIT" []

dat  = DATA <$  stringCI "DATA"
            <*> dataBody (BS.drop 2)
  where
    dataBody thunk = do
      str <- takeTill (== '\r')
      choice
        [ string "\r\n.\r\n" >> return (thunk str)
        , char '\r' *> dataBody (thunk . BS.append (BS.snoc str '\r'))
        ]

extension :: Parser SMTPCommand
extension = Extension <$> (BS.map toUpper <$> takeWhile1 isAlpha_ascii)
                      <*> option [] (char ' ' *> smtpString `sepBy` char ' ')
                      <*  string "\r\n"

smtpCommand :: Parser SMTPCommand
smtpCommand = helo <|> ehlo <|> mail <|> rcpt <|> dat  <|> rset
          <|> vrfy <|> expn <|> help <|> noop <|> quit

textstring :: Parser ByteString
textstring = takeWhile1 $ inClass "\t -~"

line :: Parser ByteString -> Parser ByteString
line p = p <* string "\r\n"

smtpReply :: Parser SMTPReply
smtpReply = (BS.pack  <$> count 3 digit) >>= reply
  where
    hasDomain = (`elem` ["220", "221", "421"])
    hasPath   = (`elem` ["251", "551"])
    reply code = SMTPReply (read $ BS.unpack code)
                   <$> (if hasDomain code then Just <$ char ' ' <*> (domain <|> address) else pure Nothing)
                   <*> (pure <$> line (option "" (char ' ' *> textstring)))
             <|> SMTPReply (read $ BS.unpack code)
                     <$  char '-'
                     <*> (if hasDomain code then Just <$> (domain <|> address) else pure Nothing)
                     <*> ((:) <$> line (option "" $ (if hasDomain code then char ' ' else char '-') *> textstring)
                              <*> submsg code)
    submsg code = (++) <$> many (string code *> char '-' *> line (option "" textstring))
                       <*  string code <*> (pure <$> line (option "" (char ' ' *> textstring)))
