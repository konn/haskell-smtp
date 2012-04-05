{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Network.SMTP.Parser.TH where
import Language.Haskell.TH
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.Char
import Network.SMTP.Types
import Control.Applicative
import Control.Monad

defineCommand :: Name -> String -> [Name] -> Q [Dec]
defineCommand cmdName str params = do
  let name = mkName $ map toLower $ nameBase cmdName
      hdr  = foldl1 (\a b -> infixApp a [|(<*)|] b) $ map (\ccc -> [| char (toLower ccc) <|> char (toUpper ccc) |]) str
  sig <- sigD name [t| Parser SMTPCommand |]
  let cons = infixApp (conE cmdName) [|(<$)|] hdr
      expr = infixApp (foldl apply cons params) [|(<*)|] [| string "\r\n" |]
  def <- valD (varP name) (normalB expr) []
  return [sig, def]

apply :: ExpQ -> Name -> ExpQ
apply e1 e2 = do
  let ma = ''Maybe
  VarI _ (AppT _ typ) _ _ <- reify e2
  case typ of
    AppT ListT _ -> [| $e1 <*> option [] (char ' ' *> $(varE e2)) |]
    AppT (ConT ma) _ -> [| $e1 <*> (join <$> optional (char ' ' *> $(varE e2))) |]
    _ -> [| $e1 <* skipMany1 (char ' ') <*> $(varE e2) |]
