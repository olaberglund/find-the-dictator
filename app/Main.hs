{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString (ByteString, isPrefixOf)
import Data.List (any, find)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text hiding (any, elem, filter, find, isPrefixOf, map)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid hiding (Attribute)
import Lucid.Base hiding (Attribute)
import Lucid.Html5
import Network.HTTP.Req
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Text.HTML.TagSoup.Match (anyAttrValue, tagOpen, tagOpenNameLit)

newtype HomePage = HomePage Text

type StylesHref = "static"

instance ToHtml HomePage where
  toHtml (HomePage page) = doc_ $ main_ $ do
    iframe_ [src_ ("/wiki/" <> page)] ""
    div_ [class_ "stats-container"] $ do
      div_ [class_ "timer"] $ do
        h1_ "Timer"
      div_ [class_ "leaderboard"] $ do
        h1_ "Leaderboard"

  toHtmlRaw = toHtml

type API =
  Get '[HTML] HomePage
    :<|> "wiki" :> Capture "page" Text :> Get '[HTML] (Html ())
    :<|> StylesHref :> Raw

server :: Server API
server =
  return (HomePage "Mjölby")
    :<|> wikiHandler
    :<|> serveDirectoryWebApp "static"
  where
    wikiHandler :: Text -> Handler (Html ())
    wikiHandler page = runReq defaultHttpConfig $ do
      bs <- responseBody <$> req Network.HTTP.Req.GET (https "en.wikipedia.org" /: "wiki" /: page) NoReqBody bsResponse mempty
      let tags = map modify (parseTags bs)
      return $ toHtmlRaw (renderTags tags)

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy API) server

doc_ :: (Monad m) => HtmlT m () -> HtmlT m ()
doc_ b =
  doctypehtml_ $
    html_ $ do
      head_ $ do
        title_ "Find the dictator"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "stylesheet", href_ (urlpath @StylesHref <> "/styles.css")]
      body_ b

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack (symbolVal (Proxy :: Proxy s))

modify :: Tag ByteString -> Tag ByteString
modify (TagOpen "link" attrs) = TagOpen "link" (map (\(a, v) -> if a == "href" then ("href", toWiki v) else (a, v)) attrs)
modify (TagOpen "a" attrs) | anyAttrValue (== "mw-file-description") attrs = TagOpen "a" [("style", "er-events: none; cursor: default;")]
modify (TagOpen "sup" _) = TagOpen "sup" [("hidden", "hidden")]
modify (TagOpen t attrs) = TagOpen t (map fixAttributes attrs)
modify t = t

fixAttributes :: Attribute ByteString -> Attribute ByteString
fixAttributes a = case a of
  ("class", c) -> if c `S.member` hiddenClasses then ("hidden", "hidden") else a
  ("href", h) -> ("href", toApp h)
  ("src", h) -> ("src", toWiki h)
  _ -> a

toApp :: ByteString -> ByteString
toApp = toAnother "//localhost:8080"

toWiki :: ByteString -> ByteString
toWiki = toAnother "//en.wikipedia.org"

toAnother :: ByteString -> ByteString -> ByteString
toAnother to h = if any (`isPrefixOf` h) ["//", "#"] then h else to <> h

hiddenClasses :: Set ByteString
hiddenClasses = S.fromList ["vector-header-container", "vector-dropdown mw-portlet mw-portlet-lang", "vector-page-toolbar", "mw-indicators", "reflist reflist-lower-alpha", "reflist reflist-columns references-column-width", "refbegin refbegin-columns references-column-width", "spoken-wikipedia sisterproject noprint haudio", "mw-footer-container", "navbar plainlinks hlist navbar-mini", "mw-editsection", "citation wikicite"]
