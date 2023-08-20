{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString, elem, isPrefixOf, split)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text hiding (any, elem, filter, find, head, isPrefixOf, map, split)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid.Base hiding (Attribute)
import Lucid.Html5
import Lucid.Htmx
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Text.HTML.TagSoup.Match (anyAttrValue, tagOpen, tagOpenNameLit)
import Prelude hiding (elem)

newtype HomePage = HomePage Text

type StylesHref = "static"

newtype Article = Article
  { title :: Text
  }
  deriving (Show, Generic)

instance FromJSON Article

newtype RandomArticles = RandomArticle
  { items :: [Article]
  }
  deriving (Show, Generic)

instance FromJSON RandomArticles

startWiki = "Adolf_Hitler"

instance ToHtml HomePage where
  toHtml (HomePage page) = doc_ $ main_ $ do
    div_ [class_ "wiki-container"] $ do
      div_ [class_ "blur"] $
        iframe_ [src_ ("/wiki/" <> page)] ""
      button_ [hxGet_ "/play", hxSwap_ "innerHTML", hxTarget_ ".wiki-container", class_ "start-button"] "Start"
    div_ [class_ "stats-container "] $ do
      div_ [class_ "timer"] $ do
        h1_ "Timer"
      div_ [class_ "leaderboard"] $ do
        h1_ "Leaderboard"

  toHtmlRaw = toHtml

type API =
  Get '[HTML] HomePage
    :<|> "wiki" :> Capture "page" Text :> Get '[HTML] (Html ())
    :<|> "play" :> Get '[HTML] (Html ())
    :<|> StylesHref :> Raw

server :: Server API
server =
  return (HomePage startWiki)
    :<|> wikiHandler
    :<|> playHandler
    :<|> serveDirectoryWebApp "static"
  where
    wikiHandler :: Text -> Handler (Html ())
    wikiHandler page = runReq defaultHttpConfig $ do
      bs <- responseBody <$> req Req.GET (https "en.wikipedia.org" /: "wiki" /: page) NoReqBody bsResponse mempty
      let tags = map modify (parseTags bs)
      return $ toHtmlRaw (renderTags tags)

    playHandler :: Handler (Html ())
    playHandler = do
      article <- liftIO fetchRandomArticle
      case article of
        Nothing -> throwError err500
        Just a -> return $ iframe_ [class_ "playing", src_ ("/wiki/" <> title a)] ""

fetchRandomArticle :: IO (Maybe Article)
fetchRandomArticle = runReq defaultHttpConfig $ do
  rArticle :: Maybe RandomArticles <- responseBody <$> req Req.GET (https "en.wikipedia.org" /: "api" /: "rest_v1" /: "page" /: "random" /: "title") NoReqBody jsonResponse mempty
  return (fmap (head . items) rArticle)

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy API) server

doc_ :: (Monad m) => HtmlT m () -> HtmlT m ()
doc_ b =
  doctypehtml_ $
    html_ $ do
      head_ $ do
        useHtmx
        title_ "Find the dictator"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "stylesheet", href_ (urlpath @StylesHref <> "/styles.css")]
      body_ b

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack (symbolVal (Proxy :: Proxy s))

modify :: Tag ByteString -> Tag ByteString
modify (TagOpen "link" attrs) = TagOpen "link" (map (\(a, v) -> if a == "href" then ("href", toWiki v) else (a, v)) attrs)
modify (TagOpen "a" attrs) | anyAttrValue (== "mw-file-description") attrs = TagOpen "a" [("style", "pointer-events: none; cursor: default;")]
modify (TagOpen "a" attrs) = TagOpen "a" (map fixAttributes attrs <> [("style", "color: #3366cc;")])
modify (TagOpen "sup" _) = TagOpen "sup" [displayNone]
modify (TagOpen t attrs) = TagOpen t (map fixAttributes attrs)
modify t = t

displayNone :: Attribute ByteString
displayNone = ("style", "display: none;")

fixAttributes :: Attribute ByteString -> Attribute ByteString
fixAttributes a = case a of
  ("class", c) -> if any (`S.member` hiddenClasses) (split " " c) then displayNone else a
  ("id", c) -> if c `S.member` hiddenIds then displayNone else a
  ("href", h) -> ("href", fixHref h)
  ("src", h) -> ("src", toWiki h)
  _ -> a

fixHref :: ByteString -> ByteString
fixHref h
  | ":" `elem` h = "javascript:;"
  | "/wiki/" `isPrefixOf` h = toAnother "//localhost:8080" h
  | otherwise = "javascript:;"

toWiki :: ByteString -> ByteString
toWiki = toAnother "//en.wikipedia.org"

toAnother :: ByteString -> ByteString -> ByteString
toAnother to h = if any (`isPrefixOf` h) ["//", "#"] then h else to <> h

hiddenClasses :: Set ByteString
hiddenClasses = S.fromList ["vector-header-container", "reflist", "mw-editsection", "external", "metadata", "mw-footer-container", "refbegin", "mw-portlet", "right-navigation", "vector-page-toolbar", "vector-body-before-content", "IPA", "authority-control", "sistersitebox"]

hiddenIds :: Set ByteString
hiddenIds = S.fromList ["References", "External_links", "Notes", "toc-External_links", "toc-Notes", "toc-References"]
