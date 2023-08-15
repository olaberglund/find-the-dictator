{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lucid.Base
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid

data HomePage = HomePage

type StylesHref = "static"

instance ToHtml HomePage where
  toHtml HomePage = doc_ $ do
    main_ $ do
      wikipedia
      div_ [class_ "stats-container"] $ do
        div_ [class_ "timer"] $ do
          h1_ "Timer"
        div_ [class_ "leaderboard"] $ do
          h1_ "Leaderboard"

  toHtmlRaw = toHtml

type API =
  Get '[HTML] HomePage
    :<|> StylesHref :> Raw

server :: Server API
server =
  return HomePage
    :<|> serveDirectoryWebApp "static"

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
      body_ $ do
        b

wikipedia :: (Monad m) => HtmlT m ()
wikipedia = iframe_ [src_ "https://wikipedia.org", title_ "Wikipedia"] ""

urlpath :: forall s. (KnownSymbol s) => Text
urlpath = pack (symbolVal (Proxy :: Proxy s))
