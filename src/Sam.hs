{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Sam
    ( main
    ) where

import Servant
import Servant.HTML.Blaze
import Data.Time.Clock
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Proxy
import Network.Wai hiding (ResponseHeaders)
import Network.Wai.Handler.Warp
import Data.Default.Class
import Text.Pretty.Simple
import System.IO

type API = 
       Get '[HTML] Html 
  :<|> "projects" :> Get '[HTML] Html
  :<|> "static" :> Raw

theAPI :: Proxy API
theAPI = Proxy

server :: Server API
server = pure aboutMe :<|> pure projects :<|> serveDirectoryWebApp "/var/www"

logger :: Middleware
logger app req respond = do
  now <- getCurrentTime
  print now
  pPrintNoColor req
  putStr "Request Body: "
  body <- strictRequestBody req
  print body
  app req \response -> do
    pPrintNoColor (responseStatus response)
    pPrintNoColor (responseHeaders response)
    respond response

app :: Application
app = logger (theAPI `serve` server)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  run 80 app

aboutMe :: Html
aboutMe = template AboutMe do
  H.div H.! A.class_ "text-container" $ do
    H.p "I am a software engineer based out of Cambridge, Massachusetts.\
        \ Currently I work for a company called SimSpace, where we use\
        \ Haskell to allow our customers to design, deploy, and manage\
        \ services around cyber ranges."
    H.p "In my spare time, I like to learn about history, mathematics,\
        \ philosophy, and computer science. I also enjoy hiking and exploring\
        \ North America in my Prius."

projects :: Html
projects = template Projects do
  H.div H.! A.class_ "text-container" $ do
    H.h2 $ H.a H.! A.href "https://github.com/SamuelSchlesinger/commander-cli" $ "Commander"
    H.p $ "A " <> (H.a H.! A.href "https://www.servant.dev/" $ "servant") <> "-like library for\
          \ implementing command line programs in Haskell. This is the most useful library I\
          \ have written for myself, and I use it every day. It allows for extremely concise\
          \ implementations of command-line utilities." 
    H.h2 $ H.a H.! A.href "https://github.com/SamuelSchlesinger/user-system" $ "User System"
    H.p "A basic user system implemented in Haskell, with a basic binary object resource\
        \ which users can have various authorization levels to use, modify, and\
        \ create. I wrote this as an exercise to understand password security, browser cookies,\
        \ and authorization."
    H.h2 $ H.a H.! A.href "https://github.com/SamuelSchlesinger/stm-actor" $ "Actor Model"
    H.p "An implementation of the actor model in Haskell using software transactional memory."
    H.h2 $ H.a H.! A.href "https://github.com/SamuelSchlesinger/mtsl" $ "Monad Transformer Stack Library"
    H.p $ "An alternative to " <> (H.a H.! A.href "https://hackage.haskell.org/package/mtl" $ "mtl") <>
          " which uses first class monad transformer stacks. I have not found myself using this much,\
          \ as it is nice to plug into existing Haskell ecosystems."

data Page = AboutMe | Projects

navbar :: Page -> Html
navbar page = H.nav H.! A.class_ "navigation" $ do
  aboutMeLink page
  projectsLink page
  githubLink
  linkedInLink
  resumeLink
  where
    aboutMeLink AboutMe = H.a H.! A.href "#" H.! A.class_ "active" $ "About Me"
    aboutMeLink _ = H.a H.! A.href "/" $ "About Me"
    projectsLink Projects = H.a H.! A.href "#" H.! A.class_ "active" $ "Projects"
    projectsLink _ = H.a H.! A.href "/projects" $ "Projects"
    githubLink = H.a H.! A.href "http://github.com/SamuelSchlesinger" $ "GitHub"
    linkedInLink = H.a H.! A.href "https://www.linkedin.com/in/samuel-schlesinger-8747b9b4/" $ "LinkedIn"
    resumeLink = H.a H.! A.href "/static/resume.pdf" $ "Resume"

template :: Page -> Html -> Html
template page body = H.docTypeHtml do
  H.head do
    H.title "Samuel Schlesinger"
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "static/stylesheet.css?version=0.3"
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
  H.body do
    H.div H.! A.class_ "header" $ do
      H.h1 "Samuel Schlesinger"
      navbar page
    body
