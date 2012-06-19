{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock

import           Database.MongoDB

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.MongoDB

import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           JHS
import           Model

updateJHSHandler = do
  items <- liftIO getItemsFromJHS
  eitherWithDB $ do
    delete $ Select [] "juhuasuan"
    insertMany "juhuasuan" $ map itemToDocument items
  writeBS "<html><head><title>Succeed</title></head><body><h1>Succeed!</h1></body></html>"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/api/update_jhs", updateJHSHandler)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    d <- nestSnaplet "database" database $ mongoDBInit 10 (host "127.0.0.1") "TaoEasy"
    addRoutes routes
    return $ App h d

