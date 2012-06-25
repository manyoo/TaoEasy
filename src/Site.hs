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
import qualified Data.Set as Set

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
import           Spider
import           Model
import           TBKChecker

checkTBKandSaveTo collection items = do
  existing <- maybeWithDB $ rest =<< (find $ select [] collection)
  case existing of
    Nothing -> do newItems <- liftM Set.toList $ liftIO $ checkTBKForItems $ Set.fromList items
                  eitherWithDB $ insertMany collection $ map toDocument newItems
    Just exist -> do let exSet = Set.fromList $ map fromDocument exist
                         itemsSet = Set.fromList items
                         inter = Set.intersection exSet itemsSet
                         new = Set.difference itemsSet inter
                         toDelete = Set.difference exSet inter
                     newItems <- liftM Set.toList $ liftIO $ checkTBKForItems new
                     eitherWithDB $ do
                       mapM_ (\doc -> delete $ select doc collection) $ map toDocument $ Set.toList toDelete
                       insertMany collection $ map toDocument newItems

updateJHSHandler = do
  items <- liftIO getItemsFromJHS
  checkTBKandSaveTo "juhuasuan" items
  writeBS "<html><head><title>Succeed</title></head><body><h1>Succeed!</h1></body></html>"

updateJinbiHandler = do
  extendTimeout 100000000
  types <- liftM (map jinbiItemTypeFromDocument) $ unsafeWithDB $ rest =<< (find $ select [] "jinbitypes")
  items <- liftIO $ getJinbiItems types
  checkTBKandSaveTo "jinbiitems" items
  writeBS "<html><head><title>Succeed</title></head><body><h1>Succeed!</h1></body></html>"

updateTeJiaHandler = do
  extendTimeout 100000000
  items <- liftIO getTeJiaItems
  checkTBKandSaveTo "tejia" items
  writeBS "<html><head><title>Succeed</title></head><body><h1>Succeed!</h1></body></html>"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/api/update_jhs", updateJHSHandler)
         , ("/api/update_jinbi", updateJinbiHandler)
         , ("/api/update_tejia", updateTeJiaHandler)
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

