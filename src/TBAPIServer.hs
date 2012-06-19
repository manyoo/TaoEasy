{-# LANGUAGE OverloadedStrings #-}

module TBAPIServer
       where


import           Prelude hiding (concat, putStrLn)

import           Control.Monad.Trans
import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString as BS
import           Data.ByteString hiding (empty, head, zipWith, map, foldl', length, putStr, take, tail, pack)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Base64 as Base64
import           Data.Maybe

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)

import           Data.Text hiding (empty, head, zipWith, map, foldl', length, take, tail, split, append, toUpper, concat, intercalate)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Aeson

import           Data.Digest.Pure.MD5
import           Data.Char (toUpper)
import           Data.List hiding (concat, intercalate)

import           Snap.Core hiding (GET, Request)
import           Snap.Snaplet

import           Network.HTTP hiding (getRequest)
import           Network.URI hiding (query)


import Application
import Model
import Util
import DataBase

defaultAppKey = "12391996"
defaultAppSecret = "15bbaf6509958270284671e6d7eb6cc0"
defaultTBKPID = "28971285"

-- handlers for manager login
validateCallbackParam n m = do if (M.member n m) && ((BS.length $ head $ m M.! n) /= 0)
                                 then return ()
                                 else writeLBS "<html><head><title>Login Failed</title></head><body><h1>Login Failed</h1></body></html>" >> getResponse >>= finishWith

validateVisitorNick m = do if (M.member "visitor_nick" m) && (head $ m M.! "visitor_nick") == "ericsyw"
                              then return ()
                              else writeLBS "<html><head><title>Login Failed</title></head><body><h1>You are not allowed to login to this server!</h1></body></html>" >> getResponse >>= finishWith

managerLoginHandler :: AppHandler ()
managerLoginHandler = redirect "http://container.open.taobao.com/container?appkey=12391996&encode=utf-8"

loginCallbackHandler = do
  params <- getParams
  validateCallbackParam "top_session" params
  validateCallbackParam "top_parameters" params
  let topSession = decodeUtf8 $ head $ params M.! "top_session"
      topParameters = head $ params M.! "top_parameters"
      paramStr = Base64.decodeLenient topParameters
      paramList = split (L.head "&") paramStr
      paramDict = foldl' addParam M.empty paramList
      addParam m str = M.insert (head $ split (L.head "=") str) (tail $ split (L.head "=") str) m
  validateVisitorNick paramDict
  validateCallbackParam "expires_in" paramDict
  validateCallbackParam "refresh_token" paramDict
  validateCallbackParam "re_expires_in" paramDict
  tz <- liftIO getCurrentTimeZone
  tm <- liftIO getCurrentTime
  let sessionExpire = read $ U.toString $ head $ paramDict M.! "expires_in" :: Int
      refreshTokenExpire = read $ U.toString $ head $ paramDict M.! "re_expires_in" :: Int
      refreshToken = decodeUtf8 $ head $ paramDict M.! "refresh_token"
      sqlValues = (topSession, sessionExpire, refreshToken, refreshTokenExpire,utcToLocalTime tz tm)
  withTransaction (\conn -> do 
                      MSS.execute_ conn "DELETE FROM jinglingtao_sessionkey"
                      MSS.execute conn "INSERT INTO jinglingtao_sessionkey (session_key,session_expire,refresh_token,refresh_token_expire,created) VALUES (?,?,?,?,?)" sqlValues)
  writeLBS "<html><head><title>Login Successfully</title></head><body><h1>Login Successfully</h1></body></html>"
  finishWithHTML


fromResult :: a -> Result a -> a
fromResult def (Error _) = def
fromResult def (Success a) = a

refreshSession session = do
  let params = M.insert "appkey" [defaultAppKey] $ M.insert "refresh_token" [encodeUtf8 $ refreshToken session] $ M.singleton "sessionkey" [encodeUtf8 $ sessionKey session]
      strArr = ["appkey",defaultAppKey,"refresh_token", encodeUtf8 $ refreshToken session, "sessionkey", encodeUtf8 $ sessionKey session, defaultAppSecret]
      sign = U.fromString $ map toUpper $ show $ md5 $ L8.fromChunks strArr
      newParams = M.insert "sign" [sign] params
      paramURL = printUrlEncoded newParams
      url = append "http://container.open.taobao.com/container/refresh?" paramURL
      uri = fromJust $ parseURI $ U.toString url
      req = mkRequest GET uri :: Request L8.ByteString
  result <- liftIO $ simpleHTTP req
  case result of
    Left err -> return Nothing
    Right resp -> case rspCode resp of
      (2,0,0) -> do let jv = decode $ rspBody resp
                    case jv of
                      Just o -> do let obj = fromResult (HM.empty :: HM.HashMap ByteString Value) $ fromJSON o
                                       getObj name o = fromJSON $ fromMaybe Null $ HM.lookup name o
                                       newSession = fromResult "" $ getObj "top_session" obj
                                       newExpire = fromResult 0 $ getObj "expires_in" obj
                                       newToken = fromResult "" $ getObj "refresh_token" obj
                                       newTokenExpire = fromResult 0 $ getObj "re_expires_in" obj
                                   tz <- liftIO getCurrentTimeZone
                                   tm <- liftIO getCurrentTime
                                   let sqlValues = (newSession, newExpire, newToken, newTokenExpire, utcToLocalTime tz tm)
                                   withTransaction (\conn -> do MSS.execute_ conn "DELETE FROM jinglingtao_sessionkey" 
                                                                MSS.execute conn "INSERT INTO jinglingtao_sessionkey (session_key,session_expire,refresh_token,refresh_token_expire,created) VALUES (?,?,?,?,?)" sqlValues)
                                   return $ Just $ SessionKey {sessionKey = newSession, sessionExpire = newExpire, refreshToken = newToken, refreshTokenExpire = newTokenExpire, created = utcToLocalTime tz tm}
                      _ -> return Nothing
      _ -> return Nothing


----------------------------------------------------------------------------------------------------
-- This is the more useful functions for interacting with Taobao Server.
----------------------------------------------------------------------------------------------------

data TBAPIMethod = USER_GET
                 | ITEMCATS_GET
                 | ITEM_GET
                 | ITEM_LIST_GET
                 | ITEMS_GET
                 | TBK_ITEMS_GET
                 | TBK_CONVERT_ITEM
                 | TBK_REPORT_GET
                 | TRADERATES_SEARCH
                 | TBK_SHOPS_GET
                 | SHOP_GET
                 | COLLECT_ITEM_GET
                 | COLLECT_ITEM_ADD
                 | GET_PROMOTION
  
methodToByString USER_GET = "taobao.user.get"
methodToByString ITEMCATS_GET = "taobao.itemcats.get"  
methodToByString ITEM_GET = "taobao.item.get"
methodToByString ITEM_LIST_GET = "taobao.items.list.get"  
methodToByString ITEMS_GET = "taobao.items.get"
methodToByString TBK_ITEMS_GET = "taobao.taobaoke.items.get"
methodToByString TBK_CONVERT_ITEM = "taobao.taobaoke.items.convert"
methodToByString TBK_REPORT_GET = "taobao.taobaoke.report.get"
methodToByString TRADERATES_SEARCH = "taobao.traderates.search"
methodToByString TBK_SHOPS_GET = "taobao.taobaoke.shops.get"
methodToByString SHOP_GET = "taobao.shop.get"
methodToByString COLLECT_ITEM_GET = "taobao.favorite.search"
methodToByString COLLECT_ITEM_ADD = "taobao.favorite.add"
methodToByString GET_PROMOTION = "taobao.ump.promotion.get"

signParam param = U.fromString $ map toUpper $ show $ md5 $ L8.fromChunks [paramStr]
  where
    kvStrs = (flip map) (M.keys param) (\k -> append k $ head $ param M.! k)
    paramStr = concat $ defaultAppSecret:kvStrs++[defaultAppSecret]

setupRequest sKey method params lt = mkRequest GET uri
  where
    uri = fromJust $ parseURI $ U.toString urlStr
    urlStr = append "http://gw.api.taobao.com/router/rest?" $ printUrlEncoded newParams
    newParams = M.insert "sign" [signParam allParams] $ addSession allParams
    addSession p = case sKey of
      Nothing -> p
      Just k -> M.insert "session" [k] p
    allParams = M.union params $ M.fromList [("method", [methodToByString method]),
                                             ("timestamp", [timeStamp]),
                                             ("format", ["json"]),
                                             ("app_key", [defaultAppKey]),
                                             ("v", ["2.0"]),
                                             ("sign_method", ["md5"])]
    timeStamp = U.fromString $ formatTime defaultTimeLocale "%F %T" lt

getResponseForMethod skey method params = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  let req = setupRequest skey method params $ utcToLocalTime tz tm
  result <- simpleHTTP req
  case result of
    Left error -> return Nothing
    Right resp -> case rspCode resp of
      (2, 0, 0) -> return $ decode $ rspBody resp
      _ -> return Nothing

checkCommissionRateForItems :: [Text] -> IO (M.Map Text Double)
checkCommissionRateForItems itemIds = do
  let p = M.fromList [("fields", ["num_iid,commission_rate"]),
                      ("pid", [defaultTBKPID]),
                      ("num_iids", [itemIdStr])]
      itemIdStr = encodeUtf8 $ T.intercalate "," itemIds
  resp <- getResponseForMethod Nothing TBK_CONVERT_ITEM p
  case resp of
    Nothing -> return M.empty
    Just o -> return $ fromResult M.empty $ processObj o >>= checkError >>= processConvertResponse >>= processItems >>= processItem
  where
    processObj = fromJSON
    getObj :: FromJSON a => Text -> HM.HashMap Text Value -> Result a
    getObj name o = fromJSON $ fromMaybe Null $ HM.lookup name o
    
    checkError o = case HM.lookup "error_response" o of
      Just ov -> Error "Error Occurred"
      Nothing -> return o
    
    processConvertResponse = getObj "taobaoke_items_convert_response"
    processItems = getObj "taobaoke_items"
    processItem o = case getObj "taobaoke_item" o of
      Error _ -> return M.empty
      Success arr -> return $ foldl' processOneItem M.empty arr
    
    processOneItem m item = let itemId = pack $ show $ fromResult (0 :: Integer) $ getObj "num_iid" item
                                commRate = (read $ fromResult "" $ getObj "commission_rate" item) / 10000.0
                            in M.insert itemId commRate m


getTBKUrlsForItems :: [Text] -> IO (M.Map Text Text)
getTBKUrlsForItems itemIds = do
  let p = M.fromList [("fields", ["num_iid,click_url"]),
                      ("pid", [defaultTBKPID]),
                      ("num_iids", [itemIdStr]),
                      ("is_mobile", ["true"])]
      itemIdStr = encodeUtf8 $ T.intercalate "," itemIds
  resp <- getResponseForMethod Nothing TBK_CONVERT_ITEM p
  case resp of
    Nothing -> return M.empty
    Just o -> return $ fromResult M.empty $ processObj o >>= checkError >>= processConvertResponse >>= processItems >>= processItem
  where
    processObj = fromJSON
    getObj :: FromJSON a => Text -> HM.HashMap Text Value -> Result a
    getObj name o = fromJSON $ fromMaybe Null $ HM.lookup name o
    
    checkError o = case HM.lookup "error_response" o of
      Just ov -> Error "Error Occurred"
      Nothing -> return o
    
    processConvertResponse = getObj "taobaoke_items_convert_response"
    processItems = getObj "taobaoke_items"
    processItem o = case getObj "taobaoke_item" o of
      Error _ -> return M.empty
      Success arr -> return $ foldl' processOneItem M.empty arr
    
    processOneItem m item = let itemId = pack $ show $ fromResult (0 :: Integer) $ getObj "num_iid" item
                                url = fromResult "" $ getObj "click_url" item
                            in M.insert itemId url m
