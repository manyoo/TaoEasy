{-# LANGUAGE OverloadedStrings #-}
module JHS where

import Text.HTML.TagSoup
import Network.HTTP
import Network.URI
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Encoding
import Data.Encoding.GB18030
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Map as M

import Control.Monad

import Model
import JHSPage
--import TBAPIServer

channelDict = [("服饰", "1000"),
               ("时尚", "2000"),
               ("鞋包", "3000"),
               ("电器", "4000"),
               ("食品", "5000"),
               ("母婴", "6000"),
               ("家居", "7000"),
               ("其他", "8000")]

getItemsFromJHS = do channelItems <- liftM concat $ mapM (jhsItemsForChannel . fst) channelDict
                     crazyWeekItems <- jhsItemsForCrazyWeek
                     return $ channelItems ++ crazyWeekItems

jhsItemsForChannel :: Text -> IO [JHSItem]
jhsItemsForChannel channel = do
  let req :: Request ByteString
      req = mkRequest GET $ fromJust $ parseURI $ T.unpack $ urlForChannel channel
  res <- getResponseBody =<< simpleHTTP req
  tm <- getCurrentTime
  let tags = parseTags $ decodeStrictByteString GB18030 res
      items = concat $ map removeNothing $ tags2JHSItems tags
      itemIds = map (\jhs -> jhsItemId jhs) items
--  urlMap <- getTBKUrlsForItems itemIds 
      --  jhsTBKUrl = M.lookup (jhsItemId jhs) urlMap
  return $ map (\jhs -> jhs {jhsChannel = channel, jhsCreated = Just tm}) items

urlForChannel :: Text -> Text
urlForChannel channel = T.append "http://ju.taobao.com/tg/today_items.htm?frontCatId=" catid
  where catid = fromMaybe "1000" $ lookup channel channelDict

removeNothing Nothing = []
removeNothing (Just a) = [a]

-- code for crazy weekend
crazyWeekJHSUrl = "http://act.ju.taobao.com/go/act/juhuasuan/crazyweekend.php"

jhsItemsForCrazyWeek :: IO [JHSItem]
jhsItemsForCrazyWeek = do
  let req :: Request ByteString
      req = mkRequest GET $ fromJust $ parseURI crazyWeekJHSUrl
  res <- getResponseBody =<< simpleHTTP req
  tm <- getCurrentTime
  let tags = parseTags $ decodeStrictByteString GB18030 res
      allItems = map (concat . map removeNothing) $ crazyWeekJHSItemsFromTags tags
      getItemIds = map (\jhs -> jhsItemId jhs)
  --urlMaps <- mapM (getTBKUrlsForItems . getItemIds) allItems
      -- , jhsTBKUrl = M.lookup (jhsItemId jhs) urlMap
  return $ concat $ map (\(items, ch) -> map (\jhs -> jhs {jhsChannel = ch, jhsCreated = Just tm}) items) $ zip allItems ["周末1","周末2","周末3"]