{-# LANGUAGE OverloadedStrings #-}
module Spider where

import Text.HTML.TagSoup
import Network.HTTP
import Network.URI
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.UTF8 as U
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
import JinbiPage
--import TBAPIServer

-- utils
tagsForUrl isGBK hdrs u = do
  let req :: Request ByteString
      req = setHeaders (mkRequest GET $ fromJust $ parseURI $ T.unpack u) hdrs
  res <- getResponseBody =<< simpleHTTP req
  if isGBK
    then return $ parseTags $ decodeStrictByteString GB18030 res
    else return $ parseTags $ U.toString res

-- code for JuHuaSuan
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
  tags <- tagsForUrl True [] $ urlForChannel channel
  tm <- getCurrentTime
  let items = concat $ map removeNothing $ tags2JHSItems tags
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
  tags <- tagsForUrl True [] crazyWeekJHSUrl
  tm <- getCurrentTime
  let allItems = map (concat . map removeNothing) $ crazyWeekJHSItemsFromTags tags
      getItemIds = map (\jhs -> jhsItemId jhs)
  --urlMaps <- mapM (getTBKUrlsForItems . getItemIds) allItems
      -- , jhsTBKUrl = M.lookup (jhsItemId jhs) urlMap
  return $ concat $ map (\(items, ch) -> map (\jhs -> jhs {jhsChannel = ch, jhsCreated = Just tm}) items) $ zip allItems ["周末1","周末2","周末3"]


-- code for TaoJinBi
getJinbiItems :: [JinbiItemType] -> IO [JinbiItem]
getJinbiItems cats = liftM concat $ mapM getJinbiItemsForCat cats

getJinbiItemsForCat :: JinbiItemType -> IO [JinbiItem]
getJinbiItemsForCat cat = getJinbiStartWith 1
  where getJinbiStartWith p = do (items, next) <- getJinbiItemsForCatOnPage cat p
                                 if next
                                   then liftM (items ++) $ getJinbiStartWith (p + 1)
                                   else return items

getJinbiItemsForCatOnPage :: JinbiItemType -> Int -> IO ([JinbiItem], Bool)
getJinbiItemsForCatOnPage cat page = do
  let hostHdr = mkHeader HdrHost "taojinbi.taobao.com"
      uaHdr = mkHeader HdrUserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_4) AppleWebKit/534.57.2 (KHTML, like Gecko) Version/5.1.6 Safari/534.56.5"
  tags <- tagsForUrl True [hostHdr, uaHdr] $ jinbiUrlForCatAndPage (jbTypeId cat) page
  return $ tags2JinbiItems cat tags

jinbiUrlForCatAndPage cat page = T.intercalate "" ["http://taojinbi.taobao.com/home/category_search_home.htm?order=2&category_id=", catStr, "&page=", pageStr]
  where pageStr = T.pack $ show page
        catStr = T.pack $ show cat

