{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Text hiding (map, foldl')
import qualified Data.Text as T
import Data.Time.Clock
import Data.Maybe

import Data.Bson


class TBItem a where
  itemTitle :: a -> Text
  itemId :: a -> Text
  itemTBKUrl :: a -> Maybe Text
  itemUrl :: a -> Text
  setTBKUrl :: a -> Maybe Text -> a
  itemOrigPrice :: a -> Text
  itemNewPrice :: a -> Text
  toDocument :: a -> Document
  fromDocument :: Document -> a

itemUrlForId id = T.concat ["http://a.m.taobao.com/i", id, ".htm"]

data JHSItem = JHSItem {
  jhsItemId :: Text,
  jhsTitle :: Text,
  jhsChannel :: Text,
  jhsTBKUrl :: Maybe Text,
  jhsOrigPrice :: Double,
  jhsNewPrice :: Double,
  jhsDiscount :: Text,
  jhsSold :: Int,
  jhsPicUrl :: Text,
  jhsCreated :: Maybe UTCTime
  } deriving Show

instance Eq JHSItem where
  j1 == j2 = jhsItemId j1 == jhsItemId j2

instance Ord JHSItem where
  compare j1 j2 = compare (jhsItemId j1) (jhsItemId j2)

instance TBItem JHSItem where
  itemTitle = jhsTitle
  itemId = jhsItemId
  itemTBKUrl = jhsTBKUrl
  itemUrl a = case jhsTBKUrl a of
    Just u -> u
    Nothing -> itemUrlForId $ jhsItemId a
  setTBKUrl a u = a {jhsTBKUrl = u}
  itemOrigPrice = T.pack . show . jhsOrigPrice
  itemNewPrice = T.pack . show . jhsNewPrice
  toDocument = itemToDocument
  fromDocument = itemFromDocument

itemToDocument item = ["item_id" =: jhsItemId item,
                       "title" =: jhsTitle item,
                       "channel" =: jhsChannel item,
                       "tbk_url" =: jhsTBKUrl item,
                       "orig_price" =: jhsOrigPrice item,
                       "new_price" =: jhsNewPrice item,
                       "discount" =: jhsDiscount item,
                       "sold" =: jhsSold item,
                       "pic_url" =: jhsPicUrl item,
                       "created" =: jhsCreated item]

itemFromDocument doc = JHSItem {
  jhsItemId = getTextValue doc "item_id",
  jhsTitle = getTextValue doc "title",
  jhsChannel = getTextValue doc "channel",
  jhsTBKUrl = doc !? "tbk_url",
  jhsOrigPrice = getDoubleValue doc "orig_price",
  jhsNewPrice = getDoubleValue doc "new_price",
  jhsDiscount = getTextValue doc "discount",
  jhsSold = getIntValue doc "sold",
  jhsPicUrl = getTextValue doc "pic_url",
  jhsCreated = doc !? "created"
  }

getTextValue doc name = fromMaybe "" $ doc !? name
getDoubleValue doc name = fromMaybe 0.0 $ doc !? name
getIntValue doc name = fromMaybe 0 $ doc !? name


data JinbiItemType = JinbiItemType {
  jbTypeName :: Text,
  jbParentTypeName :: Text,
  jbTypeId :: Int
  } deriving Show

jinbiItemTypeToDocument t = ["tn" =: jbTypeName t,
                             "ptn" =: jbParentTypeName t,
                             "cat_id" =: jbTypeId t]

jinbiItemTypeFromDocument doc = JinbiItemType {
  jbTypeName = getTextValue doc "tn",
  jbParentTypeName = getTextValue doc "ptn",
  jbTypeId = getIntValue doc "cat_id"
  }

data JinbiItem = JinbiItem {
  jbiTitle :: Text,
  jbiId :: Text,
  jbiOrigPrice :: Text,
  jbiNewPrice :: Text,
  jbiCoin :: Text,
  jbiDiscount :: Double,
  jbiSales :: Text,
  jbiPicUrl :: Text,
  jbiTBKUrl :: Maybe Text,
  jbiType :: JinbiItemType
  } deriving Show

instance Eq JinbiItem where
  j1 == j2 = jbiId j1 == jbiId j2

instance Ord JinbiItem where
  compare j1 j2 = compare (jbiId j1) (jbiId j2)

instance TBItem JinbiItem where
  itemTitle = jbiTitle
  itemId = jbiId
  itemTBKUrl = jbiTBKUrl
  itemUrl a = case jbiTBKUrl a of
    Just u -> u
    Nothing -> itemUrlForId $ jbiId a
  setTBKUrl a u = a {jbiTBKUrl = u}
  itemOrigPrice = jbiOrigPrice
  itemNewPrice = jbiNewPrice
  toDocument = jinbiItemToDocument
  fromDocument = jinbiItemFromDocument

jinbiItemToDocument j = ["title" =: jbiTitle j,
                         "item_id" =: jbiId j,
                         "orig_price" =: jbiOrigPrice j,
                         "new_price" =: jbiNewPrice j,
                         "coin" =: jbiCoin j,
                         "discount" =: jbiDiscount j,
                         "sales" =: jbiSales j,
                         "pic_url" =: jbiPicUrl j,
                         "tbk_url" =: jbiTBKUrl j,
                         "jb_type" =: jinbiItemTypeToDocument (jbiType j)]

jinbiItemFromDocument doc = JinbiItem {
  jbiTitle = getTextValue doc "title",
  jbiId = getTextValue doc "item_id",
  jbiOrigPrice = getTextValue doc "orig_price",
  jbiNewPrice = getTextValue doc "new_price",
  jbiCoin = getTextValue doc "coin",
  jbiDiscount = getDoubleValue doc "discount",
  jbiSales = getTextValue doc "sales",
  jbiPicUrl = getTextValue doc "pic_url",
  jbiTBKUrl = doc !? "tbk_url",
  jbiType = jinbiItemTypeFromDocument $ fromJust $ doc !? "jb_type"
  }


data TeJiaItem = TeJiaItem {
  tjTitle :: Text,
  tjId :: Text,
  tjOrigPrice :: Text,
  tjNewPrice :: Text,
  tjSales :: Text,
  tjPicUrl :: Text,
  tjTBKUrl :: Maybe Text,
  tjType :: Text
  } deriving Show

instance Eq TeJiaItem where
  t1 == t2 = tjId t1 == tjId t2

instance Ord TeJiaItem where
  compare t1 t2 = compare (tjId t1) (tjId t2)

instance TBItem TeJiaItem where
  itemTitle = tjTitle
  itemId = tjId
  itemTBKUrl = tjTBKUrl
  itemUrl a = case tjTBKUrl a of
    Just u -> u
    Nothing -> itemUrlForId $ tjId a
  setTBKUrl a u = a {tjTBKUrl = u}
  itemOrigPrice = tjOrigPrice
  itemNewPrice = tjNewPrice
  toDocument = tejiaItemToDocument
  fromDocument = tejiaItemFromDocument

tejiaItemToDocument t = ["title" =: tjTitle t,
                         "item_id" =: tjId t,
                         "orig_price" =: tjOrigPrice t,
                         "new_price" =: tjNewPrice t,
                         "sales" =: tjSales t,
                         "pic_url" =: tjPicUrl t,
                         "tbk_url" =: tjTBKUrl t,
                         "type" =: tjType t]
tejiaItemFromDocument doc = TeJiaItem {
  tjTitle = getTextValue doc "title",
  tjId = getTextValue doc "item_id",
  tjOrigPrice = getTextValue doc "orig_price",
  tjNewPrice = getTextValue doc "new_price",
  tjSales = getTextValue doc "sales",
  tjPicUrl = getTextValue doc "pic_url",
  tjTBKUrl = doc !? "tbk_url",
  tjType = getTextValue doc "type"
  }


data AdBanner = AdBanner {
  adName :: Text,
  adUrl :: Text,
  adPicUrl :: Text
  } deriving Show

adToDocument ad = ["name" =: adName ad,
                   "url" =: adUrl ad,
                   "pic_url" =: adPicUrl ad]
adFromDocument doc = AdBanner {
  adName = getTextValue doc "name",
  adUrl = getTextValue doc "url",
  adPicUrl = getTextValue doc "pic_url"
  }
