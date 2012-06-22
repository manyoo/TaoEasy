{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Text hiding (map, foldl')
import Data.Time.Clock
import Data.Maybe

import Data.Bson

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
