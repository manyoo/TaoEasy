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

