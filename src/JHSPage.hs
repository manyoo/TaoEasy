{-# LANGUAGE BangPatterns #-}

module JHSPage where

import Text.HTML.TagSoup
import qualified Data.Text as T
import Data.Maybe

import Model

tags2JHSItems :: [Tag String] -> [Maybe JHSItem]
tags2JHSItems = map tags2JHSItem . partitions (~== "<li>") . takeWhile (~/= "<div class='ju-pagination'>") . dropWhile (~/= "<ul class='clearfix'>")

tags2JHSItem tags = if hasStrong
                    then Just $ JHSItem itemId title (T.pack "") Nothing origPrice newPrice discount sold picUrl Nothing
                    else Nothing
  where !atag = head $ dropWhile (~/= "<a>") tags
        !title = T.pack $ fromAttrib "title" atag
        !itemId = idFromUrl $ T.pack $ fromAttrib "href" atag
        !imgTag = head $ dropWhile (~/= "<img>") tags
        pic1 = fromAttrib "data-ks-lazyload" imgTag
        !picUrl = if pic1 == ""
                  then T.pack $ fromAttrib "src" imgTag
                  else T.pack pic1
        !origPrice = priceFromStr $ fromTagText $ head $ drop 1 $ dropWhile (~/= "<del>") tags
        !newPrice = read $ fromTagText $ head $ drop 1 $ dropWhile (~/= "</em>") tags
        !disTags = drop 1 $ dropWhile (~/= "<strong>") tags
        !hasStrong = not $ null disTags
        !discount = if hasStrong
                    then T.pack $ fromTagText $ head $ disTags
                    else T.pack "-1"
        !soldTag = drop 2 $ dropWhile (~/= "<span class='buy-ed'>") tags
        !sold = if null soldTag
                then 0
                else read $ fromTagText $ head soldTag
        !priceFromStr = read . init

idFromUrl url = let query = T.tail $ T.dropWhile (/= '?') url
                    qs = T.split (== '&') query
                    dic = map (\q -> let a = T.split (== '=') q in (head a, head $ tail a)) qs
                    itemId = case lookup (T.pack "item_id") dic of
                      Nothing -> lookup (T.pack "itemId") dic
                      Just w -> Just w
                in fromMaybe (T.pack "") itemId


-- code for crazy weekend
crazyWeekJHSItemsFromTags :: [Tag String] -> [[Maybe JHSItem]]
crazyWeekJHSItemsFromTags = map getTagsFromSection . partitions (~== "<div class='pro clearfix'>") . takeWhile (~/= "<div class='bottom-bg'>") . dropWhile (~/= "<div class='pro clearfix'>")

getTagsFromSection = map tags2JHSItem . partitions (~== "<li>")
