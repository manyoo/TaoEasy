{-# LANGUAGE BangPatterns #-}

module TeJiaPage where

import Text.HTML.TagSoup
import qualified Data.Text as T
import Data.Maybe

import Model

tags2TeJiaItems :: (T.Text, T.Text) -> [Tag String] -> IO ([TeJiaItem], Bool)
tags2TeJiaItems cat tags = do items <- mapM (tags2TeJiaItem cat) $ partitions (~== "<li>") $ takeWhile (~/= "<div class='page'>") $ dropWhile (~/= "<ul class='list'>") tags
                              let !pageTags = takeWhile (~/= "<a class='page-next'>") $ dropWhile (~/= "<span class='page-cur'>") tags
                                  !hasNext = not $ null $ dropWhile (~/= "<a>") pageTags
                              return (items, hasNext)

tags2TeJiaItem (catName, _) tags = return $ TeJiaItem title id origPrice newPrice sale picUrl Nothing catName
  where !atags = dropWhile (~/= "<a>") tags
        !atag = if null atags
                then TagOpen "a" [("href", "")]
                else head atags
        !id = idFromUrl $ T.pack $ fromAttrib "href" atag
        !imgTags = dropWhile (~/= "<img>") tags
        !imgTag = if null imgTags
                  then TagOpen "img" [("data-ks-lazyload","")]
                  else head imgTags
        !picUrl = T.pack $ fromAttrib "data-ks-lazyload" imgTag
        !titleTags = dropWhile (~/= "<a>") imgTags
        !titleTag = if null titleTags
                    then TagText ""
                    else tagFromTags titleTags
        !title = T.pack $ fromTagText titleTag
        !origPrice = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<del>") tags
        !newPrice = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<strong>") tags
        !sale = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<b>") tags
        tagFromTags ts = if null ts
                         then TagText ""
                         else if null $ tail ts
                              then TagText ""
                              else head $ tail ts

idFromUrl url = if T.null url || not (T.pack "?" `T.isInfixOf` url)
                then T.pack ""
                else fromMaybe (T.pack "") itemId
  where query = T.tail $ T.dropWhile (/= '?') url
        qs = T.split (== '&') query
        dic = map (\q -> let a = T.split (== '=') q in (head a, head $ tail a)) qs
        itemId = case lookup (T.pack "id") dic of
          Nothing -> lookup (T.pack "itemId") dic
          Just w -> Just w
