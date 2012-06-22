{-# LANGUAGE BangPatterns #-}

module JinbiPage where

import Text.HTML.TagSoup
import qualified Data.Text as T
import Data.Maybe

import Model

tags2JinbiItems :: JinbiItemType -> [Tag String] -> IO ([JinbiItem], Bool)
tags2JinbiItems cat tags = do items <- mapM (tags2JinbiItem cat) $ partitions (~== "<a>") $ takeWhile (~/= "</div>") $ dropWhile (~/= "<div class='items clearfix'>") tags
                              let !pageTags = takeWhile (~/= "<a class='page-next'>") $ dropWhile (~/= "<span class='page-cur'>") tags
                                  hasNext = not $ null $ dropWhile (~/= "<a>") pageTags
                              return (items, hasNext)


tags2JinbiItem cat tags = return $ JinbiItem title id origPrice newPrice coin disc sale pic Nothing cat
  where !atags = dropWhile (~/= "<a>") tags
        !atag = if null atags
                then TagOpen "a" [("href","")]
                else head atags
        !id = idFromUrl $ T.pack $ fromAttrib "href" atag
        !imgTags = dropWhile (~/= "<img>") tags
        !imgTag = if null imgTags
                  then TagOpen "img" [("title",""), ("src","")]
                  else head imgTags
        !title = T.pack $ fromAttrib "title" imgTag
        !pic = T.pack $ fromAttrib "src" imgTag
        !origPrice = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<del>") tags
        !newPrice = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<em>") tags
        !coin = coinFromStr $ fromTagText $ tagFromTags $ dropWhile (~/= "</em>") tags
        !disc = read $ decimalFromStr $ fromTagText $ tagFromTags $ dropWhile (~/= "<span class='discount'>") tags
        !sale = T.pack $ fromTagText $ tagFromTags $ dropWhile (~/= "<span class='salescount'>") tags
        tagFromTags ts = if null ts
                         then TagText ""
                         else if null $ tail ts
                              then TagText ""
                              else head $ tail ts
        decimalFromStr s = if null s
                           then "0.0"
                           else if null $ init s
                                then "0.0"
                                else tail $ init s
        coinFromStr s = let t = T.pack s
                        in if T.null t
                           then T.pack ""
                           else if T.head t == '+'
                                then T.tail t
                                else t

idFromUrl url = if T.null url || not (T.pack "?" `T.isInfixOf` url)
                then T.pack ""
                else fromMaybe (T.pack "") itemId
  where query = T.tail $ T.dropWhile (/= '?') url
        qs = T.split (== '&') query
        dic = map (\q -> let a = T.split (== '=') q in (head a, head $ tail a)) qs
        itemId = case lookup (T.pack "id") dic of
          Nothing -> lookup (T.pack "item_id") dic
          Just w -> Just w
