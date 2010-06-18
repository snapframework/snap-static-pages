module Snap.StaticPages.Internal.Util.ExcludeList
  ( empty
  , fromPathList
  , fromPath
  , matches
  , matchList
  , descend
  , ExcludeList )

where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
import           Data.List (isPrefixOf)

newtype ExcludeList = ExcludeList [[ByteString]]
  deriving Show


------------------------------------------------------------------------------
empty :: ExcludeList
empty = ExcludeList []

------------------------------------------------------------------------------
fromPath :: ByteString -> [ByteString]
fromPath path' = B.split '/' path
  where
    path  = stripSuffix '/' $ stripPrefix '/' path'


------------------------------------------------------------------------------
fromPathList :: ByteString -> ExcludeList
fromPathList = ExcludeList .
                 filter (not . null) .
                 map (fromPath . trim) .
                 B.split ','


------------------------------------------------------------------------------
matches :: ByteString           -- ^ path
        -> ExcludeList          -- ^ exclusion list
        -> Bool
matches path el = matchList (fromPath path) el


------------------------------------------------------------------------------
matchList :: [ByteString]       -- ^ path list
          -> ExcludeList        -- ^ exclusion list
          -> Bool
matchList pathList el = match pathList el
  where
    match []     _                 = False
    match (p:[]) (ExcludeList l)   = m p l
    match (p:q)  l@(ExcludeList e) = m p e || match q (descend p l)

    m p l = any (== [p]) l


------------------------------------------------------------------------------
descend :: ByteString -> ExcludeList -> ExcludeList
descend p e@(ExcludeList el) = if B.null p then e else ExcludeList l
  where
    l        = filter (not . null) $ map (drop 1) matching
    matching = filter (isPrefixOf [p]) el


------------------------------------------------------------------------------
trim :: ByteString -> ByteString
trim s = t
  where
    s'    = B.dropWhile isSpace s
    (t,_) = B.spanEnd isSpace s'


stripPrefix :: Char -> ByteString -> ByteString
stripPrefix c s =
    if (B.pack [c] `B.isPrefixOf` s) then B.drop 1 s else s


stripSuffix :: Char -> ByteString -> ByteString
stripSuffix c s =
    if (B.pack [c] `B.isSuffixOf` s) then B.init s else s


