{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Snap.StaticPages.Internal.Handlers ( serveStaticPages ) where

import           Control.Arrow
import           Control.Exception (assert)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Snap.Util.FileServe as FS
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import           Heist
import           Heist.Interpreted
import           Heist.Splices
import qualified Text.XmlHtml as X
import qualified Text.XML.Light.Output as XML
------------------------------------------------------------------------------
import           Snap.StaticPages.Internal.Post
import           Snap.StaticPages.Internal.Time
import           Snap.StaticPages.Internal.Types
import qualified Snap.StaticPages.Internal.Util.ExcludeList as EL


type StaticPagesMonad b a = Handler b StaticPages a
type StaticPagesHandler b = StaticPagesMonad b ()


serveStaticPages :: HasHeist b => Handler b StaticPages ()
serveStaticPages = method GET $ do
    st <- get
    let cm = staticPagesPostMap st
    rq <- getRequest
    let paths = EL.fromPath $ rqPathInfo rq
    let soFar = EL.fromPath $ rqContextPath rq

    serve soFar paths cm

  where
    --------------------------------------------------------------------------
    serve :: HasHeist b => [ByteString] -> [ByteString] -> ContentMap
          -> StaticPagesHandler b
    serve soFar paths content =
        case paths of
          []      -> serveIndex soFar content
          (a:[])  -> serveFile soFar a content
          (a:b)   -> serveDir soFar a b content


    --------------------------------------------------------------------------
    serveFile :: HasHeist b => [ByteString] -> ByteString -> ContentMap
              -> StaticPagesHandler b
    serveFile soFar a content =
        if a == "feed.xml" then
            serveFeed soFar content
          else
            maybe mzero
                  (\f -> case f of
                           (ContentStatic fp)     -> FS.serveFile fp
                           (ContentPost post)     -> servePost (soFar ++ [a]) post
                           (ContentDirectory _ d) -> serveIndex (soFar ++ [a]) d)
                  (Map.lookup a content)


    --------------------------------------------------------------------------
    serveDir :: HasHeist b
             => [ByteString]
             -> ByteString
             -> [ByteString]
             -> ContentMap
             -> StaticPagesHandler b
    serveDir soFar d rest content = do
        let mbD = Map.lookup d content

        maybe mzero
              (\f -> case f of
                       (ContentDirectory _ mp) -> serve (soFar ++ [d]) rest mp
                       _                       -> mzero)
              mbD


------------------------------------------------------------------------------
-- | Take a path list @[\"foo\",\"bar\",\"baz\"]@ and turn it into
-- @\"foo/bar/baz\"@
listToPath :: [ByteString] -> ByteString
listToPath l = B.concat ("/": intersperse "/" l)


------------------------------------------------------------------------------
-- | Given a path to our post, try to find the most specific template. First
-- we'll see if there's a template specifically matching our post, and barring
-- that we'll run the "post" template
runTemplateForPost :: HasHeist b
                   => [ByteString]   -- ^ path to the post, relative
                                     -- to the \"content\/\" directory;
                                     -- if the file is in
                                     -- \"@content\/foo\/bar\/baz.md@\" then
                                     -- this list will contain
                                     -- @["foo", "bar", "baz"]@.
                   -> StaticPagesMonad b ()
runTemplateForPost pathList = do
    assert (not $ null pathList) (return ())

    msum $ flip map templatesToSearch $ \t ->
        renderAs "text/html; charset=utf-8" t

  where
    -- if requested "foo/bar/baz", then containingDirs contains
    -- [["foo","bar"], ["foo"], []]
    containingDirs    = tail . reverse . inits $ pathList

    templatesToSearch = listToPath pathList :
                         map (\d -> listToPath $ d ++ ["post"]) containingDirs



------------------------------------------------------------------------------
showEC :: Atom.EntryContent -> Text
showEC (Atom.TextContent s)  = T.pack s
showEC (Atom.HTMLContent s)  = T.pack s
showEC _                     = ""

showTC :: Atom.TextContent -> Text
showTC (Atom.TextString s)  = T.pack s
showTC (Atom.HTMLString s)  = T.pack s
showTC _                     = ""

showPerson :: Atom.Person -> Text
showPerson (Atom.Person name _ email _) =
    T.pack $ name ++ em
  where
    em = maybe "" (\e -> " <" ++ e ++ ">") email



postAttrs :: (Monad m) => StaticPages -> Post -> [(Text, Splice m)]
postAttrs st post@(Post p) =
    [ ("post:content" , return body)
    , ("post:summary" , return summary)
    , ("pageTitle"    , return [X.TextNode title])
    , ("post:id"      , textSplice url)
    , ("post:date"    , textSplice $ T.pack $ friendlyTime $
                        getPostTime post)
    , ("post:url"     , textSplice $ url)
    , ("post:title"   , textSplice $ showTC $ Atom.entryTitle p)
    , ("post:authors" , textSplice $ authors) ]
  where
    title = T.pack $ concat
              [ getTextContent . Atom.feedTitle . staticPagesFeedInfo $ st
              , let s = getTextContent $ Atom.entryTitle p
                 in if null s then "" else ": " ++ s
              ]

    e = X.parseHTML "" bodyBS

    body = either (\s -> [X.TextNode $
                          T.pack $
                          "error parsing pandoc output: " ++ s])
                  X.docContent
                  e

    e2 = X.parseHTML "" summaryBS

    summary = either (\s -> [X.TextNode $
                             T.pack $
                             "error parsing pandoc output: " ++ s])
                     X.docContent
                     e2

    authors = T.intercalate ", " (map showPerson $ Atom.entryAuthors p)

    url = T.pack $ Atom.entryId p

    bodyBS = T.encodeUtf8 $ showEC $ fromMaybe (Atom.TextContent "") $
             Atom.entryContent p
    summaryBS = T.encodeUtf8 $ showTC $ fromMaybe (Atom.HTMLString "") $
                Atom.entrySummary p


getTextContent :: Atom.TextContent -> String
getTextContent (Atom.TextString s) = s
getTextContent (Atom.HTMLString s) = s
getTextContent _                   = undefined -- don't support that yet



servePost :: HasHeist b => [ByteString] -> Post -> StaticPagesHandler b
servePost soFar post = do
    st <- get
    withSplices (map id $ postAttrs st post) $ runTemplateForPost soFar


------------------------------------------------------------------------------
getContentTitle :: ContentItem -> String
getContentTitle (ContentPost (Post p)) = getTextContent . Atom.entryTitle $ p
getContentTitle _                      = ""


------------------------------------------------------------------------------
serveIndex :: HasHeist b => [ByteString] -> ContentMap -> StaticPagesHandler b
serveIndex soFar content = do
    st <- get

    let excludes' =  staticPagesFeedExcludes st
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let alpha     =  alphabeticalPosts excludes content
    let chron     =  chronologicalPosts excludes content
    let rchron    =  reverseChronologicalPosts excludes content
    let recent    =  take 5 rchron

    let runPosts = loopThru st
    let splices1 = [ ("posts:alphabetical"        , runPosts alpha)
                   , ("posts:chronological"       , runPosts chron)
                   , ("posts:reverseChronological", runPosts rchron)
                   , ("posts:recent"              , runPosts recent) ]

    let mbPost  = Map.lookup "index" content
    let baseURL = B.pack $ staticPagesBaseURL st
    let fdPath  = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
    let feedURL = B.unpack $ B.concat [baseURL, "/", fdPath]


    let title = concat
                  [ getTextContent . Atom.feedTitle . staticPagesFeedInfo $ st
                  , maybe ""
                          (\x -> let s = getContentTitle x
                                 in if null s then "" else ": " ++ s)
                          mbPost
                  ]



    let splices2 = case mbPost of
            (Just (ContentPost p)) ->
                let bodyBS = T.encodeUtf8 $ showEC $
                             fromMaybe (Atom.TextContent "") $
                             Atom.entryContent (unPost p)
                    e = X.parseHTML "" bodyBS

                    body =
                      either (\s -> [X.TextNode $
                                     T.pack $
                                     "error parsing pandoc output: " ++ s])
                       X.docContent
                       e
                in ("index:content", return body) : splices1

            _ -> splices1


    let autoDiscovery' = X.Element "link"
                                [ ("rel" , "alternate"           )
                                , ("type", "application/atom+xml")
                                , ("href", T.pack feedURL      ) ]
                                []

    let autoDiscovery = if EL.matchList soFar excludes
                          then []
                          else [autoDiscovery']


    let splices3 = ("pageTitle", return [X.TextNode $ T.pack title]) :
                   ("feed:autoDiscoveryLink", return autoDiscovery) : splices2

    let tpath = listToPath $ soFar ++ ["index"]

    withSplices splices3 $ renderAs "text/html; charset=utf-8" tpath
                 


  where
    loopThru :: StaticPages -> [Post] -> SnapletISplice b
    loopThru st posts = do
        node <- getParamNode

        -- here we take the tag's children as a bit of markup to be run for
        -- every post. We'll bind a fresh copy of the post for each run.
        let perEach' = X.childNodes node

        -- the exception to this is when there are no posts; then we fetch the
        -- <no-posts> tag, otherwise we filter it out.
        let (noPosts,perEach) =
                partition (\x -> X.tagName x == Just "no-posts") perEach'

        let noPost = if null noPosts then [] else X.childNodes $ head noPosts

        let func post = runChildrenWith (postAttrs st post)
        allNodes <-
            if null posts
              then runNodeList noPost
              else mapSnapletSplices func posts

        stopRecursion
        return allNodes

mapSnapletSplices :: (a -> SnapletISplice b)
                  -- ^ Splice generating function
                  -> [a]
                  -- ^ List of items to generate splices for
                  -> SnapletISplice b
                  -- ^ The result of all splices concatenated together.
mapSnapletSplices f vs = liftM concat $ mapM f vs

------------------------------------------------------------------------------
addSiteURL :: String -> Post -> Post
addSiteURL siteURL (Post p) =
    Post $ p {Atom.entryId = concat [siteURL, Atom.entryId p]}


------------------------------------------------------------------------------
serveFeed :: HasHeist b => [ByteString] -> ContentMap -> StaticPagesHandler b
serveFeed soFar content = do
    st <- get

    let excludes' =  staticPagesFeedExcludes st
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let siteURL'  =  staticPagesSiteURL st
    let posts     =  map (addSiteURL siteURL') $ recentPosts excludes content 5

    templates <- withHeistState id

    let hasT = hasTemplate (listToPath $ soFar ++ ["index"]) templates

    -- if there's no index template for a 
    when (null posts || not hasT) mzero

    let siteURL  = B.pack siteURL'
    let baseURL  = B.pack $ staticPagesBaseURL st
    let fdPath   = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
    let feedURL  = B.unpack $ B.concat [siteURL, baseURL, "/", fdPath]
    let baseFeed = staticPagesFeedInfo st

    let feed     = baseFeed {
                        Atom.feedId      = feedURL
                      , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                      , Atom.feedEntries = map unPost posts
                      , Atom.feedUpdated = Atom.entryUpdated $ unPost (head posts)
                      }

    modifyResponse $ setContentType "application/atom+xml"
    writeLBS $ LT.encodeUtf8 $ LT.pack $ XML.showElement $ Atom.xmlFeed feed

