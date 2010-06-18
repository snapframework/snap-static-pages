{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Snap.StaticPages.Internal.Handlers ( serveStaticPages ) where

import             Control.Concurrent.MVar
import             Control.Exception (assert)
import "monads-fd" Control.Monad.Reader
import qualified   Data.ByteString.Char8 as B
import qualified   Data.ByteString.Lazy.Char8 as L
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.UTF8 as UTF8
import             Data.List
import qualified   Data.Map as Map
import             Data.Maybe
import             Snap.Types
import             Snap.Util.FileServe
import qualified   Text.Atom.Feed as Atom
import qualified   Text.Atom.Feed.Export as Atom
import             Text.Templating.Heist
import qualified   Text.XML.Expat.Tree as X
import qualified   Text.XML.Light.Output as XML
------------------------------------------------------------------------------
import           Snap.StaticPages.Internal.Post
import           Snap.StaticPages.Internal.Time
import           Snap.StaticPages.Internal.Types
import qualified Snap.StaticPages.Internal.Util.ExcludeList as EL


serveStaticPages :: MVar StaticPagesState
                 -> Snap ()
serveStaticPages = (serveStaticPages' =<<) . liftIO . readMVar


type StaticPagesMonad a = ReaderT StaticPagesState Snap a
type StaticPagesHandler = StaticPagesMonad ()


serveStaticPages' :: StaticPagesState -> Snap ()
serveStaticPages' state = method GET $ do

    let cm = staticPagesPostMap state
    paths <- liftM (B.split '/' . rqPathInfo) getRequest

    runReaderT (serve [] paths cm) state

  where
    --------------------------------------------------------------------------
    serve :: [ByteString] -> [ByteString] -> ContentMap -> StaticPagesHandler
    serve soFar paths content = do
        case paths of
          []      -> serveIndex soFar content
          (a:[])  -> serveFile soFar a content
          (a:b)   -> serveDir soFar a b content


    --------------------------------------------------------------------------
    serveFile :: [ByteString] -> ByteString -> ContentMap -> StaticPagesHandler
    serveFile soFar a content = do
        if a == "feed.xml" then
            serveFeed soFar content
          else
            maybe mzero
                  (\f -> case f of
                           (ContentStatic fp)     -> lift $ fileServeSingle fp
                           (ContentPost post)     -> servePost (soFar ++ [a]) post
                           (ContentDirectory _ d) -> serveIndex (soFar ++ [a]) d)
                  (Map.lookup a content)


    --------------------------------------------------------------------------
    serveDir :: [ByteString]
             -> ByteString
             -> [ByteString]
             -> ContentMap
             -> StaticPagesHandler
    serveDir soFar d rest content = do
        let mbD = Map.lookup d content

        maybe mzero
              (\f -> case f of
                       (ContentDirectory _ mp) -> serve (soFar ++ [d]) rest mp
                       _                       -> mzero)
              mbD


------------------------------------------------------------------------------
firstM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstM []     = return Nothing
firstM (x:xs) = do
    m <- x
    maybe (firstM xs) (return . Just) m



------------------------------------------------------------------------------
-- | Take a path list @[\"foo\",\"bar\",\"baz\"]@ and turn it into
-- @\"foo/bar/baz\"@
listToPath :: [ByteString] -> ByteString
listToPath l = B.concat $ ("/": intersperse "/" l)


------------------------------------------------------------------------------
-- | Given a path to our post, try to find the most specific template. First
-- we'll see if there's a template specifically matching our post, and barring
-- that we'll run the "post" template
runTemplateForPost :: [ByteString]   -- ^ path to the post, relative
                                     -- to the \"content\/\" directory;
                                     -- if the file is in
                                     -- \"@content\/foo\/bar\/baz.md@\" then
                                     -- this list will contain
                                     -- @["foo", "bar", "baz"]@.
                   -> TemplateState Snap
                   -> StaticPagesMonad (Maybe ByteString)
runTemplateForPost pathList templates = do
    assert (not $ null pathList) (return ())

    lift $ firstM $ map (renderTemplate templates) templatesToSearch

  where
    -- if requested "foo/bar/baz", then containingDirs contains
    -- [["foo","bar"], ["foo"], []]
    containingDirs    = tail . reverse . inits $ pathList

    templatesToSearch = (listToPath pathList :
                         map (\d -> listToPath $ d ++ ["post"]) containingDirs)



------------------------------------------------------------------------------
runTemplateForDirectory :: [ByteString]   -- ^ path to the post, relative
                                          -- to the \"content\/\" directory;
                                          -- if the file is in
                                          -- \"@content\/foo\/bar\/baz.md@\" then
                                          -- this list will contain
                                          -- @["foo", "bar", "baz"]@.
                   -> TemplateState Snap
                   -> StaticPagesMonad (Maybe ByteString)
runTemplateForDirectory pathList templates = do
    assert (not $ null pathList) (return ())

    lift $ renderTemplate templates (listToPath $ pathList ++ ["index"])


------------------------------------------------------------------------------
showEC :: Atom.EntryContent -> ByteString
showEC (Atom.TextContent s)  = UTF8.fromString s
showEC (Atom.HTMLContent s)  = UTF8.fromString s
showEC _                     = ""

showTC :: Atom.TextContent -> ByteString
showTC (Atom.TextString s)  = UTF8.fromString s
showTC (Atom.HTMLString s)  = UTF8.fromString s
showTC _                     = ""

showPerson :: Atom.Person -> ByteString
showPerson (Atom.Person name _ email _) =
    UTF8.fromString $ name ++ em
  where
    em = maybe "" (\e -> " <" ++ e ++ ">") email



bindPostAttrs :: (MonadIO m) =>
                 StaticPagesState
              -> TemplateState Snap
              -> Post
              -> m (TemplateState Snap)
bindPostAttrs state ts post@(Post p) = do
    let title = B.pack $ concat
                  [ getTextContent . Atom.feedTitle . staticPagesFeedInfo $ state
                  , (let s = getTextContent $ Atom.entryTitle p
                     in if null s then "" else ": " ++ s)
                  ]

    e <- liftIO $ parseDoc bodyBS

    let body = either (\s -> [X.Text $
                              B.pack $
                              "error parsing pandoc output: " ++ s])
                      snd
                      e

    e2 <- liftIO $ parseDoc summaryBS

    let summary = either (\s -> [X.Text $
                                 B.pack $
                                 "error parsing pandoc output: " ++ s])
                         snd
                         e2

    return $ bindSplice "post:content" (return body) $
             bindSplice "post:summary" (return summary) $
             bindSplice "pageTitle"    (return [X.Text title]) ts'

  where
    authors = B.intercalate ", " (map showPerson $ Atom.entryAuthors p)

    ts' = bindStrings [ ("post:id"      , url                            )
                      , ("post:date"    , B.pack $ friendlyTime $
                                          getPostTime post               )
                      , ("post:url"     , url                            )
                      , ("post:title"   , showTC $ Atom.entryTitle p     )
                      , ("post:authors" , authors                        ) ] ts

    url = B.pack $ Atom.entryId p

    bodyBS = showEC $ fromMaybe (Atom.TextContent "") $ Atom.entryContent p
    summaryBS = showTC $ fromMaybe (Atom.HTMLString "") $ Atom.entrySummary p



getTextContent :: Atom.TextContent -> String
getTextContent (Atom.TextString s) = s
getTextContent (Atom.HTMLString s) = s
getTextContent _                   = undefined -- don't support that yet



servePost :: [ByteString] -> Post -> StaticPagesHandler
servePost soFar post = do
    state  <- ask

    let xformTmpl = staticPagesExtraTmpl state
    let templatesOrig = staticPagesTemplates state
    templates <- (lift $ xformTmpl templatesOrig) >>= \t ->
                 bindPostAttrs state t post

    mb <- runTemplateForPost soFar templates

    -- no post template? mzero out.
    b <- maybe (lift mzero) return mb

    lift $ modifyResponse $ setContentType "text/html; charset=utf-8"
    lift $ writeBS b


------------------------------------------------------------------------------
getContentTitle :: ContentItem -> String
getContentTitle (ContentPost (Post p)) = getTextContent . Atom.entryTitle $ p
getContentTitle _                      = ""


------------------------------------------------------------------------------
serveIndex :: [ByteString] -> ContentMap -> StaticPagesHandler
serveIndex soFar content = do
    state <- ask

    let xformTmpl = staticPagesExtraTmpl state
    let templatesOrig = staticPagesTemplates state
    templates <- lift $ xformTmpl templatesOrig

    let excludes' =  staticPagesFeedExcludes state
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let alpha     =  alphabeticalPosts excludes content
    let chron     =  chronologicalPosts excludes content
    let rchron    =  reverseChronologicalPosts excludes content
    let recent    =  take 5 rchron

    let runPosts = loopThru state
    let spliceMap = [ ("posts:alphabetical"        , runPosts alpha)
                    , ("posts:chronological"       , runPosts chron)
                    , ("posts:reverseChronological", runPosts rchron)
                    , ("posts:recent"              , runPosts recent) ]

    let tmpl' = bindSplices spliceMap templates

    let mbPost  = Map.lookup "index" content
    let baseURL = B.pack $ staticPagesBaseURL state
    let fdPath  = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
    let feedURL = B.unpack $ B.concat [baseURL, "/", fdPath]


    let title = concat
                  [ getTextContent . Atom.feedTitle . staticPagesFeedInfo $ state
                  , maybe ""
                          (\x -> let s = getContentTitle x
                                 in if null s then "" else ": " ++ s)
                          mbPost
                  ]



    tmpl'' <- case mbPost of
                (Just (ContentPost p)) -> do
                    let bodyBS = showEC $
                                 fromMaybe (Atom.TextContent "") $
                                 Atom.entryContent (unPost p)
                    e <- liftIO $ parseDoc bodyBS

                    let body =
                          either (\s -> [X.Text $
                                         B.pack $
                                         "error parsing pandoc output: " ++ s])
                           snd
                           e
                    return $ bindSplice "index:content" (return body) tmpl'

                _ -> return tmpl'


    let autoDiscovery' = X.mkElement "link"
                                [ ("rel" , "alternate"           )
                                , ("type", "application/atom+xml")
                                , ("href", B.pack $ feedURL      ) ]
                                []

    let autoDiscovery = if EL.matchList soFar excludes
                          then []
                          else [autoDiscovery']


    let tmpl''' = bindSplices [ ("pageTitle", return [X.Text $ B.pack title])
                              , ("feed:autoDiscoveryLink", return autoDiscovery) ]
                              tmpl''


    mb <- runTemplateForDirectory soFar tmpl'''

    -- no template? barf.
    b  <- maybe (lift mzero) return mb

    lift $ modifyResponse $ setContentType "text/html; charset=utf-8"
    lift $ writeBS b


  where
    doOne :: StaticPagesState -> [Node] -> Post -> Splice Snap
    doOne state perEach post = do
        ts  <- getTS
        ts' <- bindPostAttrs state ts post
        putTS ts'
        runNodeList perEach


    loopThru :: StaticPagesState -> [Post] -> Splice Snap
    loopThru state posts = do
        node <- getParamNode
        ts   <- getTS

        -- here we take the tag's children as a bit of markup to be run for
        -- every post. We'll bind a fresh copy of the post for each run.
        let perEach = X.getChildren node

        allNodes <- liftM concat $ mapM (doOne state perEach) posts

        stopRecursion
        restoreTS ts
        return allNodes


------------------------------------------------------------------------------
addSiteURL :: String -> Post -> Post
addSiteURL siteURL (Post p) =
    Post $ p {Atom.entryId = concat [siteURL, Atom.entryId p]}


------------------------------------------------------------------------------
serveFeed :: [ByteString] -> ContentMap -> StaticPagesHandler
serveFeed soFar content = do
    state <- ask

    let excludes' =  staticPagesFeedExcludes state
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let siteURL'  =  staticPagesSiteURL state
    let posts     =  map (addSiteURL siteURL') $ recentPosts excludes content 5

    let xformTmpl = staticPagesExtraTmpl state
    let templatesOrig = staticPagesTemplates state
    templates <- lift $ xformTmpl templatesOrig

    let hasT = hasTemplate (listToPath $ soFar ++ ["index"]) templates

    -- if there's no index template for a 
    when (null posts || not hasT) $ lift mzero

    let siteURL  = B.pack siteURL'
    let baseURL  = B.pack $ staticPagesBaseURL state
    let fdPath   = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
    let feedURL  = B.unpack $ B.concat
                            $ [siteURL, baseURL, "/", fdPath]
    let baseFeed = staticPagesFeedInfo state

    let feed     = baseFeed {
                        Atom.feedId      = feedURL
                      , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                      , Atom.feedEntries = map unPost posts
                      , Atom.feedUpdated = Atom.entryUpdated $ unPost (head posts)
                      }

    lift $ modifyResponse $ setContentType "application/atom+xml"
    lift $ writeLBS $ L.pack $ XML.showElement $ Atom.xmlFeed feed

{-
    hasTemplate   <- lift $ liftM isJust $ findTemplateForDirectory soFar

    if null posts || not hasTemplate
      then mzero
      else do
        let siteURL  = B.pack siteURL'
        let baseURL  = B.pack $ staticPagesBaseURL state
        let fdPath     = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
        let feedURL  = B.unpack $ B.concat
                                $ [siteURL, baseURL, "/", fdPath]
        let baseFeed = staticPagesFeedInfo state

        let feed     = baseFeed {
                            Atom.feedId      = feedURL
                          , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                          , Atom.feedEntries = map unPost posts
                          , Atom.feedUpdated = Atom.entryUpdated $ unPost (head posts)
                          }
        return $ toResponse feed
-}
