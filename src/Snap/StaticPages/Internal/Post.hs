{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}

module Snap.StaticPages.Internal.Post
  ( getTimeStamp
  , parsePersons
  , collectPosts
  , recentPosts
  , chronologicalPosts
  , reverseChronologicalPosts
  , alphabeticalPosts
  , buildContentMap
  , setEntryId
  , setEntryTitle
  , setEntryUpdated
  , setEntryAuthors
  , setEntrySummary
  , setEntryHTMLContent
  , setEntryContributor
  , setEntryCategories
  , setEntryLinks
  , setEntryPublished
  , setEntryRights
  , setEntrySource
  , setEntryInReplyTo
  , setEntryInReplyTotal
  , setEntryAttrs
  , setEntryOther
  )
where

------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import "mtl"     Control.Monad.Error
import "mtl"     Control.Monad.Identity
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char
import qualified Data.ConfigFile as Cfg
import           Data.List
import           Data.List.Split
import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           Text.Atom.Feed
import           Text.Printf
import           Text.XML.Light
import           Text.Templating.Heist.Splices.Markdown

------------------------------------------------------------------------
import           Snap.StaticPages.Internal.Time
import           Snap.StaticPages.Internal.Types
import qualified Snap.StaticPages.Internal.Util.ExcludeList as EL
import           Snap.StaticPages.Internal.Util.ExcludeList (ExcludeList)
------------------------------------------------------------------------


getTimeStamp :: FilePath -> IO UTCTime
getTimeStamp file =
  (posixSecondsToUTCTime . realToFrac)
    <$> modificationTime
    <$> getFileStatus file


trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace


parsePersons :: String -> [Person]
parsePersons = map mkPerson . endBy ","
  where
    mkPerson s = Person x Nothing y []
      where
        (x,y) = parseOut s

    parseOut s = (trim a, mb c)
      where
        mb x   = if null x then Nothing else Just x
        (a,b') = span (/= '<') s
        b      = drop 1 b'
        (c,_)  = span (/= '>') b



parseHeaders :: String -> (Either Cfg.CPError Cfg.ConfigParser)
parseHeaders = Cfg.readstring Cfg.emptyCP


getKVP :: Cfg.ConfigParser -> String -> Maybe String
getKVP cp key = retval
  where
    e :: Either Cfg.CPError String
    e = runIdentity . runErrorT $ Cfg.get cp "DEFAULT" key
    retval = case e of Left _  -> Nothing
                       Right x -> Just x


headerTable :: [(String, String -> Post -> Post)]
headerTable = [ ("title",     setEntryTitle)
              , ("author",    setEntryAuthors)
              , ("authors" ,  setEntryAuthors)
              , ("summary",   setEntrySummary)
              , ("updated",   setEntryUpdated)
              , ("published", setEntryPublished . Just) ]


-- break the post apart at the header ender, strip the prefix chars
-- off of the header, return both
breakPost :: B.ByteString -> (B.ByteString, B.ByteString)
breakPost s = (B.unlines hdr, B.unlines body)
  where
    chomp x = if "| " `B.isPrefixOf` x
                then B.drop 2 x
                else if "|" `B.isPrefixOf` x
                   then B.drop 1 x
                   else x

    lns          = B.lines s
    (hdr', body) = span ("|" `B.isPrefixOf`) lns
    hdr          = chomp `map` hdr'


readPost :: String -> FilePath -> IO Post
readPost pId path = do
    !tz  <- getCurrentTimeZone
    !t   <- getTimeStamp path
    let !atm = formatAtomTime tz t

    !contents <- B.readFile path

    let (hdr,body) = breakPost contents

    let !hdrS = B.unpack hdr

    let !cfg = case parseHeaders hdrS of
                Left e -> error
                          $ printf "Couldn't parse headers from %s:\n%s"
                                   path (show e)
                Right r -> r

    let !post = foldl (\p (k,f) ->
                           case getKVP cfg k of
                             Nothing -> p
                             Just x  -> f x p)
                      (Post $ nullEntry pId (HTMLString "") atm)
                      headerTable

    mbPandocpath <- findExecutable "pandoc"

    pandocpath <- maybe
                    (throwIO (StaticPagesException "pandoc executable not found"))
                    return
                    mbPandocpath


    html <- pandocBS pandocpath body

    return $! setEntryHTMLContent (UTF8.toString html)
           $! setEntryLinks [ (nullLink pId) {
                                  linkRel = Just $ Left "alternate"
                              } ]
           $! post



collectPosts :: ExcludeList -> ContentMap -> [Post]
collectPosts el m = help el ("", ContentDirectory "" m)
  where
    -- don't count posts named "index" -- they're there to provide
    -- text for directory indices
    help :: ExcludeList -> (ByteString, ContentItem) -> [Post]
    help s (nm, ContentPost p) =
        if nm == "index" || EL.matches nm s then [] else [p]
    help s (nm, ContentDirectory _ cm) =
        if not $ EL.matches nm s
          then concatMap (help (EL.descend nm s)) $ Map.assocs cm
          else []
    help _  _ = []


recentPosts :: ExcludeList -> ContentMap -> Int -> [Post]
recentPosts sl m nposts =
    take nposts $ reverseChronologicalPosts sl m


chronologicalPosts :: ExcludeList -> ContentMap -> [Post]
chronologicalPosts sl m =
    sortBy cmp $ collectPosts sl m
  where
    pt = zonedTimeToUTC . getPostTime

    cmp a b = pt a `compare` pt b


reverseChronologicalPosts :: ExcludeList -> ContentMap -> [Post]
reverseChronologicalPosts = (reverse .) . chronologicalPosts


alphabeticalPosts :: ExcludeList -> ContentMap -> [Post]
alphabeticalPosts sl m = sortBy cmp $ collectPosts sl m
  where
    cmp (Post a) (Post b) = entryId a `compare` entryId b


buildContentMap :: String -> FilePath -> IO ContentMap
buildContentMap baseURL basedir = build [] "."
  where
    build :: [String] -> FilePath -> IO ContentMap
    build prefixes path = do
        files <- getDirectoryContents $ basedir </> path

        foldM processFile Map.empty files

     where
        ----------------------------------------------------------------
        pathSoFar :: FilePath
        pathSoFar = intercalate "/" prefixes

        ----------------------------------------------------------------
        processFile :: ContentMap -> FilePath -> IO ContentMap
        processFile mp f =
            if "." `isPrefixOf` f || "~" `isSuffixOf` f then
                return mp
              else do
                isDir <- doesDirectoryExist $ basedir </> pathSoFar </> f
                if isDir then dir mp f else file mp f

        ----------------------------------------------------------------
        dir :: ContentMap -> FilePath -> IO ContentMap
        dir mp f = do
            let fp = if null pathSoFar then f else concat [pathSoFar, "/", f]
            let fullPath = B.pack (concat [baseURL, "/", fp])
            !cm <- build (prefixes ++ [f]) fp
            return $! Map.insert (B.pack f)
                                 (ContentDirectory fullPath cm)
                                 mp

        ----------------------------------------------------------------
        file :: ContentMap -> FilePath -> IO ContentMap
        file mp f = do
            let fp = basedir </> pathSoFar </> f

            if ".md" `isSuffixOf` f then do
                -- it's a post
                let baseName = dropExtension f
                let pId = concat [baseURL, "/", pathSoFar, "/", baseName]
                !p <- readPost pId fp
                return $! Map.insert (B.pack baseName) (ContentPost p) mp
              else
                -- it's a static item
                return $! Map.insert (B.pack f) (ContentStatic fp) mp



------------------------------------------------------------------------
-- mutator functions for post objects
setEntryId :: String -> Post -> Post
setEntryId x (Post p) = Post $ p { entryId = x }

setEntryTitle :: String -> Post -> Post
setEntryTitle x (Post p) = Post $ p { entryTitle = TextString x }

--setEntryUpdated :: TimeZone -> UTCTime -> Post -> Post
--setEntryUpdated tz tm (Post p) = Post $ p { entryUpdated = formatAtomTime tz tm }

setEntryUpdated :: String -> Post -> Post
setEntryUpdated tm (Post p) = Post $ p { entryUpdated = tm }

setEntryAuthors :: String -> Post -> Post
setEntryAuthors x (Post p) = Post $ p { entryAuthors = parsePersons x }

setEntrySummary :: String -> Post -> Post
setEntrySummary x (Post p) = Post $ p { entrySummary = Just $ HTMLString x }

setEntryHTMLContent :: String -> Post -> Post
setEntryHTMLContent x (Post p) = Post $ p { entryContent = Just $ HTMLContent x }

setEntryContributor :: String -> Post -> Post
setEntryContributor x (Post p) = Post $ p { entryContributor = parsePersons x }


-- doubt we'll be using these for now
setEntryCategories :: [Category] -> Post -> Post
setEntryCategories x (Post p) = Post $ p { entryCategories = x }

setEntryLinks :: [Link] -> Post -> Post
setEntryLinks x (Post p) = Post $ p { entryLinks = x }

setEntryPublished :: Maybe Date -> Post -> Post
setEntryPublished x (Post p) = Post $ p { entryPublished = x }

setEntryRights :: Maybe TextContent -> Post -> Post
setEntryRights x (Post p) = Post $ p { entryRights = x }

setEntrySource :: Maybe Source -> Post -> Post
setEntrySource x (Post p) = Post $ p { entrySource = x }

setEntryInReplyTo :: Maybe InReplyTo -> Post -> Post
setEntryInReplyTo x (Post p) = Post $ p { entryInReplyTo = x }

setEntryInReplyTotal :: Maybe InReplyTotal -> Post -> Post
setEntryInReplyTotal x (Post p) = Post $ p { entryInReplyTotal = x }

setEntryAttrs :: [Attr] -> Post -> Post
setEntryAttrs x (Post p) = Post $ p { entryAttrs = x }

setEntryOther :: [Element] -> Post -> Post
setEntryOther x (Post p) = Post $ p { entryOther = x }
