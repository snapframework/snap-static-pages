{-# LANGUAGE OverloadedStrings #-}

{-|

FIXME: document this.
-}


module Snap.StaticPages
  ( staticPagesInit
--  , runStaticPagesHandler
--  , addExtraTemplateArguments
  , StaticPagesException
  , staticPagesExceptionMsg
  , StaticPages
  , staticPagesTemplateDir
  )
where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.Aeson
import qualified Data.Attoparsec as Atto
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Directory
import           System.FilePath
import qualified Text.Atom.Feed as Atom
import           Text.Printf

------------------------------------------------------------------------------
import           Snap.StaticPages.Internal.Handlers
import           Snap.StaticPages.Internal.Post
import           Snap.StaticPages.Internal.Types
import qualified Snap.StaticPages.Internal.Util.ExcludeList as EL
import           Snap.StaticPages.Internal.Util.ExcludeList (ExcludeList)


desc :: T.Text
desc = "Simple blog backed by flat files"


{-|

  Initialize a static pages instance. Given the name of a directory on the
  disk, 'staticPagesInit' searches it for configuration, content, and template
  files, and produces a finished 'StaticPages' value. Throws a
  'StaticPagesException' if there was an error reading the state.

-}
staticPagesInit :: HasHeist b
                => FilePath
                -- ^ path to staticPages directory
                -> SnapletInit b StaticPages
staticPagesInit p = makeSnaplet "static-pages" desc Nothing $ do
    let pth = staticPagesTemplateDir p
    -- make sure directories exist
    liftIO $ mapM_ failIfNotDir [p, contentDir, pth]

    (StaticPagesConfig feed siteURL baseURL excludeList) <-
        liftIO $ readConfig configFilePath

    cmap <- liftIO $ buildContentMap baseURL contentDir

    url <- getSnapletRootURL
    addTemplatesAt url pth
    addRoutes [ ("", serveStaticPages) ]

    return StaticPages {
                      staticPagesPath          = p
                    , staticPagesSiteURL       = siteURL
                    , staticPagesBaseURL       = baseURL
                    , staticPagesPostMap       = cmap
                    , staticPagesFeedInfo      = feed
                    , staticPagesFeedExcludes  = excludeList
                    }
  where
    --------------------------------------------------------------------------
    unlessM :: IO Bool -> IO () -> IO ()
    unlessM b act = b >>= flip unless act

    --------------------------------------------------------------------------
    failIfNotDir :: FilePath -> IO ()
    failIfNotDir d = unlessM (doesDirectoryExist d)
                             (throwIO $ StaticPagesException
                                      $ printf "'%s' is not a directory" p)

    --------------------------------------------------------------------------
    configFilePath = p </> "config"
    contentDir     = p </> "content"


------------------------------------------------------------------------------
-- | Takes the static pages root directory and returns the template directory
-- for static pages.  If you construct your own 'TemplateState', use this to
-- construct the parameter to emptyTemplateState.
staticPagesTemplateDir :: FilePath -> FilePath
staticPagesTemplateDir pth = pth </> "templates"


data StaticPagesConfig = StaticPagesConfig {
      _feed     :: Atom.Feed
    , _siteURL  :: String
    , _baseURL  :: String
    , _excludes :: ExcludeList
    }


instance FromJSON StaticPagesConfig where
    parseJSON (Object m) = do
        tFeedTitle <- m .:  "feedTitle"
        tAuthors   <- m .:  "feedAuthors"
        tBaseURL   <- m .:  "baseURL"
        tSiteURL   <- m .:  "siteURL"
        tIcon      <- m .:? "icon"
        tSkipStr   <- m .:? "skipurls"

        let skip = maybe EL.empty
                   (EL.fromPathList . T.encodeUtf8)
                   tSkipStr

        let feedTitle = T.unpack tFeedTitle
        let authors   = T.unpack tAuthors
        let baseURL   = stripSuffix '/' $ ensurePrefix '/' $ T.unpack tBaseURL
        let siteURL   = stripSuffix '/' $ T.unpack tSiteURL
        let feedURL   = siteURL ++ baseURL
        let icon      = fmap T.unpack tIcon

        let feed = Atom.nullFeed feedURL
                                 (Atom.TextString feedTitle)
                                 ""

        let feed' = feed { Atom.feedAuthors = parsePersons authors
                         , Atom.feedIcon    = icon
                         , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                         }

        return $! StaticPagesConfig feed' siteURL baseURL skip


      where
        ensurePrefix :: Char -> String -> String
        ensurePrefix p s = if [p] `isPrefixOf` s then s else p:s

        stripSuffix :: Char -> String -> String
        stripSuffix x s = if [x] `isSuffixOf` s then init s else s



    parseJSON _          = mzero


readConfig :: FilePath -> IO StaticPagesConfig
readConfig fp = do
    contents <- B.readFile fp
    let val  =  either errorOut id $ Atto.parseOnly json contents
    return $! resToVal $! fromJSON val

  where
    errorOut e = error $ concat [
                  "Error parsing config file \""
                 , fp
                 , "\": "
                 , e ]

    resToVal (Error e)   = error e
    resToVal (Success x) = x
