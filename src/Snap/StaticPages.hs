{-# LANGUAGE OverloadedStrings #-}

{-|

FIXME: document this.
-}


module Snap.StaticPages
  ( loadStaticPages
  , loadStaticPages'
  , reloadStaticPages
  , reloadStaticPages'
  , initStaticPages
  , serveStaticPages
--  , runStaticPagesHandler
--  , addExtraTemplateArguments
  , StaticPagesException
  , staticPagesExceptionMsg
  , StaticPagesState
  , staticPagesTemplateDir
  )
where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Aeson
import qualified Data.Attoparsec as Atto
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Types
import           System.Directory
import           System.FilePath
import qualified Text.Atom.Feed as Atom
import           Text.Printf
import           Text.Templating.Heist

------------------------------------------------------------------------------
import           Snap.StaticPages.Internal.Handlers
import           Snap.StaticPages.Internal.Post
import           Snap.StaticPages.Internal.Types
import qualified Snap.StaticPages.Internal.Util.ExcludeList as EL
import           Snap.StaticPages.Internal.Util.ExcludeList (ExcludeList)


------------------------------------------------------------------------------
loadStaticPages :: FilePath -> IO (MVar StaticPagesState)
loadStaticPages f = initStaticPages f >>= newMVar

loadStaticPages' :: TemplateState Snap -> FilePath -> IO (MVar StaticPagesState)
loadStaticPages' t f = initStaticPages' t f >>= newMVar


reloadStaticPages :: MVar StaticPagesState -> IO ()
reloadStaticPages mv = modifyMVar_ mv $ \st -> do
    let p = staticPagesPath st
    initStaticPages p


reloadStaticPages' :: TemplateState Snap -> MVar StaticPagesState -> IO ()
reloadStaticPages' ts mv = modifyMVar_ mv $ \st -> do
    let p = staticPagesPath st
    initStaticPages' ts p


{-|

  Initialize a static pages instance. Given the name of a directory on the
  disk, 'initStaticPages' searches it for configuration, content, and template
  files, and produces a finished 'StaticPagesState' value. Throws a
  'StaticPagesException' if there was an error reading the state.

-}
initStaticPages :: FilePath        -- ^ path to staticPages directory
                -> IO StaticPagesState
initStaticPages pth = initStaticPages' (emptyTemplateState pth) pth

initStaticPages' :: TemplateState Snap -- ^ root template state
                 -> FilePath           -- ^ path to staticPages directory
                 -> IO StaticPagesState
initStaticPages' ts pth = do
    -- make sure directories exist
    mapM_ failIfNotDir [pth, contentDir, staticPagesTemplateDir pth]

    (StaticPagesConfig feed siteURL baseURL excludeList) <-
        readConfig configFilePath

    cmap      <- buildContentMap baseURL contentDir

    etemplates <- loadTemplates (staticPagesTemplateDir pth) ts

    templates <- either (\s -> throwIO $
                               StaticPagesException $
                               "error loading templates: " ++ s)
                        return
                        etemplates

    return StaticPagesState {
                      staticPagesPath          = pth
                    , staticPagesSiteURL       = siteURL
                    , staticPagesBaseURL       = baseURL
                    , staticPagesPostMap       = cmap
                    , staticPagesTemplates     = templates
                    , staticPagesFeedInfo      = feed
                    , staticPagesFeedExcludes  = excludeList
                    , staticPagesExtraTmpl     = return
                    }

  where
    --------------------------------------------------------------------------
    unlessM :: IO Bool -> IO () -> IO ()
    unlessM b act = b >>= flip unless act

    --------------------------------------------------------------------------
    failIfNotDir :: FilePath -> IO ()
    failIfNotDir d = unlessM (doesDirectoryExist d)
                             (throwIO $ StaticPagesException
                                      $ printf "'%s' is not a directory" pth)

    --------------------------------------------------------------------------
    configFilePath = pth </> "config"
    contentDir     = pth </> "content"


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
