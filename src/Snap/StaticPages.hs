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
import qualified Data.ConfigFile as Cfg
import           Data.List
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

    (feed, siteURL, baseURL, excludeList) <- readConfig configFilePath

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

getM :: Cfg.Get_C a => Cfg.ConfigParser -> String -> String -> Maybe a
getM cp section = either (const Nothing) Just . Cfg.get cp section


readConfig :: FilePath -> IO (Atom.Feed, String, String, ExcludeList)
readConfig fp = do
    cp <- parseConfig fp

    either (throwIO . StaticPagesException . show)
           return
           (mkFeed cp)
  where
    ensurePrefix :: Char -> String -> String
    ensurePrefix p s = if [p] `isPrefixOf` s then s else p:s

    stripSuffix :: Char -> String -> String
    stripSuffix x s = if [x] `isSuffixOf` s then init s else s


    mkFeed :: Either Cfg.CPError Cfg.ConfigParser
           -> Either Cfg.CPError (Atom.Feed, String, String, ExcludeList)
    mkFeed cfg = do
      cp       <- cfg
      title    <- Cfg.get cp "feed" "title"
      authors  <- Cfg.get cp "feed" "authors"
      baseURL' <- Cfg.get cp "default" "baseurl"
      siteURL' <- Cfg.get cp "default" "siteurl"

      let icon = getM cp "feed" "icon"
      let skip = maybe EL.empty
                       (EL.fromPathList . B.pack)
                       (getM cp "feed" "skipurls")

      let siteURL = stripSuffix '/' siteURL'
      let baseURL = stripSuffix '/' $ ensurePrefix '/' baseURL'
      let feedURL = (siteURL ++ baseURL)

      let feed = Atom.nullFeed feedURL
                               (Atom.TextString title)
                               ""

      let feed' = feed { Atom.feedAuthors = parsePersons authors
                       , Atom.feedIcon    = icon
                       , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                       }

      return (feed', siteURL, baseURL, skip)


parseConfig :: FilePath -> IO (Either Cfg.CPError Cfg.ConfigParser)
parseConfig = Cfg.readfile Cfg.emptyCP
