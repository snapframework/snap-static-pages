{-# LANGUAGE DeriveDataTypeable #-}

module Snap.StaticPages.Internal.Exception 
( StaticPagesException(..)
, staticPagesExceptionMsg )
where

import           Control.Exception
import           Data.Typeable

-- | 'StaticPagesException' is the exception type thrown when StaticPages
-- encounters an error.
data StaticPagesException = StaticPagesException String
  deriving (Show, Typeable)

instance Exception StaticPagesException


-- | Obtain the error message from a 'StaticPagesException'
staticPagesExceptionMsg :: StaticPagesException -> String
staticPagesExceptionMsg (StaticPagesException s) = s
