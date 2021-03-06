{-# LANGUAGE CPP #-}

module Snap.StaticPages.Internal.Time where

import           Data.Time
import           Text.Printf

#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif

formatAtomTime :: TimeZone -> UTCTime -> String
formatAtomTime tz =  fmt . utcToLocalTime tz
  where
    fmt t = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t ++ z
      where
        mins  = timeZoneMinutes tz
        minus = if mins < 0 then "-" else "+"
        h     = printf "%02d" $ abs mins `div` 60
        m     = printf "%02d" $ abs mins `rem` 60
        z     = concat [minus, h, ":", m]


parseAtomTime :: String -> ZonedTime
parseAtomTime s = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" s


friendlyTime :: ZonedTime -> String
friendlyTime t = formatTime defaultTimeLocale "%b %e, %Y" t

