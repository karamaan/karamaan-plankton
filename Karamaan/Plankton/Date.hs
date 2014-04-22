module Karamaan.Plankton.Date where

import Data.Time.Calendar (Day, addDays, fromGregorian, toGregorian)
import Data.Time.LocalTime (ZonedTime, zonedTimeToLocalTime, getZonedTime, localDay)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Time.Parse
import qualified Data.Time.LocalTime
import Karamaan.Plankton ((.:))

usDateFormatString :: String
usDateFormatString = "%m-%d-%Y"

isoDateFormatString :: String
isoDateFormatString = "%Y-%m-%d"

monthNameFormatString :: String
monthNameFormatString = "%B %e, %Y"

ymdNoSpaceFormatString :: String
ymdNoSpaceFormatString = "%Y%m%d"

usDateFormat :: Day -> String
usDateFormat = formatTimeDefault usDateFormatString

isoDateFormat :: Day -> String
isoDateFormat = formatTimeDefault isoDateFormatString

dayToSQL :: Day -> String
dayToSQL = formatTimeDefault isoDateFormatString

monthNameFormat :: Day -> String
monthNameFormat = formatTimeDefault monthNameFormatString

formatTimeDefault :: String -> Day -> String
formatTimeDefault = formatTime defaultTimeLocale

zonedTimeToDay :: ZonedTime -> Day
zonedTimeToDay = localDay . zonedTimeToLocalTime

todayLocal :: IO Day
todayLocal = (return . zonedTimeToDay) =<< getZonedTime

yesterdayLocal :: IO Day
yesterdayLocal = (return . dayBefore . zonedTimeToDay) =<< getZonedTime

dayBefore :: Day -> Day
dayBefore = addDays (-1)

dayOrTodayLocal :: Maybe Day -> IO Day
dayOrTodayLocal = maybe todayLocal return

dayOrYesterdayLocal :: Maybe Day -> IO Day
dayOrYesterdayLocal = maybe yesterdayLocal return

-- TODO: this accepts "2013-07-123" and returns 'Just 2013-07-12'.
-- Seems like a bug
parseDate :: String -> Maybe Day
parseDate = fmap (Data.Time.LocalTime.localDay . fst)
            . Data.Time.Parse.strptime isoDateFormatString
-- ^^ Here we use Data.Time.Parse.strptime rather than parseTime because
-- Massy wants '2013-5-28' to be valid
-- (Message-Id: <10CFB694-68C0-4FFF-8019-61035E3F8C3E@karamaan.com>)
-- parseTime doesn't support that because parseValue's definition for the
-- 'm' character is 'digits 2', rather than 'spdigits 2'

parseDateG :: String -> String -> Maybe Day
parseDateG = fmap (Data.Time.LocalTime.localDay . fst)
             .: Data.Time.Parse.strptime

firstDayOfYear :: Day -> Day
firstDayOfYear day = fromGregorian year 1 1
  where (year, _, _) = toGregorian day

firstDayOfQuarter :: Day -> Day
firstDayOfQuarter day = fromGregorian year quarterMonth 1
  where (year, month, _) = toGregorian day
        quarterMonth = ((+1) . (*3) . (`div` 3) . (subtract 1)) month

firstDayOfMonth :: Day -> Day
firstDayOfMonth day = fromGregorian year month 1
  where (year, month, _) = toGregorian day
