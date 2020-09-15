{- |
Module      : Main
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Parse times & durations from last(1).
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Prelude                 hiding ( take )

import           Control.Applicative            ( empty
                                                , (<|>)
                                                )
import           Data.Attoparsec.Text
import           Data.Functor                   ( ($>) )
import           Data.List                      ( sort )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime
                                                , addUTCTime
                                                , getCurrentTime
                                                , nominalDay
                                                , secondsToNominalDiffTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                , iso8601DateFormat
                                                , parseTimeM
                                                )
import           Data.Time.LocalTime            ( LocalTime
                                                , TimeZone
                                                , hoursToTimeZone
                                                , utcToLocalTime
                                                )


data Session = Session { sessUser :: Text
                       , sessFrom :: Text
                       , sessLogin :: UTCTime
                       , sessLogout :: UTCTime
                       } deriving (Eq, Ord, Show)

-- | Parse output from @last -F@.
outputFromLast :: UTCTime -> Parser [Session]
outputFromLast now = catMaybes <$> many' (line <* char '\n')
 where
  line       = wtmpBegins <|> reboot <|> (Just <$> sessionLine now) <|> blank
  blank      = skipHSpace $> Nothing
  wtmpBegins = "wtmp begins " *> timestamp $> Nothing
  reboot     = "reboot " *> takeTill isEndOfLine $> Nothing

-- | Parse a 'Session' from a line of output from @last -F@.
sessionLine :: UTCTime -> Parser Session
sessionLine now = do
  user      <- T.stripEnd <$> take 9
  from      <- T.stripEnd <$> take 13 <* take 17
  loginTime <- timestamp
  let down = " - down" *> skipHSpace1 *> ((`addUTCTime` loginTime) <$> duration)
  Session user from loginTime <$> (logoutTime <|> stillIn <|> down)
 where
  logoutTime = (" - " *> timestamp) <* skipSpace <* duration
  stillIn    = "   still logged in" $> now

-- | Parse a session duration, e.g. @(12:34)@, @(2+13:41)@.
duration :: Parser NominalDiffTime
duration = (char '(' *> (withDays <|> withoutDays)) <* char ')'

withoutDays :: Parser NominalDiffTime
withoutDays = do
  hh <- (decimal <* char ':') :: Parser Integer
  mm <- decimal :: Parser Integer
  pure $ secondsToNominalDiffTime $ fromIntegral $ hh * 3600 + mm * 60

withDays :: Parser NominalDiffTime
withDays = do
  days <- (decimal <* char '+') :: Parser Integer
  rest <- withoutDays
  pure $ nominalDay * fromIntegral days + rest

-- | Parse a timestamp like 'Fri Aug  7 19:25:07 2020'.
timestamp :: Parser UTCTime
timestamp = count 24 anyChar >>= parseTimeM False defaultTimeLocale "%a %b %e %T %Y"

-- | Skip zero or more spaces and/or horizontal tabs.
skipHSpace :: Parser ()
skipHSpace = many' (char ' ' <|> char '\t') $> ()

-- | Skip one or more spaces and/or horizontal tabs.
skipHSpace1 :: Parser ()
skipHSpace1 = many1 (char ' ' <|> char '\t') $> ()


-- | TODO: handle daylight saving
tz :: TimeZone
tz = hoursToTimeZone 10

-- | Pretty-print a 'Session'.
showSession :: Session -> Text
showSession Session {..} =
  sessUser <> " logged in from " <> sessFrom <> " between " <> from <> " and " <> until
 where
  from  = ft $ utcToLocalTime tz sessLogin
  until = ft $ utcToLocalTime tz sessLogout
  ft    = T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M"))


main :: IO ()
main = do
  input <- TIO.getContents
  now   <- getCurrentTime
  let Right sessions = parseOnly (outputFromLast now <* endOfInput) input
  mapM_ (TIO.putStrLn . showSession) $ sort sessions
