{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Prelude hiding (interact)
import Data.Default
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as LT
import Data.ByteString.Lazy.Char8 (interact)
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.ICalendar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime(..), DiffTime, secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime


remind :: Parser VCalendar
remind = do
  es <- events
  return $ def {
    vcEvents = Map.fromList $ do
        (n, event) <- zip [0..] es
        let ident = (LT.pack $ show n, Nothing)
        return (ident, event { veUID = UID (LT.pack $ show n) def
                             })
    }

  where

    events :: Parser [VEvent]
    events = (whitespace *> events)
             <|>
             (liftA2 (:) reminder events)
             <|>
             (ignored *> events)
             <|>
             (eof *> pure [])

    whitespace = skipMany1 $ oneOf " \t\r"

    ignored = do
             string "INCLUDE "
             skipMany (noneOf "\n")
             char '\n'

    reminder = do
      string "REM"
      whitespace
      (mYear, month, day) <- date
      mStartTime <- optionMaybe $ try $ do
                      whitespace
                      string "AT "
                      (h, m) <- time
                      return $ (h, m)
      let startDay =
            fromGregorian (fromMaybe 2000 mYear) (month + 1) day
          stampTime =
              secondsToDiffTime $
              case mStartTime of
                Nothing -> 0
                Just (h, m) -> fromIntegral $ 60 * h + m
          stamp :: DTStamp
          stamp =
            DTStamp (UTCTime startDay stampTime) def
          start :: DTStart
          start =
            case mStartTime of
              Nothing ->
                DTStartDate (Date startDay) def
              Just (h, m) ->
                let time = TimeOfDay h m 0
                in DTStartDateTime (FloatingDateTime $ LocalTime startDay time) def

          recurs = case mYear of
            Just _ -> Set.empty
            Nothing -> Set.singleton $
                       RRule (Recur { recurFreq       = Yearly
                                    , recurUntilCount = Nothing
                                    , recurInterval   = 1
                                    , recurBySecond   = []
                                    , recurByMinute   = []
                                    , recurByHour     = []
                                    , recurByDay      = []
                                    , recurByMonthDay = []
                                    , recurByYearDay  = []
                                    , recurByWeekNo   = []
                                    , recurByMonth    = []
                                    , recurBySetPos   = []
                                    , recurWkSt       = Monday
                                    }) def
      
      mDuration <- optionMaybe $ try $ do
        whitespace
        string "DURATION"
        whitespace
        (h, m) <- time
        return $ Right $ DurationProp (DurationTime def h m 0) def

      whitespace
      string "MSG "
      msg <- manyTill anyChar $ char '\n'
      
      return $
        VEvent { veDTStamp       = stamp
               , veUID           = UID "" def
               , veClass         = def
               , veDTStart       = Just start
               , veCreated       = Nothing
               , veDescription   = Nothing -- Just $ Description (LT.pack msg) Nothing Nothing def
               , veGeo           = Nothing
               , veLastMod       = Nothing
               , veLocation      = Nothing
               , veOrganizer     = Nothing
               , vePriority      = def
               , veSeq           = def
               , veStatus        = Nothing
               , veSummary       = Just $ Summary (LT.pack msg) Nothing Nothing def
               , veTransp        = def
               , veUrl           = Nothing
               , veRecurId       = Nothing -- Maybe RecurrenceId
               , veRRule         = recurs
               , veDTEndDuration = mDuration
               , veAttach        = mempty
               , veAttendee      = mempty
               , veCategories    = mempty
               , veComment       = mempty
               , veContact       = mempty
               , veExDate        = mempty
               , veRStatus       = mempty
               , veRelated       = mempty
               , veResources     = mempty
               , veRDate         = mempty
               , veAlarms        = mempty
               , veOther         = mempty
               }

    date =
      (try $ do
           y <- Just <$> year
           whitespace
           m <- month
           whitespace
           d <- day
           return (y, m, d)
      ) <|>
      (try $ do
           m <- month
           whitespace
           d <- day
           whitespace
           y <- Just <$> year
           return (y, m, d)
      ) <|>
      (try $ do
           d <- day
           whitespace
           m <- month
           whitespace
           y <- Just <$> year
           return (y, m, d)
      ) <|>
      (try $ liftA3 (,,) (Just <$> year) (whitespace *> day) (whitespace *> month)) <|>
      (try $ liftA2 (Nothing,,) month (whitespace *> day)) <|>
      (try $ liftA2 (flip (Nothing,,)) day (whitespace *> month))

    year =
      read <$> forM [1..4] (const $ digit)

    day =
      read <$> many1 digit

    month =
      fromMaybe undefined .
      (`elemIndex` months) <$>
      foldl (\m n -> m <|> try (string n)) (try $ string $ head months) (tail months)
      
      where months = [ "Jan", "Feb", "Mar"
                     , "Apr", "May", "Jun"
                     , "Jul" , "Aug", "Sep"
                     , "Oct" , "Nov", "Dec"
                     ]

    time :: Parser (Int, Int)
    time = do
      hour <- read <$> many1 digit
      char ':'
      min <- read <$> many1 digit
      return (hour, min)

main =
  interact $ \input ->
  printICalendar def $
  case parse remind "<stdin>" input of
    Left e -> error $ show e
    Right a -> a
