{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where

import GHC.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay = \case
  Monday -> Tuesday
  Tuesday -> Wednesday
  Wednesday -> Thursday
  Thursday -> Friday
  Friday -> Saturday
  Saturday -> Sunday
  Sunday -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays num day = case num of
  0 -> day
  _ -> afterDays (num -1) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend = \case
  Saturday -> True
  Sunday -> True
  _ -> False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty day = case day of
  Friday -> 0
  _ -> 1 + daysToParty (nextDay day)
