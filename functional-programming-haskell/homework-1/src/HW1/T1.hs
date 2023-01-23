module HW1.T1 where

import Numeric.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay d = case d of
  Monday -> Tuesday
  Tuesday -> Wednesday
  Wednesday -> Thursday
  Thursday -> Friday
  Friday -> Saturday
  Saturday -> Sunday
  Sunday -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays n d = case n of
  0 -> d
  1 -> nextDay d
  x -> afterDays (x - 1) $ nextDay d

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend d = case d of
  Saturday -> True
  Sunday -> True
  _ -> False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty d = case d of
  Friday -> 0
  _ -> daysToParty (nextDay d) + 1
