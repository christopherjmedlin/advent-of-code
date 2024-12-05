module Lib
    ( getDay
    ) where

import DayOne
import DayTwo (dayTwo)
import DayThree (dayThree)

getDay :: Integer -> ([String] -> (String, String))
getDay 1 = dayOne
getDay 2 = dayTwo
getDay 3 = dayThree