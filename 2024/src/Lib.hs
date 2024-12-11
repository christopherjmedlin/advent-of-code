module Lib
    ( getDay
    ) where

import DayOne
import DayTwo (dayTwo)
import DayThree (dayThree)
import DayFour (dayFour)
import DayFive (dayFive)
import DaySix (daySix)
import DaySeven (daySeven)
import DayEight (dayEight)
import DayNine (dayNine)

getDay :: Integer -> ([String] -> (String, String))
getDay 1 = dayOne
getDay 2 = dayTwo
getDay 3 = dayThree
getDay 4 = dayFour
getDay 5 = dayFive
getDay 6 = daySix
getDay 7 = daySeven
getDay 8 = dayEight
getDay 9 = dayNine