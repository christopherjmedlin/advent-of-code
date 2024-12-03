module Lib
    ( getDay
    ) where

import DayOne
import DayTwo (dayTwo)

getDay :: Integer -> ([String] -> (String, String))
getDay 1 = dayOne
getDay 2 = dayTwo
