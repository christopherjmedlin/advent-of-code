module Lib
    ( getDay
    ) where

import DayOne

getDay :: Integer -> ([String] -> (String, String))
getDay 1 = dayOne
