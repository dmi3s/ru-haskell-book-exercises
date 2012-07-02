module Calendar where

import Prelude (Int, Char, String, Show(..), (++))

data Date = Date Year Month Day

data Year = Year Int

data Month = January   | February   | March     | April
           | May       | June       | July      | August
           | September | October    | November  | December

data Day = Day Int

data Week = Monday    | Tuesday    | Wednesday
          | Thursday  | Friday     | Saturday
          | Sunday

data Time = Time Hour Minute Second


data Hour   = Hour    Int
data Minute = Minute  Int
data Second = Second  Int

instance Show Week where
  show Monday = "Mon"
  show Tuesday = "Tue"
  show Wednesday = "Wed"
  show Thursday = "Thu"
  show Friday = "Fri"
  show Saturday = "Sat"
  show Sunday = "Sun"

instance Show Date where
  show(Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

instance Show Year where
  show (Year y) = show y

instance Show Month where
  show January = "Jan"
  show February = "Feb"
  show March = "Mar"
  show April = "Apr"
  show June = "Jun"
  show July = "Jul"
  show August = "Aug"
  show September = "Sep"
  show October = "Oct"
  show November = "Nov"
  show December = "Dec"

instance Show Day where
  show (Day d) = addZero (show d)


instance Show Time where
  show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
  show (Hour h) = addZero (show h)

instance Show Minute where
  show (Minute m) = addZero (show m)

instance Show Second where
  show (Second s) = addZero (show s)

addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as     = as
