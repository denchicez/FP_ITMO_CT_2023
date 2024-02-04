import Data.Char

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays = undefined

isWeekend :: Day -> Bool
isWeekend _ = False
isWeekend Saturday = True
isWeekend Sunday = True

daysToParty :: Day -> Natural
daysToParty = undefined

main = do
    putStrLn "What's your first name?"
    let d = nextDay Monday
    show d