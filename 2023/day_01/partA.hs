module DayOne.PartA () where

import Data.Char (isDigit)
import Language.Haskell.TH (lamE)

main :: IO ()
main = do
    inputs <- lines <$> getContents

    let result = processInputs inputs
        total = sum result

    print total

processInputs :: [String] -> [Int]
processInputs = map processPair

processPair :: String -> Int
processPair pair =
    let digitPairs = getFirstAndLastDigit pair
        digits = (\(x, y) -> [x, y]) digitPairs
        numbers = read digits
     in numbers

getFirstAndLastDigit :: String -> (Char, Char)
getFirstAndLastDigit lst =
    let justTheDigits = filter isDigit lst
     in (head justTheDigits, last justTheDigits)
