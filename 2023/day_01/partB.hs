module DayOne.PartB () where

import Data.Char (isDigit)
import Debug.Trace (trace)
import GhcPlugins (stringTy)

type Mutator = String -> String

main :: IO ()
main = do
    inputs <- lines <$> getContents

    let nums = processString <$> inputs
        total = sum nums

    print total

processString :: String -> Int
processString str =
    let findFirst = getFirstNumber id
        findLast = getFirstNumber reverse . reverse

        firstNum = findFirst str
        lastNum = findLast str
     in firstNum * 10 + lastNum

wordToInt :: String -> Maybe Int
wordToInt str =
    case str of
        "one" -> Just 1
        "two" -> Just 2
        "three" -> Just 3
        "four" -> Just 4
        "five" -> Just 5
        "six" -> Just 6
        "seven" -> Just 7
        "eight" -> Just 8
        "nine" -> Just 9
        _ -> Nothing

getFirstNumber :: Mutator -> String -> Int
getFirstNumber _ [] = 0
getFirstNumber mutator str@(_ : chs) = case getNumberAtFront mutator str of
    Just n -> n
    Nothing -> getFirstNumber mutator chs

getNumberAtFront :: Mutator -> String -> Maybe Int
getNumberAtFront mutator str
    | isDigit (head str) = Just $ read [head str]
    | otherwise = checkAtFront str
  where
    checkAtFront = checkForNumberInLengthRange 5 3 mutator

checkForNumberInLengthRange :: Int -> Int -> Mutator -> String -> Maybe Int
checkForNumberInLengthRange maxLength minLength mutator str
    | maxLength == minLength = tryParse maxLength str
    | otherwise = case tryParse maxLength str of
        Nothing -> checkForNumberInLengthRange (maxLength - 1) minLength mutator str
        found -> found
  where
    tryParse n str = wordToInt $ mutator $ take n str
