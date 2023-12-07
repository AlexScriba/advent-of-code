module PartA (parseLine, ColourCount (..), Game (..)) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

data ColourCount = ColourCount
    { red :: Int
    , green :: Int
    , blue :: Int
    }
    deriving (Show)

type Restriction = ColourCount

data Game = Game
    { gameId :: Int
    , counts :: ColourCount
    }
    deriving (Show)

main :: IO ()
main = do
    let restriction = ColourCount 12 13 14

    inputs <- lines <$> getContents

    let games = parseLine <$> inputs
        validGames = filter (validCount restriction) games

        invalidGames = filter (not . validCount restriction) games

        validIds = gameId <$> validGames
        result = sum validIds

    -- mapM_ print validGames
    print result

validCount :: Restriction -> Game -> Bool
validCount restriction game =
    let count = counts game

        redValid = red count <= red restriction
        greenValid = green count <= green restriction
        blueValid = blue count <= blue restriction
     in redValid && blueValid && greenValid

parseLine :: String -> Game
parseLine str =
    let (idData : gameData : _) = splitOn ":" str

        id = read $ last $ splitOn " " idData
        counts = parseGame gameData
     in Game id counts

parseGame :: String -> ColourCount
parseGame str =
    let rounds = splitOn "; " str
        counts = parseRound <$> rounds

        reds = red <$> counts
        greens = green <$> counts
        blues = blue <$> counts

        highestRed = maximum reds
        highestGreen = maximum greens
        highestBlue = maximum blues
     in ColourCount highestRed highestGreen highestBlue

parseRound :: String -> ColourCount
parseRound str =
    let colourData = splitOn ", " str
     in parseColours (ColourCount 0 0 0) colourData

parseColours :: ColourCount -> [String] -> ColourCount
parseColours = foldl parseColour

parseColour :: ColourCount -> String -> ColourCount
parseColour prev str =
    let (numStr : colour : _) = splitOn " " $ trim str
        num = read numStr :: Int
     in case colour of
            "red" -> prev{red = num}
            "green" -> prev{green = num}
            "blue" -> prev{blue = num}
            _ -> prev

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
