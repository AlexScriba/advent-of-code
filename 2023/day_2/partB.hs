import PartA (ColourCount (..), Game (..), parseLine)

main :: IO ()
main = do
    inputs <- lines <$> getContents

    let games = parseLine <$> inputs
        powers = powerOfGame <$> games

        total = sum powers

    print total

powerOfGame :: Game -> Int
powerOfGame Game{counts = count} =
    let ColourCount{red = r, green = g, blue = b} = count
     in r * b * g
