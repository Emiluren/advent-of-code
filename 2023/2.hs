data Color = R | G | B deriving (Show, Eq, Ord)

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString c input =
    let (word, rest) = break (== c) input
    in word : splitString c (dropWhile (== c) rest)

parseColor :: String -> Color
parseColor "red" = R
parseColor "green" = G
parseColor _ = B 

l = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

parseLine :: String -> (Int, [(Int, Color)])
parseLine line =
    let
        (lineStart, lineGames) = span (/= ':') line

        gameN :: Int
        gameN = read $ drop 5 lineStart

        gameStrings :: [String]
        gameStrings = splitString ';' (drop 1 lineGames)
    in
        (gameN, concat $ map parseGame gameStrings)

parseGame :: String -> [(Int, Color)]
parseGame game = map parseColorAndCount $ splitString ',' game

parseColorAndCount :: String -> (Int, Color)
parseColorAndCount colorAndCount =
    let
        ws = words colorAndCount
        count = read (ws !! 0)
    in
        (count, parseColor $ ws !! 1)

countIsValid :: (Int, Color) -> Bool
countIsValid (n, R) = n <= 12
countIsValid (n, G) = n <= 13
countIsValid (n, B) = n <= 14

isColor c1 (_, c2) = c1 == c2

gameIsValid = all countIsValid

gameProduct (_, g) =
    let reds = map fst $ filter (isColor R) g
        greens = map fst $ filter (isColor G) g
        blues = map fst $ filter (isColor B) g
    in
        maximum reds * maximum greens * maximum blues

main :: IO ()
main = do
    input <- readFile "2input"

    let parsedGames = map parseLine (lines input)
    putStr "Part 1: "
    print $ sum $ map fst $ filter (\(n, g) -> gameIsValid g) parsedGames

    putStr "Part 2: "
    print $ sum (map gameProduct parsedGames)
