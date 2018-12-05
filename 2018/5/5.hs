import Data.Char

-- The first string is the characters before the current point and the second
-- are the ones after.
replace :: String -> String -> String
replace c1s [] = c1s
replace [] (c2:c2s) = replace [c2] c2s
replace (c1:c1s) (c2:c2s) =
  if isUpper c1 && isLower c2 && c1 == toUpper c2 then
    replace c1s c2s
  else if isLower c1 && isUpper c2 && c1 == toLower c2 then
    replace c1s c2s
  else
    replace (c2:c1:c1s) c2s

replaceUntilFinished :: String -> String
replaceUntilFinished input = replace [] input

partOne = length . replaceUntilFinished <$> readFile "5_input"

partTwo = do
  input <- readFile "5_input"
  let
    tryWithout letter = length
      (replaceUntilFinished $ filter (\c -> toLower c /= letter) input)
  return (minimum $ map tryWithout ['a'..'z'])
