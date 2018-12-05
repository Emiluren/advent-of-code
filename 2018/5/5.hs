import Data.Char

replace :: String -> String
replace (c1:c2:cs) =
  if isUpper c1 && isLower c2 && c1 == toUpper c2 then
    replace cs
  else if isLower c1 && isUpper c2 && c1 == toLower c2 then
    replace cs
  else
    c1 : replace (c2 : cs)
replace [] = []
replace [c] = [c]

replaceUntilFinished :: String -> String
replaceUntilFinished input =
  let res = replace input
  in if res == input then
    res
  else
    replaceUntilFinished res

partOne = length . replaceUntilFinished <$> readFile "5_input"

partTwo = do
  input <- readFile "5_input"
  let
    tryWithout letter = length
      (replaceUntilFinished $ filter (\c -> toLower c /= letter) input)
  return (minimum $ map tryWithout ['a'..'z'])
