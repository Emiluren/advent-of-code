import Data.Array

-- Started on this solution but then switched to clojure the array lib was
-- complicated

input = 990941

type Index = Int

startScoreboard :: Array Index Int
startScoreboard = array (0, 1000000) [(0, 3), (1, 7)]

newScores :: (Ix i) => (i, i) -> Array i Int -> [Int]
newScores (i1, i2) arr =
  let score = arr!i1 + arr!i2
  in if score > 9 then
       [score `div` 10, score `mod` 10]
     else
       [score]

appendNewScores :: (Ix i, Enum i) => Array i Int -> (i, i) -> i -> Array i Int
appendNewScores board indices len =
  board // zip [len..] (newScores indices board)

arraySlice :: (Ix i) => [i] -> Array i Int -> [Int]
arraySlice indices arr =
  map (\i -> arr!i) indices
