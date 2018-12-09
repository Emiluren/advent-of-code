data Zipper a = Zipper [a] a [a]

-- instance Show a => Show (Zipper a) where
--   show (Zipper prev current next) =
--     "Zipper " ++ show (reverse prev) ++ " " ++ show current ++ " " ++ show next

stepClockwise :: Zipper a -> Zipper a
stepClockwise (Zipper prev current (next1:next)) =
  Zipper (current:prev) next1 next
stepClockwise (Zipper prev current []) =
  let rev = reverse prev in
    case rev of
      [] -> Zipper [] current []
      x:xs -> Zipper [current] x xs

stepCounterClockwise :: Zipper a -> Zipper a
stepCounterClockwise (Zipper (prev1:prev) current next) =
  Zipper prev prev1 (current:next)
stepCounterClockwise (Zipper [] current next) =
  let rev = reverse next in
    case rev of
      [] -> Zipper [] current []
      x:xs -> Zipper xs x [current]

remove :: Zipper a -> Zipper a
remove (Zipper prev _ (next1:next)) = Zipper prev next1 next
remove (Zipper prev _ []) =
  let rev = reverse prev in Zipper [] (head rev) (tail rev)

insertAfter :: Zipper a -> a -> Zipper a
insertAfter (Zipper prev current next) new = Zipper (current:prev) new next

get :: Zipper a -> a
get (Zipper _ x _) = x

addAt :: Num a => [a] -> a -> Int -> [a]
addAt (x:xs) y 0 = (x + y) : xs
addAt (x:xs) y n = x : addAt xs y (n - 1)

data State = State
  { circle :: Zipper Int
  , score :: [Int]
  , currentPlayer :: Int
  , marble :: Int
  } --deriving Show

makeMove :: Int -> State -> State
makeMove playerAmount (State circle score currentPlayer marble) =
  if marble `mod` 23 == 0 then
    let newScore = addAt score marble currentPlayer
        rotated = iterate stepCounterClockwise circle !! 7
    in State
       { circle = remove rotated
       , score = addAt newScore (get rotated) currentPlayer
       , currentPlayer = (currentPlayer + 1) `mod` playerAmount
       , marble = marble + 1
       }
  else State
       { circle = insertAfter (stepClockwise circle) marble
       , score = score
       , currentPlayer = (currentPlayer + 1) `mod` playerAmount
       , marble = marble + 1
       }

run :: Int -> Int -> Int
run playerAmount marbleAmount =
  let startState = State
        { circle = Zipper [] 0 []
        , score = take playerAmount [0,0..]
        , currentPlayer = 0
        , marble = 1
        }
  in maximum $ score $
     iterate (makeMove playerAmount) startState !! marbleAmount

partOne = run 463 71787

partTwo = run 463 7178700
