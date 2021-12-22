import System.IO
import Text.Read
import Data.Foldable (asum, foldl')

data Snailfish = Pair Snailfish Snailfish | V Int

instance Show Snailfish where
  show (Pair sn1 sn2) = "[" ++ show sn1 ++ "," ++ show sn2 ++ "]"
  show (V v) = show v

instance Read Snailfish where
  readPrec = choice [pPair, pV] where
    pChar c = do
      c' <- get
      if c == c' then return c else pfail
    pPair = Pair <$> (pChar '[' *> readPrec) <*> (pChar ',' *> readPrec <* pChar ']')
    pV = V <$> readPrec

readFromInputFile :: IO [Snailfish]
readFromInputFile = do
  file <- openFile "18input" ReadMode
  contents <- hGetContents file
  return $ map read (lines contents)

addDeepLeft :: Snailfish -> Int -> Snailfish
addDeepLeft sf 0 = sf
addDeepLeft (Pair sn1 sn2) add = Pair (addDeepLeft sn1 add) sn2
addDeepLeft (V v) add = V (v + add)

addDeepRight :: Snailfish -> Int -> Snailfish
addDeepRight sf 0 = sf
addDeepRight (Pair sn1 sn2) add = Pair sn1 (addDeepRight sn2 add)
addDeepRight (V v) add = V (v + add)

explode :: Snailfish -> Int -> Maybe (Snailfish, Int, Int)
explode (V _) _ = Nothing
explode (Pair (V v1) (V v2)) depth | depth > 4 = Just (V 0, v1, v2)
explode (Pair sn1 sn2) depth =
  asum
    [ do
        (n, al, ar) <- explode sn1 (depth+1)
        Just (Pair n (addDeepLeft sn2 ar), al, 0)
    , do
        (n, al, ar) <- explode sn2 (depth+1)
        Just (Pair (addDeepRight sn1 al) n, 0, ar)
    ]

split :: Snailfish -> Maybe Snailfish
split (V v) = if v >= 10 then
  Just $ Pair (V $ v `div` 2) (V $ v `div` 2 + v `mod` 2) else
  Nothing
split (Pair sn1 sn2) = asum
  [ (\n -> Pair n sn2) <$> split sn1
  , Pair sn1 <$> split sn2
  ]

reduce :: Snailfish -> Snailfish
reduce fish = case asum [(\(f, _, _) -> f) <$> explode fish 1, split fish] of
  Just newFish ->
    reduce newFish
  Nothing ->
    fish

mag :: Snailfish -> Int
mag (V v) = v
mag (Pair sn1 sn2) = (mag sn1)*3 + (mag sn2*2)

part1 :: [Snailfish] -> Int
part1 input = mag $
  foldl' (\acc fish -> reduce $ Pair acc fish) (reduce $ head input) (tail input)

part2 :: [Snailfish] -> Int
part2 input =
  maximum $ for input $ \f1 ->
    maximum $ for input $ \f2 ->
      mag $ reduce $ Pair f1 f2
  where for = flip map

main :: IO ()
main = do
  input <- readFromInputFile
  putStr "Part 1: "
  print (part1 input)
  putStr "Part 2: "
  print (part2 input)
