{-
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import qualified Control.Monad.ST.Unsafe as STU

data Card = Card {card :: Int} | Joker {joker :: Int} deriving Eq

instance Show Card where
  show (Card c) = [chr (number + symbol * 16 + 0x1F0A0)]
    where number = norm $ (c - 1) `mod` 13 + 1
          symbol = (c - 1) `div` 13
          norm i
            | i < 12    = i
            | otherwise = i + 1
  show (Joker j) = ["🃏", "🃟"] !! j

value :: Card -> Int
value (Card i) = i
value _        = 53

deck :: [Card]
deck = map Card [1..52] ++ [Joker 0, Joker 1]

tripleCutIdx :: Int -> Int -> Int -> Int
tripleCutIdx a b i
  | i <  t    = i + nd
  | i >  d    = i - d
  | otherwise = i - t + nt
  where t  = min a b
        d  = max a b
        nt = 55 - d
        nd = 55 - t

cutIdx :: Int -> Int -> Int
cutIdx p i
  | i == 54   = 54
  | i <=  p   = 53 - p + i
  | otherwise = i - p

swapIdx :: Int -> Int -> Int
swapIdx 54  1 = 1
swapIdx 54 54 = 2
swapIdx 54 i  = i + 1
swapIdx p  i
  | i == p     = i + 1
  | i == p + 1 = p
  | otherwise  = i
                 
solitaireSteps :: STRef s (STArray s Int Card) -> STRef s Int -> STRef s Int ->
                  ST s [Card]
solitaireSteps cards a b =
  do swapWithNext a                                                     -- 2
     swapWithNext b >> swapWithNext b                                   -- 3
     liftM2 tripleCutIdx (readSTRef a) (readSTRef b) >>= makeCut        -- 4
     readSTRef cards >>= flip readArray 54 >>= makeCut . cutIdx . value -- 5
                              
     array <- readSTRef cards
     crd   <- readArray array 1
     final <- readArray array $ value crd + 1
                              
     -- This is safe: I don't use cards, a and b after this call
     others <- STU.unsafeInterleaveST $ solitaireSteps cards a b
     if (value final <= 52)
       then return $ final : others
       else return others

        -- | this can be made a lot faster with in-place modifications,
        --   except than in the "54" case
  where swapWithNext v = readSTRef v >>= makeCut . swapIdx
                            
        makeCut idx = do p1 <- liftM idx $ readSTRef a
                         p2 <- liftM idx $ readSTRef b
                         readSTRef cards >>= mapIdx idx >>= writeSTRef cards
                         updateJoker p1 >> updateJoker p2
                         
        updateJoker p = do Joker j <- readSTRef cards >>= flip readArray p
                           flip writeSTRef p $ [a, b] !! j

        mapIdx idx array = do ret <- newArray_ (1, 54) ::
                                ST s (STArray s Int Card)
                              sequence_ [do v <- readArray array i
                                            writeArray ret (idx i) v
                                         | i <- [1..54]]
                              return ret

solitaireString :: [Card]
solitaireString = runST $ do cards <- newListArray (1, 54) deck >>= newSTRef
                             a <- newSTRef 53
                             b <- newSTRef 54
                             solitaireSteps cards a b

cardString :: [Int]
cardString = map convert solitaireString
  where convert c = (value c - 1) `mod` 26 + 1

normalizeString :: String -> String
normalizeString = normalize (0 :: Integer) . map toUpper . filter isLetter
  where normalize n (c:cs) = c   : normalize ((n + 1) `mod` 5) cs
        normalize 0 []     = []
        normalize n []     = 'X' : normalize ((n + 1) `mod` 5) []

formatString :: String -> String
formatString = intercalate " " . chunksOf 5 . normalizeString

encodeString :: (Int -> Int -> Int) -> String -> String
encodeString f = formatString . zipWith encode cardString . normalizeString
  where encode c l = chr $ ((ord l - ord 'A') `f` c) `mod` 26 + ord 'A'

main :: IO ()
main = do line <- getLine
          putStrLn $ "original:  " ++ formatString line
          putStrLn $ "encrypted: " ++ encodeString (+) line
          putStrLn $ "decrypted: " ++ encodeString (-) line
