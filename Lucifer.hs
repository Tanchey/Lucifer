
{-# LANGUAGE QuasiQuotes #-}

module Lucifer    ( Digit(..),
                    unDigit,

                    zero, one, two, three, four, five, six, seven, eight, nine,
                    none,

                    digits, numbers,

                    digitToInt,
                    intToDigit,
                    toString,
                    fromString,
                    toList,
                    fromList,

                    allCandidates,
                    detailedCandidates,
                    realWorldDetailedCandidates
                   ) where

import Language.Literals.Binary
import Data.Word
import Data.Bits
import Numeric
import Data.Maybe
import Control.Monad
import qualified Data.Char as DC
import qualified Data.String as DS

newtype Digit = Digit Word8 deriving (Eq)

segmentsNumber = 7
--  _       _   _       _   _   _   _   _ 
-- | |   |  _|  _| |_| |_  |_    | |_| |_|   
-- |_|   | |_   _|   |  _| |_|   | |_|  _|
-- 

zero        = Digit [b| 1110111 |]
one         = Digit [b| 0010010 |]
two         = Digit [b| 1011101 |]
three       = Digit [b| 1011011 |]
four        = Digit [b| 0111010 |]
five        = Digit [b| 1101011 |]
six         = Digit [b| 1101111 |]
seven       = Digit [b| 1010010 |]
eight       = Digit [b| 1111111 |]
nine        = Digit [b| 1111011 |]

none        = Digit [b| 0000000 |]
allSegments = Digit [b| 11111111 |]

unDigit (Digit w) = w


digits = [zero, one, two, three, four, five, six, seven, eight, nine]
numbers = [0..9]

intToDigit :: Int -> Maybe Digit
intToDigit n = lookup n (numbers `zip` digits)

digitToInt :: Digit -> Maybe Int
digitToInt digit = lookup digit (digits `zip` numbers)

toString :: Digit -> String
toString = addZerosIfNeeded . toWhateverString 
    where 
         addZerosIfNeeded s
                | length s < segmentsNumber = replicate (segmentsNumber - length s) '0' ++ s
                | otherwise = s

toWhateverString :: Digit -> String
toWhateverString (Digit w ) = showIntAtBase 2 DC.intToDigit w ""

fromString :: String -> Maybe Digit
fromString s = w (sequence mapM fromChar s)
    where w Nothing = Nothing
          w (Just bits) = Just $ fromList bits

fromList :: [Word8] -> Digit
fromList bits = Digit $ foldl appendBit 0 (zip [0..] (reverse bits))
    where appendBit acc (position, bit) = acc + bit * 2^position

toList :: Digit -> [Word8]
toList (Digit w) = parseWord w [] segmentsNumber
    where parseWord _ acc 0 = acc
          parseWord word acc n = parseWord (word `div` 2) ((word `mod` 2):acc) (n-1)

fromChar ::  Char -> Maybe Word8
fromChar '1' = Just 1
fromChar '0' = Just 0
fromChar _ = Nothing

-- what we see       | 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
-- candidate segment | 0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1
-- definitely works  | 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1
-- definitely broken | 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
-- ------------------+--------------------------------
-- is it possible?   | + + + - + + + - + - + - + - + -
-- good candidate?   | 1 1 1 - 1 0 0 - 0 - 0 - 1 - 1 -
  
-- Shows whether observer really was able to see this segment of a digit after all
--             see      works    broken
possibleBit :: Word8 -> Word8 -> Word8 -> Bool
possibleBit _ 1 1 = False
possibleBit 1 _ 1 = False
possibleBit s w b | s > 1 || w > 1 || b > 1 = False
                  | otherwise = True

-- Shows whether observer could see this digit
digitIsPossible :: Digit -> Digit -> Digit -> Bool
digitIsPossible see works broken = all checkBit triples
    where triples = zip3 (toList see) (toList works) (toList broken)
          checkBit (s, w, b) = possibleBit s w b


-- =OR(AND(NOT(s);NOT(c));AND(s;c);AND(NOT(s);c;NOT(w);NOT(b)))
-- ^s & ^c | s & c | ^s & c & ^w & ^b
handleCandidate :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 
handleCandidate s c w b = complement s .&. complement c .|. s .&. c .|. complement s .&. c .&. complement w .&. complement b

digitIsGoodCandidate :: Digit -> Digit -> Digit -> Digit -> Bool
digitIsGoodCandidate (Digit see) (Digit works) (Digit broken) (Digit candidate) = Digit segments == allSegments
    where segments = handleCandidate see candidate works broken 

allCandidates :: Digit -> Digit -> Digit -> [Digit]
allCandidates see works broken = filter (digitIsGoodCandidate see works broken) digits

detailedCandidates :: Digit -> Digit -> Digit -> ([Digit], Digit, Digit)
detailedCandidates see works broken = 
    let 
        candidates = allCandidates see works broken
        newWorks = Digit (unDigit see .|. unDigit works)
        newBroken = Digit $ foldl (.&.) (complement (unDigit see)) (map unDigit candidates)
    in
        (candidates, newWorks, newBroken)

realWorldDetailedCandidates :: Digit -> Digit -> Digit -> Maybe ([Digit], Digit, Digit)
realWorldDetailedCandidates see works broken
     | digitIsPossible see works broken = Just (detailedCandidates see works broken)
     | otherwise = Nothing                                         

instance Show Digit where
    show digit
        | digit `elem` digits = "<" ++ [DC.intToDigit (fromJust (digitToInt digit))] ++ "> " ++ toString digit
        | otherwise = "<?> " ++ toString digit

    
