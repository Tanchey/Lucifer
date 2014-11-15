
{-# LANGUAGE QuasiQuotes #-}

module Lucifer    ( Digit(..),
                    unDigit,

                    zero, one, two, three, four, five, six, seven, eight, nine,
                    digits, numbers,

                    digitToInt,
                    intToDigit,
                    toString,

                    digitIsPossible ) where

import Language.Literals.Binary
import Data.Word
import Data.Bits
import Numeric
import Data.Maybe
import qualified Data.Char as DC
import qualified Data.String as DS

newtype Digit = Digit Word8 deriving (Eq)

segmentsNumber = 7
--  _       _   _       _   _   _   _   _ 
-- | |   |  _|  _| |_| |_  |_    | |_| |_|   
-- |_|   | |_   _|   |  _| |_|   | |_|  _|
 

zero = Digit [b| 1110111 |]
one = Digit [b| 0010010 |]
two = Digit [b| 1011101 |]
three = Digit [b| 1011011 |]
four = Digit [b| 0111010 |]
five = Digit [b| 1101011 |]
six = Digit [b| 1101111 |]
seven = Digit [b| 1010010 |]
eight = Digit [b| 1111111 |]
nine = Digit [b| 1111011 |]
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


digitIsPossible :: Digit -> Digit -> Digit -> Bool
digitIsPossible (Digit definitelyWork) (Digit real) (Digit candidate) = Digit segments == allSegments
    where segments = handleCandidate definitelyWork real candidate 

-- =OR(AND(NOT(B3);NOT(C3));AND(D3;C3);AND(B3;NOT(C3);NOT(D3)))
handleCandidate x y z = complement x .&. complement y .|. y .&. z .|. x .&. complement y .&. complement z


instance Show Digit where
    show digit
        | digit `elem` digits = "<" ++ [DC.intToDigit (fromJust (digitToInt digit))] ++ "> " ++ toString digit
        | otherwise = "<?> " ++ toString digit

    
