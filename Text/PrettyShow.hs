---------------------------------------------------------
--
-- Module        : Text.PrettyShow
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | Type class Show-like formating numbers for humans.
---------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Text.PrettyShow
   (PrettyShow             -- type class providing following functions
   ,showWithLen            -- returns output formated into String of given length
   ,showWithLenDec         -- like above with constrain to be for numbers with decimal point
   ,showWithLenDecChar     -- like above with char that spans output to get requested length
   ,showAmtWithLen         -- formats amount - always two digits after decimal point
   )

where

import Data.Time
import System.Locale (defaultTimeLocale)

-- | Provides String of given length for given type 'a'.
class (Show a) => PrettyShow a where
      -- | Of given length.
      showWithLen            :: (Integral b) => b -> a -> String
      -- | Of given length with given length after decimal point.
      showWithLenDec         :: (Integral b) => b -> b -> a -> String
      -- | Of given length with given length after decimal point with defined char for decimal poit.
      showWithLenDecChar     :: (Integral b) => b -> b -> Char -> a -> String
      -- | Formats amount which is @showAmtWithLen l x = showWithLenDec l 2 x@
      showAmtWithLen         :: (Integral b) => b -> a -> String

      showWithLen l x = showWithLenDec l 0 x
      showWithLenDec l d x = showWithLenDecChar l d ' ' x
      showAmtWithLen l x = showWithLenDec l 2 x

instance PrettyShow Int where
      showWithLenDecChar l d c n
	    | length string <= l1 = (replicate (l1 - (length string)) c) ++ string
	    | otherwise           = (replicate (l1 - 1) '.') ++ [last string]
	  where string = show n
	        l1     = fromIntegral l
      showAmtWithLen l n | l < 5     = replicate l1 '.'
                         | n < 0 &&
                           n > -100  = replicate (l1 - 5) ' ' ++ "-0." ++ showDec
                         | otherwise = showWithLen (l1 - 3)  (n `quot` 100) ++
                                         "." ++ showDec
         where l1 = fromIntegral l
               dec = (n `rem` 100)
               showDec = showWithLenDecChar 2 0 '0' (abs dec)
---         showWithLenDec l 2 ((fromIntegral x :: Double) / 100)

instance PrettyShow Integer where
      showWithLenDecChar l d c n
	    | d == -1 &&  
              length string <= l1 = (replicate (l1 - (length string)) c) ++ "-" ++ [last string]  -- hack to show '-'
	    | length string <= l1 = (replicate (l1 - (length string)) c) ++ string
	    | otherwise           = (replicate (l1 - 1) '.') ++ [last string]
	  where string = show n
	        l1     = fromIntegral l
      showAmtWithLen l n | l < 5     = replicate l1 '.'
                         | n < 0 &&
                           n > -100  = replicate (l1 - 5) ' ' ++ "-0." ++ showDec
                         | otherwise = showWithLen (l1 - 3)  (n `quot` 100) ++
                                         "." ++ showDec
         where l1 = fromIntegral l
               dec = (n `rem` 100)
               showDec = showWithLenDecChar 2 0 '0' (abs dec)
--      showAmtWithLen l x = showWithLenDec l 2 ((fromIntegral x :: Double) / 100)

instance PrettyShow String where
      showWithLenDecChar l _ c xs = take (fromIntegral l) (xs ++ repeat c)

instance PrettyShow Double where
      showWithLenDecChar l d c n
           | l <= d + 1 = replicate l1 '.'
           | d < 0      = replicate l1 '.'
           | d == 0     = showWithLen (l-d) (truncate n :: Integer)
           | n < 0 && n > (-1)
                        = showWithLenDec (l-d-2) (-1) (truncate n :: Integer) ++ "." ++ ratioPart -- hack to show '-' in case of -0.01
           | otherwise  = showWithLen (l-d-1) (truncate n :: Integer) ++ "." ++ ratioPart
         where ratioPart = showWithLenDecChar d 0 '0' $ abs $ (truncate (n * 10^d) - (truncate n) * 10^d ::Integer)-- = replicate (d - lenRatioPartRaw) '0' ++ ratioPartRaw
               l1 = fromIntegral l

instance PrettyShow Day where
      showWithLenDecChar 6 _ _ = formatTime defaultTimeLocale "%y%m%d"
      showWithLenDecChar _ _ _ = formatTime defaultTimeLocale "%Y %m %d"
