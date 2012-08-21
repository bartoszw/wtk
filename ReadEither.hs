---------------------------------------------------------
--
-- Module        : ReadEither
-- Copyright     : Bartosz Wójcik (2012)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Module is part of Wojcik Tool Kit package.
-- | Safe version of Read with human friendly error messages.
---------------------------------------------------------


{-# LANGUAGE TypeSynonymInstances #-}

module ReadEither
where

import Control.Monad (liftM)
import Control.Monad.Trans.Error
import Data.Char
import Data.Maybe
import Data.Time
import System.Locale
import Text.WtkParser
import Calculator


-- | Type class providing simple input parsing
class (Show a) => ReadEither a where
      readEither :: String -> Either String a  -- ^ Parses String to given readEither or returns error message.

instance ReadEither () where
         readEither _ = Right ()

instance ReadEither Char where
         readEither [] = Left "Empty string"
         readEither x  = Right $ head x
         
instance ReadEither String where
         readEither = Right

instance ReadEither Bool where
         readEither " " = Right False
         readEither "0" = Right False
         readEither "False" = Right False
         readEither _   = Right True

instance ReadEither Integer where
         readEither = eval intSigned

instance ReadEither Int where
         readEither x = eval intSigned x >>= return . fromIntegral  

instance ReadEither Double where
         readEither = eval real 

instance ReadEither Float where
         readEither x = eval real x >>= return . realToFrac 

-- | Parsing to Day can be done on many ways. For details see the source code.
instance ReadEither Day where
         readEither x = readEither' localeList
            where readEither' [] = Left "Date doesn't fit to the expected format" 
                  readEither' (l:ls) = case parseTime defaultTimeLocale l x of
                                       Just d  -> Right d
                                       Nothing -> readEither' ls

--  List of all parsable formats.
--  Month name only in english due to usage of @defaultTimeLocale@.
localeList = ["%Y %m %d"  -- 2000 11 10
             ,"%Y %b %d"  -- 2000 Nov 10
             ,"%Y-%m-%d"  -- 2000-11-10
             ,"%d %m %Y"  -- 10 11 2000
             ,"%d %b %Y"  -- 10 Nov 2000
             ,"%d.%m.%Y"  -- 10.11.2000
             ,"%Y.%m.%d"  -- 2000.11.10
             ,"%d-%m-%Y"  -- 10-11-2000
             ,"%Y/%m/%d"  -- 2000/11/10
             ,"%d/%m/%Y"  -- 10/11/2000
             ,"%d/%m/%y"  -- 10/11/00
             ,"%y %m %d"  -- 00 11 10
             ,"%d%m%y"    -- 101100
             ,"%d%m%Y"    -- 10112000
             ,"%y%m%d"    -- 0011210
             ,"%Y%m%d"    -- 200011210
             ]

instance ReadEither a => ReadEither (Maybe a) where
   readEither "" = Right Nothing
   readEither x  = liftM Just $ readEither x


-- | Type class providing input parsing with additional features.
class (ReadEither a) => ReadEitherPlus a where
      readEitherPlus :: String -> Either String a  -- ^ Parses String to given readEither or returns error message.

-- | Strips out all control charaters
instance ReadEitherPlus String where
         readEitherPlus = Right . filter (\x -> ord x >= 32)

-- | Parses arithmetic formula
instance ReadEitherPlus Double where
         readEitherPlus = evaluate

-- | Parses arithmetic formula
instance ReadEitherPlus Float where
         readEitherPlus = liftM realToFrac . evaluate

-- | Parses arithmetic formula
instance ReadEitherPlus Integer where
         readEitherPlus = evaluateInt

-- | Parses arithmetic formula
instance ReadEitherPlus Int where
         readEitherPlus = liftM fromIntegral . evaluateInt

instance ReadEitherPlus a => ReadEitherPlus (Maybe a) where
   readEitherPlus "" = Right Nothing
   readEitherPlus x  = liftM Just $ readEitherPlus x

