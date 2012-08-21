---------------------------------------------------------
--
-- Module        : Text.WtkParser
-- Copyright     : Bartosz Wójcik (2012)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Module is part of Wojcik Tool Kit package.
-- | Simple parser for internal usage of Wtk.
-- Motivation for this parser vs. already existing ones
-- was its lightweightness. This decision can be changed
-- in the future, the parser is not available outside wtk.
---------------------------------------------------------

module Text.WtkParser
where

import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char
import Data.Either

newtype Parser a = Parser {
                   runParser :: String -> Either String (String,a) }
                   
eval :: Parser a -> String -> Either String a
eval p s = case runParser p s of
              Left err      -> Left err
              Right (_,res) -> Right res

instance Monad Parser where
   return a = Parser (\xl -> Right (xl,a))
   fail   s = Parser (\xl -> Left s)
   Parser m >>= k = Parser $ \xl ->
      case m xl of
         Left s        -> Left s
         Right (xl',a) ->
            let Parser n = k a
            in n xl'

instance MonadPlus Parser where
   mzero = Parser (\xl -> Left "mzero")
   Parser p `mplus` Parser q = Parser $ \xl ->
      case p xl of
         Right a  -> Right a
         Left err -> case q xl of
                        Right b -> Right b
                        Left _  -> Left err

char :: Char -> Parser Char
char c = Parser char'
   where char' []    = Left ("expecting " ++ [c] ++ ",got EOF")
         char' (x:xs) | x == c    = Right (xs,c)
                      | otherwise = Left ("expecting " ++ [c] ++ ",got " ++ show x)

matchChar :: (Char -> Bool) -> Parser Char
matchChar isTrue = Parser mC
   where mC [] = Left ("expecting char, got EOF")
         mC (x:xs) | isTrue x  = Right (xs,x)
                   | otherwise = Left ("expecting char, got " ++ show x)

many :: Parser a -> Parser [a]
many (Parser p) = Parser many'
   where many' x =
            case p x of
               Left err     -> Right (x,[])
               Right (x',a) ->
                  let Right (x'',rest) = many' x'
                  in Right (x'',a:rest)

manyF :: (a -> a -> a) -> Parser a -> a -> Parser a
manyF f (Parser p) n = Parser many'
   where many' x =
            case p x of
               Left err     -> Right (x,n)
               Right (x',a) ->
                  let Right (x'',rest) = many' x'
                  in Right (x'',rest `f` a)

skipMany :: Parser p -> Parser ()
skipMany (Parser p) = Parser skip'
   where skip' x =
            case p x of
               Left err     -> Right (x,())
               Right (x',_) -> skip' x'

skipSpaces = (skipMany . matchChar) isSpace

lexeme p = do
            v <- p
            skipSpaces
            return v

--int :: Parser Int
int = do
   t1 <- matchChar isDigit
   tr <- many $ matchChar isDigit
   return (read (t1:tr))

--intSigned :: Parser Int
intSigned = (char '-' >> liftM (*(-1)) int) `mplus` int

fromIntSign :: (Num a) => Parser a
fromIntSign = liftM fromIntegral intSigned

fromIntUnsign :: (Num a) => Parser a
fromIntUnsign = liftM fromIntegral int

decimalize :: (Ord a, Fractional a) => a -> a
decimalize n | abs n >= 1 = decimalize (n / 10)
             | otherwise = n

real :: (Fractional a, Ord a) => Parser a
real  = sign >>= \s ->
        fromIntUnsign >>= \i ->
        (decimalPart >>= \d ->
         (expPart >>= \e ->
          return (withExp (s * (i + d)) e))
          `mplus`
          return (s * (i + d))
        )
        `mplus`
        ((expPart >>= \e ->
         return (withExp (s*i) e)
         )
         `mplus`
         return (s*i)
        )
  where sign = (char '-' >> return (-1)) `mplus` return 1
        withExp n e | e < 0     = n / 10 ^^ (-e)
                    | otherwise = n * 10 ^^ e
        decimalPart = (char '.' `mplus` char ',') >>
                      many (char '0') >>= \zeroes ->
                      (fromIntUnsign >>= \b ->
                       return ((decimalize b) / 10 ^ length zeroes)
                      )
                      `mplus`
                      return 0
        expPart = char 'e' >> fromIntSign

eof = Parser many'
   where many' [] = Right ([],())
         many' xs = Left $ "Epexted EOF, got " ++ take 10 xs
