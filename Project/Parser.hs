module Parser (runParser, parseCmd, parseInput) where

import Cmd

import Data.Maybe
import Data.Char

import Control.Applicative

-- We begin by defining the type of parsers for a type:
newtype Parser tok a = Parser { runParser :: [tok] -> Maybe (a,[tok]) }

-- The idea is that a value of type Parser tok a is something that
-- takes a string of tokens as input, and tries to parse a prefix of the
-- input as a value of type a.  If it succeeds, it returns "Just" of a
-- value of type (a,[tok]), where the second component is the suffix of
-- remaining tokens.  Otherwise it returns "Nothing".

-- Or in other words, adapting a poem by Graham Hutton,

    -- A parser for things

    -- Is a function from strings

    -- To maybe a pair

    -- Of a thing and a string!

-- Anyways, we will use the fact that for any type `tok` of tokens,
-- `Parser tok` defines a monad.
instance Monad (Parser tok) where
  -- return :: a -> Parser tok a
  return x = Parser (\ts -> Just (x,ts))

  -- (>>=) :: Parser a -> (a -> Parser tok b) -> Parser tok b
  p >>= f  = Parser (\ts -> case runParser p ts of
                             Nothing -> Nothing
                             Just (x,ts') -> runParser (f x) ts')

-- We add some boilerplate code to derive Functor and Applicative
-- instances from the Monad instance
instance Functor (Parser tok) where
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok) where
  pure = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

-- Note that the type Parser tok a is isomorphic to StateT [tok] Maybe a,
-- and we could have defined it that way to automatically derive all
-- these type class instances. But we prefer to do it for ourselves.

-- We also define an Alternative instance, which makes it convenient
-- to write backtracking parsers.
instance Alternative (Parser tok) where
  -- empty :: Parser tok a
  empty = Parser (\ts -> Nothing)

  -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  p1 <|> p2 = Parser (\ts -> case runParser p1 ts of
                               Just (x,ts') -> Just (x,ts')
                               Nothing -> runParser p2 ts)

-- The idea is that "empty" is a parser that always fails, while
-- p1 <|> p2 is a parser that first tries to parse a string of tokens using p1,
-- and if that fails tries parsing the same string using p2.

-- Now we define parsers for various kinds of basic stuff.

-- The "token" parser just reads one token of the input and returns it.
-- Note there must be at least one token for item to succeed.
token :: Parser tok tok
token = Parser $ \ts -> case ts of
                          []     -> Nothing
                          (t:ts') -> Just (t,ts')

-- The "sat p" parser matches a token satisfying the predicate p.
sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t then return t else empty

-- Now we move onto our actual example of interest.

-- Our parsers will assume that the input string has already been split
-- up into a space-separated list of words, and thus use `String` as the
-- basic token type from now on.

-- It will be useful to have a parser that consumes a token matching a
-- specific string and ignoring case. This is achieved by "match s".
match :: String -> Parser String String
match s = sat (\s' -> map toLower s == map toLower s')

-- We parse English number words as numbers (restricted to numbers
-- between one and nine).
number :: Parser String Int
number = do
  (match "one" >> return 1)    <|> (match "two" >> return 2) <|>
   (match "three" >> return 3) <|> (match "four" >> return 4) <|>
   (match "five" >> return 5)  <|> (match "six" >> return 6) <|>
   (match "seven" >> return 7) <|> (match "eight" >> return 8) <|>
   (match "nine" >> return 9) <|> (match "ten" >> return 10) <|>
   (match "eleven" >> return 11) <|> (match "twelve" >> return 12) <|>
   (match "thirteen" >> return 13) <|> (match "fourteen" >> return 14) <|>
   (match "fifteen" >> return 15) <|> (match "sixteen" >> return 16) <|>
   (match "seventeen" >> return 17) <|> (match "eighteen" >> return 18) <|>
   (match "nineteen" >> return 19) <|> (match "twenty" >> return 20) <|>
   (match "1" >> return 1)    <|> (match "2" >> return 2) <|>
   (match "3" >> return 3) <|> (match "4" >> return 4) <|>
   (match "5" >> return 5)  <|> (match "6" >> return 6) <|>
   (match "7" >> return 7) <|> (match "8" >> return 8) <|>
   (match "9" >> return 9) <|> (match "10" >> return 10) <|>
   (match "11" >> return 11) <|> (match "12" >> return 12) <|>
   (match "13" >> return 13) <|> (match "14" >> return 14) <|>
   (match "15" >> return 15) <|> (match "16" >> return 16) <|>
   (match "17" >> return 17) <|> (match "18" >> return 18) <|>
   (match "19" >> return 19) <|> (match "20" >> return 20)

-- parseCmd is our general-purpose parser for commands, which can be either
-- inserting parameters, hint requests, climbing commands, meditation commands, or quitting.
parseCmd :: Parser String Cmd
parseCmd = parseHint <|> parseNumber <|> parseClimb <|> parseMeditate <|> parseQuit

-- parse a hint requests
parseHint :: Parser String Cmd
parseHint = do
  match "hint" >> return Hint

-- Parse a number parameter
parseNumber :: Parser String Cmd
parseNumber = do
  n <- number
  return (Num n)

-- Parse a climbing command.
parseClimb :: Parser String Cmd
parseClimb = do
  match "climb" <|> match "go"
  (match "down" >> return Go_Down) <|>
   (match "left" >> return Go_Left) <|>
   (match "right" >> return Go_Right)

-- Parse a 'take some rest'command.
parseMeditate :: Parser String Cmd
parseMeditate = do
  match "rest"
  match "for"
  n <- number
  if n == 1 then match "second" else match "seconds"
  return (Meditate n)

-- Parse a quit command
parseQuit :: Parser String Cmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit

-- Finally, we export a function that runs a parser on the entire input string, broken up into words.
-- This function runs in any MonadFail monad, to deal with the possiblity of failure.
parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s = case runParser p (words s) of
                   Just (x,ts') -> if null ts' then return x else fail "parseInput: some tokens left"
                   Nothing -> fail "parseInput: failed to parse"