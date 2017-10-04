{-# LANGUAGE FlexibleContexts #-}
{- |
   Module      :  Parse
   Description :  Parsers for QuText.
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Tue Oct  3 15:33:29 2017.

Parse for targets, commands, etc.

* Changelog

-}

module Parse where

import Text.Parsec
import Control.Applicative (liftA2)

-- * Types

-- | A target is just a string.
type Target = String

-- | The source, i.e., the host that the file is tailored for at the moment.
type Source = Target

-- | The destination, i.e., the host that the file will be tailored for.
type Dest = Target

-- | A list of possible commands.
data Command = Targets | Current | Only
             deriving (Show, Eq)

-- | Instructions consist of a command and maybe also a list of targets.
data Instruction = Instruction { instrCmd :: Command
                               , instrTgs :: [Target] }
                   deriving (Show, Eq)

word :: Stream s m Char => ParsecT s u m String
word = many1 alphaNum

-- | Wrap a character 'c' around a parser. Do not return the character, but only
-- the parser.
wrap :: Stream s m Char => Char -> ParsecT s u m String -> ParsecT s u m String
wrap c w = char c *> w <* char c

-- | Wrap a character 'c' 'n' times around a parser. Do not return the
-- character, but only the parser.
wrapN :: Stream s m Char => Char -> Int -> ParsecT s u m String -> ParsecT s u m String
wrapN c n w = iterate (wrap c) w !! n

-- * Identifiers

-- | The character that identifies a comment.
cmtChar :: Char
cmtChar = '#'

-- | The character that identifies a command.
cmdChar :: Char
cmdChar = '.'

-- | The character that identifies a target.
targetChar :: Char
targetChar = '='

-- * Commands

-- | The command to identify the 'Source'.
toCmd :: String -> Command
toCmd "targets" = Targets
toCmd "current" = Current
toCmd "only"    = Only
toCmd s = error $ "Unknown command: " ++ s

-- | Get the command out of a wrapped command.
cmd :: Stream s m Char => ParsecT s u m Command
cmd = toCmd <$> wrapN cmdChar 2 word

-- * Parsers

-- | Get the instruction from a line.
instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = liftA2 Instruction cmdInline targetsInline
  where cmdInline = spaces *> many1 (char cmtChar) *> spaces *> cmd <* spaces
        targetsInline = many (target <* spaces) <* skipMany anyChar <* optional endOfLine

-- | Look for an instruction with a specific command.
-- instructionSpec :: Stream s m Char => Command -> ParsecT s u m Instruction

instructionParser :: String -> Either ParseError Instruction
instructionParser = parse instruction ""

-- -- | Parse a wrapped target.
target :: Stream s m Char => ParsecT s u m Target
target = wrap targetChar word
