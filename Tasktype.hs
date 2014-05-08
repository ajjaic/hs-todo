{-
Defines the actual "Task" type.
Contains internal functions used for parsing various
parts of the "Task" type.
-}
module Tasktype(
    Priority(..),
    Task(..),
    parseTask
) where

import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.List as L

data Priority = A | B | C deriving (Show)
data Task = Task { priority :: Maybe Priority,
                   timeadded :: UTCTime,
                   timedone :: Maybe UTCTime,
                   task :: String,
                   project :: Maybe String,
                   context :: [String] } deriving (Show)

parsePriority :: Parser Priority
parsePriority = fmap pri
    $ char '('
   *> satisfy (inClass "ABC")
   <* char ')' where
        pri p = case p of
            'A' -> A
            'B' -> B
            'C' -> C

parseDate :: Parser UTCTime
parseDate = (readTime defaultTimeLocale "%Y-%m-%d")
        <$> (count 10 $ choice [digit, char '-'])

parseProject :: Parser String
parseProject = char '+' *> many1 letter_ascii

parseContent :: Parser String
parseContent = L.intercalate " " <$> sepBy1 parseWord space where
    parseWord = many1 letter_ascii

parseContext :: Parser [String]
parseContext = sepBy' context space where
    context = char '@' *> many1 letter_ascii

parseTask :: Parser Task
parseTask = Task
    <$> optional (parsePriority <* space)
    <*> parseDate <* space
    <*> optional (parseDate <* space)
    <*> parseContent
    <*> optional (space *> parseProject <* space)
    <*> option [] parseContext

