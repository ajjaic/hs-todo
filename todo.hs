import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L

data Priority = A | B | C deriving (Show)
data Task = Task { taskc :: String,
                   addedt :: UTCTime,
                   donedt :: Maybe UTCTime,
                   prj :: Maybe String,
                   pri :: Maybe Priority,
                   ctxt :: Maybe [String] } deriving (Show)

t = "(A) 2014-04-17 us doolars convert +home @weekend"
p = "(A)"
d = "2014-04-17"
c = "us doolars convert"
pr = "+home"
ct = "@weekend"
test = "us doolars convert +makerspace"

parsePriority :: Parser (Maybe Priority)
parsePriority = Just <$> parsePriority' <|> pure Nothing where
    parsePriority' = fmap pri
        $ char '('
       *> satisfy (inClass "ABC")
       <* char ')' where
            pri p = case p of
                'A' -> A
                'B' -> B
                'C' -> C

parseAddedDate :: Parser UTCTime
parseAddedDate = (readTime defaultTimeLocale "%Y-%m-%d") <$>
    (count 10 $ choice [digit, char '-'])

parseDoneDate :: Parser (Maybe UTCTime)
parseDoneDate = Just
    <$> readTime defaultTimeLocale "%Y-%m-%d"
    <$> count 10 (choice [digit, char '-'])
    <|> pure Nothing

parseProject :: Parser String
parseProject = char '+' *> many1 letter_ascii

parseContext :: Parser String
parseContext = char '@' *> many1 letter_ascii

parseContent :: Parser String
parseContent = L.intercalate " "
    <$> sepBy1 parseWord space where
            parseWord = many1 letter_ascii

--parseTask :: Task
--parseTask = Task <$>
--    many
--parseTask :: Task
--parseTask = Task <$>
--    parsePriority <*
--    space <*>
--    parseAddedDate <*
--    space <*>
--    parseContent <*
--
--    parseProject <*
--    space <*>


























