import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L

data Priority = A | B | C deriving (Show)
data Task = Task { pri :: Maybe Priority,
                   added :: UTCTime,
                   done :: Maybe UTCTime,
                   taskc :: String,
                   prj :: Maybe String,
                   ctxt :: [String] } deriving (Show)

t1 = "(A) 2014-04-17 us doolars convert +home @weekend"
t2 = "2014-04-17 us doolars convert +home @weekend"
t3 = "2014-04-17 us doolars convert "
t4 = "2014-04-17 us doolars convert @weekend"

parsePriority :: Parser Priority
parsePriority = fmap pri $ char '(' *> satisfy (inClass "ABC") <* char ')' where
    pri p = case p of
        'A' -> A
        'B' -> B
        'C' -> C

parseDate :: Parser UTCTime
parseDate = (readTime defaultTimeLocale "%Y-%m-%d") <$> (count 10 $ choice [digit, char '-'])

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























