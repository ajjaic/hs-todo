import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
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

parsePriority :: Parser Priority
parsePriority = do  char '('
                    c <- satisfy $ inClass "ABC"
                    char ')'
                    return $ pri c
    where
        pri 'A' = A
        pri 'B' = B
        pri 'C' = C

parseDate :: Parser UTCTime
parseDate = do date <- count 10 $ choice [digit, char '-']
               return $ (readTime defaultTimeLocale "%Y-%m-%d" date :: UTCTime)

parseProject :: Parser String
parseProject = do char '+'
                  prj <- many1 letter_ascii
                  return prj

parseContext :: Parser String
parseContext = do char '@'
                  ctx <- many1 letter_ascii
                  return ctx

parseContent :: Parser String
parseContent = do c <- sepBy1 parseWord space
                  return $ L.intercalate " " c
    where
        parseWord = do w <- many1 letter_ascii
                       return w
