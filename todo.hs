import Data.Time.Clock
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

data Priority = A | B | C deriving (Show)
data Task = Task { taskc :: String,
                   addedt :: UTCTime,
                   donedt :: Maybe UTCTime,
                   prj :: Maybe String,
                   pri :: Maybe Priority,
                   ctxt :: Maybe [String] } deriving (Show)

t = "(A) 2014-04-17 us doolars convert +home @weekend"
d = "2014-04-17"

parsePriority :: Parser Priority
parsePriority = do char '('
                   c <- satisfy $ inClass "ABC"
                   char ')'
                   return $ pri c
    where
        pri c
          | c == 'A' = A
          | c == 'B' = B
          | c == 'C' = C



