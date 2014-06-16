module Task (
    Sessionstate(..),
    Task(..),
    unknowncmd,
    lsProjects,
    lsContexts,
    lsProjectsM,
    lsContextsM,
    toMap,
    parseTask,
    parseInput,
    insertTaskIntoMap,
    deleteTaskFromMap,
    allTasksWithProject,
    allTasksWithContext,
    allTasksWithPriority
) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, readTime)
import System.Locale (defaultTimeLocale)
import Control.Applicative (optional, (<*>), (<*), (*>), (<$>), (<|>))
import Data.Attoparsec.Combinator (count, choice, sepBy1, sepBy', many1, many')
import Data.Attoparsec.ByteString.Char8 (Parser, char, satisfy, inClass, digit, space, notInClass, letter_ascii, anyChar)
import Data.Maybe (maybe, catMaybes)
import qualified Data.List as L
import qualified Data.IntMap.Lazy as M

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Task| type
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Project = String
type Context = String
type Priority = Char
data Task = Task { getTimeadded :: UTCTime,
                   getTimedone  :: Maybe UTCTime,
                   getTask      :: String,
                   getProject   :: Maybe Project,
                   getContext   :: [Context],
                   getPriority  :: Maybe Priority }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parsers for parsing various components |Task|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
dateTimeFormat = "%Y-%m-%d %H:%M"

parsePriority :: Parser (Maybe Priority)
parsePriority = optional $ (char '$' *> satisfy (inClass "ABCabc"))

parseDate :: Parser UTCTime
parseDate = (readTime defaultTimeLocale dateTimeFormat)
        <$> (count 16 $ choice [digit, char '-', char ':', space])

parseProject :: Parser (Maybe Project)
parseProject = optional $ (char '+' *> many1 (letter_ascii <|> digit))

parseContent :: Parser String
parseContent  = L.intercalate " " <$> sepBy1 parseWord space where
    parseWord = many1 (satisfy (notInClass "+@$ "))

parseContext :: Parser [Context]
parseContext = sepBy' context space where
    context  = char '@' *> many1 (letter_ascii <|> digit)

parseTask :: Parser Task
parseTask = Task
    <$> parseDate <* space
    <*> optional (parseDate <* space)
    <*> parseContent
    <*> ((optional space) *> parseProject)
    <*> ((optional space) *> parseContext)
    <*> ((optional space) *> parsePriority)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parser for parsing the command and arguments of the command
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
parseInput :: Parser (String, Maybe String)
parseInput = (,)
    <$> many1 letter_ascii
    <*> optional (space *> many' anyChar)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Show instance for |Task|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
instance Show (Task) where
    show t = L.intercalate " " $ filter (not . null) $ telements where
        createtime  = fmttime (getTimeadded t)
        donetime    = maybe ""  fmttime (getTimedone t)
        taskcontent = getTask t
        project     = maybe "" ('+':) (getProject t)
        context     = L.intercalate " " $ map ('@':) (getContext t)
        priority    = maybe "" ((['$']++).(:[])) (getPriority t)
        fmttime     = formatTime defaultTimeLocale dateTimeFormat
        telements   = createtime:donetime:taskcontent:project:context:priority:[]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Taskadd| type. Type that is used while adding a new task inside
-- REPL. The parser for this type parses plain text to create this
-- type.
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Taskadd = Taskadd { tatask     :: String,
                         taproject  :: Maybe Project,
                         tacontext  :: [String],
                         tapriority :: Maybe Priority}

parseTaskAdd :: Parser Taskadd
parseTaskAdd = Taskadd
    <$> parseContent
    <*> ((optional space) *> parseProject)
    <*> ((optional space) *> parseContext)
    <*> ((optional space) *> parsePriority)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Sessionstate| type. This is used for keeping track of added and
-- modified tasks after which, the entire session is written to file
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO:Perhaps it would be better if we can have a seperate session
-- field to store the tasks as a list as well.
data Sessionstate = Sessionstate { sessionTasks    :: M.IntMap Task,
                                   sessionProjects :: [Project],
                                   sessionContexts :: [Context],
                                   sessionAutocomp :: [String]}
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
lsProjects :: [Task] -> [String]
lsProjects t = L.nub $ catMaybes $ map getProject t

lsContexts :: [Task] -> [String]
lsContexts t = L.nub $ concat $ map getContext t

lsProjectsM :: M.IntMap Task -> [String]
lsProjectsM tm = L.nub $ catMaybes $ map getProject $ M.elems tm

lsContextsM :: M.IntMap Task -> [String]
lsContextsM tm = L.nub $ concat $ map getContext $ M.elems tm

toMap :: [Task] -> M.IntMap Task
toMap t = M.fromAscList $ zip [1..] t

insertTaskIntoMap :: Task -> M.IntMap Task -> M.IntMap Task
insertTaskIntoMap t tm = M.insert key t tm where
    key = if M.null tm then 1 else (fst $ M.findMax tm) + 1

deleteTaskFromMap :: M.IntMap Task -> Int -> M.IntMap Task
deleteTaskFromMap tm key = M.delete key tm

allTasksWithProject :: M.IntMap Task -> Project -> [Task]
allTasksWithProject tm p = filter helper $ M.elems tm where
    helper = ((==(Just p)) . getProject)

allTasksWithContext :: M.IntMap Task -> Context -> [Task]
allTasksWithContext tm c = filter helper $ M.elems tm where
    helper = (c `elem`) . getContext

allTasksWithPriority :: M.IntMap Task -> Priority -> [Task]
allTasksWithPriority tm pr = filter helper $ M.elems tm where
    helper t = (==(Just pr)) $ getPriority t

unknowncmd c = "WTF is: " ++ c

