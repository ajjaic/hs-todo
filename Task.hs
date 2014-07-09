module Task (
    Sessionstate(..),
    Task(..),
    Taskadd(..),
    unknowncmd,
    programerror,
    info,
    lsProjects,
    lsContexts,
    lsProjectsM,
    lsContextsM,
    prioritycolorA,
    prioritycolorB,
    headingcolor,
    noprioritycolor,
    toMap,
    todoFile,
    prompt,
    parseTask,
    parseTaskAdd,
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
import Data.Maybe (mapMaybe)
import System.Console.Terminfo.Color (Color(..))
import System.Console.Terminfo.Base (Terminal)
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
dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%d %H:%M"

--TODO: The only priorities must be changed to ABab. No Cc.
parsePriority :: Parser (Maybe Priority)
parsePriority = optional (char '$' *> satisfy (inClass "ABab"))

parseDate :: Parser UTCTime
parseDate = readTime defaultTimeLocale dateTimeFormat
        <$> count 16 (choice [digit, char '-', char ':', space])

parseProject :: Parser (Maybe Project)
parseProject = optional (char '+' *> many1 (letter_ascii <|> digit))

parseContent :: Parser String
parseContent  = unwords <$> sepBy1 parseWord space where
    parseWord = many1 (satisfy (notInClass "+@$ "))

parseContext :: Parser [Context]
parseContext = sepBy' context space where
    context  = char '@' *> many1 (letter_ascii <|> digit)

parseTask :: Parser Task
parseTask = Task
    <$> parseDate <* space
    <*> optional (parseDate <* space)
    <*> parseContent
    <*> (optional space *> parseProject)
    <*> (optional space *> parseContext)
    <*> (optional space *> parsePriority)


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
    show t = unwords $ filter (not . null) telements where
        createtime  = fmttime (getTimeadded t)
        donetime    = maybe ""  fmttime (getTimedone t)
        taskcontent = getTask t
        project     = maybe "" ('+':) (getProject t)
        context     = unwords $ map ('@':) (getContext t)
        priority    = maybe "" (("$"++).(:[])) (getPriority t)
        fmttime     = formatTime defaultTimeLocale dateTimeFormat
        telements   = [createtime,donetime,taskcontent,project,context,priority]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Taskadd| type. Type that is used while adding a new task inside
-- REPL. The parser for this type parses plain text to create this
-- type.
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Taskadd = Taskadd { newContent  :: String,
                         newProject  :: Maybe Project,
                         newContext  :: [String],
                         newPriority :: Maybe Priority}

parseTaskAdd :: Parser Taskadd
parseTaskAdd = Taskadd
    <$> parseContent
    <*> (optional space *> parseProject)
    <*> (optional space *> parseContext)
    <*> (optional space *> parsePriority)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Sessionstate| type. This is used for keeping track of added and
-- modified tasks after which, the entire session is written to file
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO:Perhaps it would be better if we can have a seperate session
-- field to store the tasks as a list as well.
data Sessionstate = Sessionstate { sessionTasks    :: M.IntMap Task,
                                   sessionProjects :: [Project],
                                   sessionContexts :: [Context],
                                   sessionAutocomp :: [String],
                                   sessionTerminal :: Maybe Terminal }
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
lsProjects :: [Task] -> [Project]
lsProjects t = L.nub $ mapMaybe getProject t

lsContexts :: [Task] -> [Context]
lsContexts t = L.nub $ concatMap getContext t

lsProjectsM :: M.IntMap Task -> [Project]
lsProjectsM tm = L.nub $ mapMaybe getProject $ M.elems tm

lsContextsM :: M.IntMap Task -> [Context]
lsContextsM tm = L.nub $ concatMap getContext $ M.elems tm

toMap :: [Task] -> M.IntMap Task
--TODO: Perhaps this should not be a part of the API
toMap t = M.fromAscList $ zip [1..] t

insertTaskIntoMap :: Task -> M.IntMap Task -> M.IntMap Task
--TODO: Perhaps this should not be a part of the API
insertTaskIntoMap t tm = M.insert key t tm where
    key = if M.null tm then 1 else fst (M.findMax tm) + 1

deleteTaskFromMap :: M.IntMap Task -> Int -> M.IntMap Task
deleteTaskFromMap tm key = M.delete key tm

allTasksWithProject :: M.IntMap Task -> Maybe Project -> M.IntMap Task
allTasksWithProject tm Nothing = M.filter (( == Nothing) . getProject) tm
allTasksWithProject tm p       = M.filter (( == p) . getProject) tm

allTasksWithContext :: M.IntMap Task -> Maybe Context -> M.IntMap Task
allTasksWithContext tm Nothing  = M.filter (L.null . getContext) tm
allTasksWithContext tm (Just c) = M.filter ((c `elem`) . getContext) tm

allTasksWithPriority :: M.IntMap Task -> Maybe Priority -> M.IntMap Task
allTasksWithPriority tm Nothing = M.filter (( == Nothing) . getPriority) tm
allTasksWithPriority tm pr      = M.filter (( == pr) . getPriority) tm

unknowncmd :: String -> String
unknowncmd = (++) "Unknown: "

programerror :: String -> String
programerror = (++) "Error: "

info :: String -> String
info = (++) "Info: "

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Hardcoded Settings
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
todoFile :: String
todoFile = "todoapi.txt"

prompt :: String
prompt = ":: "

prioritycolorA :: Color
prioritycolorA = Red

prioritycolorB :: Color
prioritycolorB = Blue

noprioritycolor :: Color
noprioritycolor = Green

headingcolor :: Color
headingcolor = White
