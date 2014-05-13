{-
Defines the actual |Task| type.
Contains internal functions used for parsing various
parts of the |Task| type.
-}

module Tasktype(
    Priority(..),
    Task(..),
    parseTask,
    readTaskFile,
) where

import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import System.IO (withFile, IOMode(ReadMode))
import System.Console.Haskeline
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

data Priority = A | B | C
-- TODO: why does priority need to its own type. Doesn't solve
-- anything.
data Task = Task { priority :: Maybe Priority,
                   timeadded :: UTCTime,
                   timedone :: Maybe UTCTime,
                   task :: String,
                   project :: Maybe String,
                   context :: [String] }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parser for parsing various components |Task|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
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
parseProject = char '+' *> many1 (letter_ascii <|> digit)

parseContent :: Parser String
parseContent = L.intercalate " " <$> sepBy1 parseWord space where
    parseWord = many1 (letter_ascii <|> digit)

parseContext :: Parser [String]
parseContext = sepBy' context space where
    context = char '@' *> many1 (letter_ascii <|> digit)

parseTask :: Parser Task
parseTask = Task
    <$> optional (parsePriority <* space)
    <*> parseDate <* space
    <*> optional (parseDate <* space)
    <*> parseContent <* (optional space)
    <*> optional (parseProject <* space)
    <*> option [] parseContext

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Show instance for |Task|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
instance Show (Priority) where
    show A = "(A)"
    show B = "(B)"
    show C = "(C)"

instance Show (Task) where
    show t = L.intercalate " " $ filter (not . null) $ p:ta:td:tk:pr:ct:[] where
        p  = maybe "" show (priority t)
        ta = show (timeadded t)
        td = maybe "" show (timedone t)
        tk = task t
        pr = maybe "" id $ ('+':) <$> (project t)
        ct = L.intercalate " " $ map ('@':) $ context t

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Read |Task| from file
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
readTaskFile :: FilePath -> IO [Task]
-- Proper error handling required
readTaskFile f = withFile f ReadMode helper where
    helper h = do
        bytes <- B.hGetContents h
        let lines = B.lines bytes
        return $ map getTask lines
    getTask :: B.ByteString -> Task
    getTask b = let getRight (Right t) = t
                in getRight $ parseOnly parseTask b

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- REPL Commands
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
cmds = [("ls",   listTasks,   "List all the tasks"),
        ("pv",   projectView, "List all tasks projectwise"),
        ("cv",   contextView, "List all tasks contextwise"),
        ("help", help,        "Show this help"),
        ("at",   addTask,     "Create a new task"),
        ("del",  delTask,     "Delete an existing task"),
        ("app",  appTask,     "Append to an existing task")]

listTasks :: [Task] -> String -> InputT IO ()
listTasks t _ = mapM_ outputStrLn $ map show t

projectView :: [Task] -> String -> InputT IO ()
projectView tasks p = mapM_ outputStrLn
    $ map show
    $ filter (helper . project) tasks where
        helper (Just prj) = prj == p
        helper _ = False

contextView = undefined
help        = undefined
addTask     = undefined
delTask     = undefined
appTask     = undefined


execCmd :: [Task] -> (String, String) -> InputT IO ()
execCmd t (c, args) = if L.null cmd'
                        then outputStrLn ("WTF is: " ++ c)
                        else let (_, cmd, _) = head cmd'
                              in cmd t args
    where
        cmd' = filter (\(c',_,_) -> c == c') cmds
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helper functions
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
todoFile :: String
todoFile = "todo.txt"

prompt :: String
prompt = ">> "

blankLines :: Int -> IO ()
blankLines n = putStr $ replicate n '\n'

-- -- -- --  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- REPL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

oneREPloop :: [Task] -> InputT IO ()
oneREPloop t = do
    c <- getInputLine prompt
    case c of
        Nothing -> return ()
        Just "" -> oneREPloop t
        Just "quit" -> return ()
        Just cmdargs -> do execCmd t $ mbreak cmdargs ""
                           oneREPloop t
    where
        mbreak (x:xs) s = if isSpace x then (s, xs) else mbreak xs (s ++ [x])
        mbreak [] s = (s, "")

main :: IO ()
main = do
    tasks <- readTaskFile todoFile
    runInputT defaultSettings $ oneREPloop tasks








































