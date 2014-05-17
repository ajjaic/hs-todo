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
import Data.Maybe (fromMaybe, catMaybes)
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
        pr = fromMaybe "" $ ('+':) <$> (project t)
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
        ("lsp",  listProjects,"List all the projects"),
        ("lsc",  listContexts,"List all the contexts"),
        ("pv",   projectView, "List all tasks projectwise"),
        ("cv",   contextView, "List all tasks contextwise"),
        ("help", help,        "Show this help"),
        ("at",   addTask,     "Create a new task"),
        ("del",  delTask,     "Delete an existing task"),
        ("app",  appTask,     "Append to an existing task")]

listTasks :: [Task] -> Maybe String -> InputT IO ()
listTasks t _ = mapM_ outputStrLn $ map show t

projectView :: [Task] -> Maybe String -> InputT IO ()
projectView tasks Nothing = mapM_ allprjs $ lsprojects tasks where
    allprjs p = outputStrLn (makeProject p) >> projectView tasks (Just p)
projectView tasks p = mapM_ outputStrLn
    $ map show
    $ filter (helper . project) tasks where
        helper prj = prj == p

contextView :: [Task] -> Maybe String -> InputT IO ()
contextView t Nothing = mapM_ allctxts $ lscontexts t where
    allctxts c = outputStrLn (makeContext c) >> contextView t (Just c)
contextView t (Just c) = mapM_ outputStrLn
    $ map show
    $ filter (helper . context) t where
        helper = any (==c)

listProjects :: [Task] -> Maybe String -> InputT IO ()
listProjects t _ = mapM_ outputStrLn $ lsprojects t

listContexts :: [Task] -> Maybe String -> InputT IO ()
listContexts t _ = mapM_ outputStrLn $ lscontexts t

help        = undefined
addTask     = undefined
delTask     = undefined
appTask     = undefined


execCmd :: [Task] -> (String, Maybe String) -> InputT IO ()
execCmd t (c, args) = getcmd where
    getcmd
        | L.null cmd' = outputStrLn (unknownCmd ++ c)
        | otherwise = cmd t args
    cmd' = filter (\(c',_,_) -> c == c') cmds
    (_,cmd, _) = head cmd'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helper functions
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
todoFile :: String
todoFile = "todo.txt"

prompt :: String
prompt = ">> "

unknownCmd :: String
unknownCmd = "WTF is: "

blankLines :: Int -> IO ()
blankLines n = putStr $ replicate n '\n'

lsprojects :: [Task] -> [String]
lsprojects t = L.nub $ catMaybes $ map project t

lscontexts :: [Task] -> [String]
lscontexts t = L.nub $ concat $ map context t

makeProject :: String -> String
makeProject p = "+" ++ p

makeContext :: String -> String
makeContext c = "@" ++ c
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
        mbreak :: String -> String -> (String, Maybe String)
        mbreak (x:xs) s = if isSpace x then (s, Just xs) else mbreak xs (s ++ [x])
        mbreak [] s = (s, Nothing)

main :: IO ()
main = do
    tasks <- readTaskFile todoFile
    runInputT defaultSettings $ oneREPloop tasks








































