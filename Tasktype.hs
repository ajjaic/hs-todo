--Defines the actual |Task| type.
--Contains internal functions used for parsing various
--parts of the |Task| type.

{-module Tasktype( -}
{-    Priority(..),-}
{-    Task(..),    -}
{-    parseTask,   -}
{-    readTaskFile,-}
{-) where          -}

import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import System.IO (withFile, IOMode(ReadMode))
import System.Console.Haskeline
import Data.Maybe (fromMaybe, catMaybes, isNothing, fromJust)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Lazy as M
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)

data Priority = A | B | C
-- TODO: Why does priority need to its own type. Doesn't solve
-- anything.
data Task = Task { priority  :: Maybe Priority,
                   timeadded :: UTCTime,
                   timedone  :: Maybe UTCTime,
                   task      :: String,
                   project   :: Maybe String,
                   context   :: [String] }

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
--TODO: Content can be anything. At the moment accepts only letters
--and digits
parseContent  = L.intercalate " " <$> sepBy1 parseWord space where
    parseWord = many1 (letter_ascii <|> digit)

parseContext :: Parser [String]
parseContext = sepBy' context space where
    context  = char '@' *> many1 (letter_ascii <|> digit)

parseTask :: Parser Task
parseTask = Task
    <$> optional (parsePriority <* space)
    <*> parseDate <* space
    <*> optional (parseDate <* space)
    <*> parseContent <* (optional space)
    <*> optional (parseProject <* space)
    <*> option [] parseContext

parseInput :: Parser (String, String)
parseInput = (,)
    <$> many1 letter_ascii
    <*> option "" (space *> many' anyChar)
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
-- TODO: Proper error handling required
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
data Command = Command {name :: String,
                        func :: Maybe String -> StateT Sessionstate (InputT IO) (),
                        desc :: String}

ls   = Command   "ls"     listTasks      "List all the tasks"
lsp  = Command   "lsp"    listProjects   "List all the projects"
lsc  = Command   "lsc"    listContexts   "List all the contexts"
pv   = Command   "pv"     projectView    "List all tasks projectwise"
cv   = Command   "cv"     contextView    "List all tasks contextwise"
help = Command   "help"   todoHelp           "Show this help"
at   = Command   "at"     addTask        "Create a new task"
del  = Command   "del"    deleteTask     "Delete an existing task"
app  = Command   "app"    appTask        "Append to an existing task"
cmds = [ls, lsp, lsc, pv, cv, help, at, del, app]


todoHelp    = undefined
addTask     = undefined
deleteTask  = undefined
appTask     = undefined

contextView :: Maybe String -> StateT Sessionstate (InputT IO) ()
contextView (Just c) = do
    ss <- get
    let t = M.elems $ tasks ss
        ctxt = filter ((any (==c)) . context) t
    if L.null ctxt
        then return ()
        else do lift $ outputStrLn (makeContext c)
                    >> mapM_ (outputStrLn . show) ctxt
contextView _ = do
    ss <- get
    let c = contexts ss
        (c', c'') = (init c, last c)
        c'v = mapM_ (\cc -> contextView cc  >> lift (outputStrLn "")) (map Just c')
        c''v = contextView (Just c'')
    if length c == 1
        then contextView (Just (head c))
        else c'v >> c''v


projectView :: Maybe String -> StateT Sessionstate (InputT IO) ()
projectView prj@(Just p) = do
    ss <- get
    let t    = M.elems $ tasks ss
        prjt = filter ((==prj) . project) t
    if L.null prjt
        then return ()
        else do lift $ outputStrLn (makeProject p)
                    >> mapM_ (outputStrLn . show) prjt
projectView _ = do
    ss <- get
    let p = projects ss
        (p', p'') = (init p, last p)
        p'v = mapM_ (\pp -> projectView pp  >> lift (outputStrLn "")) (map Just p')
        p''v = projectView (Just p'')
    if length p == 1
        then projectView (Just $ head p)
        else p'v >> p''v


listTasks :: Maybe String -> StateT Sessionstate (InputT IO) ()
listTasks _ = get
    >>= (\s -> lift $ mapM_ (outputStrLn . show) $ M.elems $ tasks s)

listProjects :: Maybe String -> StateT Sessionstate (InputT IO) ()
listProjects _ = do
    ss <- get
    lift $ mapM_ (outputStrLn . makeProject) $ projects ss

listContexts :: Maybe String -> StateT Sessionstate (InputT IO) ()
listContexts _ = do
    ss <- get
    lift $ mapM_ (outputStrLn . makeContext) $ contexts ss
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helper functions
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
todoFile :: String
todoFile = "todo.txt"

prompt :: String
prompt = ">> "

lsprojects :: [Task] -> [String]
lsprojects t = L.nub $ catMaybes $ map project t

lscontexts :: [Task] -> [String]
lscontexts t = L.nub $ concat $ map context t

makeProject :: String -> String
makeProject p = "+" ++ p

makeContext :: String -> String
makeContext c = "@" ++ c


lookupFunction :: String -> Maybe (Maybe String -> StateT Sessionstate (InputT IO) ())
lookupFunction cmd =  if L.null cmd' then Nothing else Just $ func $ head cmd' where
    cmd' = filter (\c -> (name c) == cmd) cmds

-- -- -- --  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- REPL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- TODO:Perhaps it would be better if we can have a seperate session
-- field to store the tasks as a list as well.
data Sessionstate = Sessionstate { tasks    :: M.Map Int Task,
                                   projects :: [String],
                                   contexts :: [String]} deriving (Show)

main :: IO ()
main = do
-- TODO:What if the tasks are empty?
    tasks <- readTaskFile todoFile
    let taskmap  = M.fromAscList $ zip [1 ..] tasks
        projects = lsprojects tasks
        contexts = lscontexts tasks
        ss       = Sessionstate taskmap projects contexts
    runInputT defaultSettings $ evalStateT oneREPloop ss

oneREPloop :: StateT Sessionstate (InputT IO) ()
oneREPloop = do c <- lift $ getInputLine prompt
                return ()
                case c of
                    Nothing      -> return ()
                    Just ""      -> oneREPloop
                    Just "quit"  -> return ()
                    Just "exit"  -> return ()
                    Just cmdargs -> let (cmd, args) = mbreak cmdargs ""
                                        cmd' = lookupFunction cmd
                                     in if isNothing cmd'
                                            then unknowcmd cmd
                                            else applycmd (fromJust cmd') args
    where
        mbreak (x:xs) s = if isSpace x then (s, Just xs) else mbreak xs (s ++ [x])
        mbreak [] s     = (s, Nothing)
        applycmd c args = (c args) >> oneREPloop
        unknowcmd c     = (lift $ outputStrLn ("WTF is: " ++ c)) >> oneREPloop





































