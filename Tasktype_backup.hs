--Defines the actual |Task| type.
--Contains internal functions used for parsing various
--parts of the |Task| type.


-- TODO: You can't selectively choose to import some fully and
-- some partially
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import System.IO (withFile, IOMode(ReadMode))
import System.Console.Haskeline
import Data.Maybe (fromMaybe, catMaybes, isNothing, fromJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Lazy as M

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
parseDate = (readTime defaultTimeLocale datetimeformat)
        <$> (count 16 $ choice [digit, char '-', char ':', space])

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
    <*> optional parseProject <* (optional space)
    <*> option [] parseContext

parseInput :: Parser (String, Maybe String)
parseInput = (,)
    <$> many1 letter_ascii
    <*> optional (space *> many' anyChar)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Show instance for |Task|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
instance Show (Priority) where
    show A = "(A)"
    show B = "(B)"
    show C = "(C)"

instance Show (Task) where
    show t      = L.intercalate " " $ filter (not . null) $ p:ta:td:tk:pr:ct:[] where
        p       = maybe "" show (priority t)
        ta      = fmttime (timeadded t)
        td      = maybe "" fmttime (timedone t)
        tk      = task t
        pr      = fromMaybe "" $ makeProject <$> (project t)
        ct      = L.intercalate " " $ map makeContext $ context t
        fmttime = formatTime defaultTimeLocale datetimeformat


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- REPL Commands
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Command = Command {name :: String,
                        func :: Maybe String -> StateT Sessionstate (InputT IO) (),
                        desc :: String}

cmds = [Command   "ls"     listTasks      "List all the tasks",
        Command   "lsp"    listProjects   "List all the projects",
        Command   "lsc"    listContexts   "List all the contexts",
        Command   "pv"     projectView    "List all tasks projectwise",
        Command   "cv"     contextView    "List all tasks contextwise",
        Command   "h"      todoHelp       "Show this help",
        Command   "at"     addTask        "Create a new task",
        Command   "del"    deleteTask     "Delete an existing task",
        Command   "app"    appTask        "Append to an existing task"
       ]

getCmd :: String -> Maybe Command
getCmd c = if L.null cmds' then Nothing else Just (head cmds') where
    cmds' = filter helper cmds
    helper cmd = c == (name cmd)

deleteTask  = undefined
appTask     = undefined

data Taskadd = Taskadd
    {tapriority :: Maybe Priority,
     tatask     :: String,
     taproject  :: Maybe String,
     tacontext  :: [String]}

parseTaskAdd :: Parser Taskadd
parseTaskAdd = Taskadd
    <$> optional (parsePriority <* space)
    <*> parseContent <* (optional space)
    <*> (optional parseProject) <* (optional space)
    <*> option [] parseContext

todoHelp :: Maybe String -> StateT Sessionstate (InputT IO) ()
todoHelp Nothing = lift $ mapM_ outputStrLn $ map (\c -> uncurry (printf "%3s: %s") (name c, desc c)) cmds
todoHelp (Just c) = lift $ outputStrLn (maybe (unknowcmd c) desc (getCmd c))

addTask :: Maybe String -> StateT Sessionstate (InputT IO) ()
addTask Nothing     = lift $ outputStrLn "Nothing to add"
addTask (Just xtsk) = do
    case parseOnly parseTaskAdd (B.pack xtsk) of
        (Right r) -> do
            utc <- lift $ liftIO getCurrentTime
            ss  <- get
            let newpriority    = tapriority r
                newtaskcontent = tatask r
                newproject     = taproject r
                newcontexts    = tacontext r
                newtask        = Task newpriority utc Nothing newtaskcontent newproject newcontexts
                newtaskset     = M.insert 1 newtask $ M.mapKeys (+1) (tasks ss)
                newprojectset  = if isNothing newproject then (projects ss) else L.union (projects ss) [fromJust newproject]
                newcontextset  = if L.null newcontexts then (contexts ss) else L.union (contexts ss) newcontexts
                newss          = Sessionstate newtaskset newprojectset newcontextset
            put newss
        (Left l) -> lift $ outputStrLn l


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
    if L.null c
        then return ()
        else do
            mapM_ (\mc -> contextView mc >> (lift $ outputStrLn "")) (map Just (init c))
            contextView (Just $ last c)


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
    if L.null p
        then return ()
        else do
           mapM_ (\mp -> projectView mp >> (lift $ outputStrLn "")) (map Just (init p))
           projectView (Just $ last p)


listTasks :: Maybe String -> StateT Sessionstate (InputT IO) ()
listTasks _ = do
    ss <- get
    let tasksascending = M.assocs (M.map show $ tasks ss)
        printstrs = map (uncurry (printf "%3d -> %s")) tasksascending
    lift $ mapM_ outputStrLn printstrs

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
makeProject p = '+' : p

makeContext :: String -> String
makeContext c = '@' : c

unknowcmd c = "WTF is: " ++ c
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
                    Just cmdargs -> parsedinp cmdargs
    where
        parsedinp c = case parseOnly parseInput (B.pack c) of
            (Right (cmd, args)) -> (maybe (lift $ outputStrLn $ unknowcmd cmd) (($ args) . func) (getCmd cmd)) >> oneREPloop
            (Left error) -> (lift $ outputStrLn $ unknowcmd c) >> oneREPloop




