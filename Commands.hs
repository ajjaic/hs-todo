module Commands (
    Command(..),
    getCmd,
    cmdNames
) where

import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Monad (unless)
import System.Console.Haskeline (InputT, outputStrLn)
import Control.Applicative (pure, (<*>))
import Text.Printf (printf)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe, fromJust, isNothing)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import System.Console.Terminfo.Color (withForegroundColor, Color)
import System.Console.Terminfo.Base (getCapability)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Lazy as M
import qualified Data.List as L

import Task
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- REPL Commands
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Type for |Command|
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Command = Command {
    name :: String,
    func :: Maybe String -> InputT (StateT Sessionstate IO) (),
    desc :: String}

cmds :: [Command]
cmds = [Command   "ls"     listTasks      "ls [task search]. List all the tasks",
        Command   "lsp"    listProjects   "List all the projects",
        Command   "lsc"    listContexts   "List all the contexts",
        Command   "pv"     projectView    "pv [project name]. List all tasks projectwise",
        Command   "cv"     contextView    "cv [context name]. List all tasks contextwise",
        Command   "h"      todoHelp       "Show this help",
        Command   "at"     addTask        "at <text> [+project] [@context..] [$priority]. Create a new task. The priority can either be A, B or C",
        Command   "del"    deleteTask     "del <task number>. Delete an existing task"
       ]

getCmd :: String -> Maybe Command
getCmd c = if L.null cmds' then Nothing else Just (head cmds') where
    cmds' = filter helper cmds
    helper cmd = c == name cmd

cmdNames :: [String]
cmdNames = map name cmds

todoHelp :: Maybe String -> InputT (StateT Sessionstate IO) ()
todoHelp Nothing  = mapM_ (outputStrLn . (\c -> uncurry (printf "%3s -> %s") (name c, desc c))) cmds
todoHelp (Just c) = outputStrLn (maybe (unknowncmd c) desc (getCmd c))

deleteTask :: Maybe String -> InputT (StateT Sessionstate IO) ()
deleteTask Nothing  = return ()
deleteTask (Just n) = do
    ss <- lift get
    let intn          = read n :: Int
        taskset       = sessionTasks ss
        newtaskset    = deleteTaskFromMap taskset intn
        newprojectset = lsProjectsM newtaskset
        newcontextset = lsContextsM newtaskset
        newss         = ss {sessionTasks = newtaskset, sessionProjects = newprojectset, sessionContexts = newcontextset}
    if not $ M.member intn taskset
        then outputStrLn $ programerror "The task does not exist"
        else lift (put newss)


addTask :: Maybe String -> InputT (StateT Sessionstate IO) ()
addTask Nothing     = return ()
addTask (Just newtask) = case parseOnly parseTaskAdd (B.pack newtask) of
        (Right r) -> do
            utc <- liftIO getCurrentTime
            ss  <- lift get
            let content         = newContent r
                project         = newProject r
                contexts        = newContext r
                priority        = newPriority r
                task            = Task utc Nothing content project contexts priority
                newtaskset      = insertTaskIntoMap task (sessionTasks ss)
                newprojectset   = if isNothing project then sessionProjects ss else sessionProjects ss `L.union` [fromJust project]
                newcontextset   = if L.null contexts then sessionContexts ss else sessionContexts ss `L.union` contexts
                newautocomptags = let ac   = sessionAutocomp ss
                                      tags = if isNothing project then contexts else fromJust project:contexts
                                   in L.union ac tags
                newss = ss {sessionTasks=newtaskset, sessionProjects=newprojectset, sessionContexts=newcontextset, sessionAutocomp=newautocomptags}
            lift $ put newss
            outputStrLn (info "Task Added")
            ((>>= mapM_ outputStrLn) . colorTask) $ M.singleton 1 task
        (Left l) -> outputStrLn (programerror l)

contextView :: Maybe String -> InputT (StateT Sessionstate IO) ()
contextView Nothing = do
    ss <- lift get
    let c = sessionContexts ss
    unless (L.null c) $ do
            mapM_ ((>> outputStrLn "") . contextView . Just) (init c)
            contextView (Just $ last c)
contextView ctx@(Just c) = do
    ss <- lift get
    let tasks = allTasksWithContext (sessionTasks ss) ctx
        ctxf = makeContext c
    unless (M.null tasks) $ ((>>= outputStrLn) . applyColor headingcolor) ctxf
                     >> ((>>= mapM_ outputStrLn) . colorTask) tasks

projectView :: Maybe String -> InputT (StateT Sessionstate IO) ()
projectView Nothing = do
    ss <- lift get
    let p = sessionProjects ss
    unless (L.null p) $ do
           mapM_ ((>> outputStrLn "") . projectView . Just) (init p)
           projectView (Just $ last p)
projectView prj@(Just p) = do
    ss <- lift get
    let tasks = allTasksWithProject (sessionTasks ss) prj
        prjf = makeProject p
    unless (M.null tasks) $ ((>>= outputStrLn) . applyColor headingcolor) prjf
                         >> ((>>= mapM_ outputStrLn) . colorTask) tasks


listContexts :: Maybe String -> InputT (StateT Sessionstate IO) ()
listContexts Nothing = do
    ss <- lift get
    let contexts  = zip [1 .. ] (sessionContexts ss)
    coloured <- mapM (applyColor headingcolor . commandViewFormat) contexts
    mapM_ outputStrLn coloured
listContexts _ = return ()

listProjects :: Maybe String -> InputT (StateT Sessionstate IO) ()
listProjects Nothing = do
    ss <- lift get
    let projects  = zip [1 .. ] (sessionProjects ss)
    coloured <- mapM (applyColor headingcolor . commandViewFormat) projects
    mapM_ outputStrLn coloured
listProjects _ = return ()

listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
listTasks Nothing = do
    ss <- lift get
    tasktoprint <- colorTask (sessionTasks ss)
    mapM_ outputStrLn tasktoprint
listTasks _ = return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
applyColor :: Color -> String -> InputT (StateT Sessionstate IO) String
applyColor c str = do
    ss <- lift get
    let terminal  = sessionTerminal ss
    case terminal of
        Nothing -> return str
        Just t -> return $ fromMaybe str $ getCapability t $ withForegroundColor <*> pure c <*> pure str

colorForPriority :: Task -> Color
colorForPriority t = case getPriority t of
    Just 'A' -> prioritycolorA
    Just 'a' -> prioritycolorA
    Just 'B' -> prioritycolorB
    Just 'b' -> prioritycolorB
    _        -> noprioritycolor

colorTask :: M.IntMap Task -> InputT (StateT Sessionstate IO) [String]
colorTask taskmap = do
    ss <- lift get
    let terminal = sessionTerminal ss
    case terminal of
        Nothing -> return $ map commandViewFormat $ M.assocs (M.map show taskmap)
        Just t  -> return $ mapMaybe (applycolor t) $ M.assocs taskmap
    where
        applycolor term (k, tsk) = getCapability term
                                 $ withForegroundColor
                               <*> pure (colorForPriority tsk)
                               <*> pure (commandViewFormat (k, show tsk))


commandViewFormat :: (Int, String) -> String
commandViewFormat = uncurry (printf "%3d -> %s")

makeProject :: String -> String
makeProject p = '+':p

makeContext :: String -> String
makeContext c = '@':c
