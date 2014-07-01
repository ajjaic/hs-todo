module Commands (
    Command(..),
    getCmd,
    cmdNames
) where

import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import System.Console.Haskeline (InputT, outputStrLn)
import Text.Printf (printf)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isNothing)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
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
data Command = Command {name :: String,
                        func :: Maybe String -> InputT (StateT Sessionstate IO) (),
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

cmdNames :: [String]
cmdNames = map name cmds

appTask      = undefined

todoHelp :: Maybe String -> InputT (StateT Sessionstate IO) ()
todoHelp Nothing  = mapM_ outputStrLn $ map (\c -> uncurry (printf "%3s -> %s") (name c, desc c)) cmds
todoHelp (Just c) = outputStrLn (maybe (unknowncmd c) desc (getCmd c))

deleteTask :: Maybe String -> InputT (StateT Sessionstate IO) ()
deleteTask Nothing  = return ()
deleteTask (Just n) = do
    ss <- lift get
    let intn          = (read n) :: Int
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
addTask (Just newtask) = do
    case parseOnly parseTaskAdd (B.pack newtask) of
        (Right r) -> do
            utc <- liftIO getCurrentTime
            ss  <- lift get
            let content         = newContent r
                project         = newProject r
                contexts        = newContext r
                priority        = newPriority r
                task            = Task utc Nothing content project contexts priority
                newtaskset      = if M.member 1 (sessionTasks ss) then M.insert 1 task $ M.mapKeys (+1) (sessionTasks ss) else M.insert 1 task (sessionTasks ss)
                newprojectset   = if isNothing project then (sessionProjects ss) else L.union (sessionProjects ss) [fromJust project]
                newcontextset   = if L.null contexts then (sessionContexts ss) else L.union (sessionContexts ss) contexts
                newautocomptags = let ac   = sessionAutocomp ss
                                      tags = if isNothing project then contexts else (fromJust project):contexts
                                   in L.union ac tags
                newss = Sessionstate newtaskset newprojectset newcontextset newautocomptags
            lift $ put newss
        (Left l) -> outputStrLn l


contextView :: Maybe String -> InputT (StateT Sessionstate IO) ()
contextView Nothing = do
    ss <- lift get
    let c = sessionContexts ss
    if L.null c
        then return ()
        else do
            mapM_ (\mc -> contextView mc >> outputStrLn "") (map Just (init c))
            contextView (Just $ last c)
contextView ctx@(Just c) = do
    ss <- lift get
    let t = M.toAscList $ M.map show $ allTasksWithContext (sessionTasks ss) ctx
        printtasks = map commandViewFormat t
    if L.null t
        then return ()
        else do outputStrLn (makeContext c) >> mapM_ outputStrLn printtasks

projectView :: Maybe String -> InputT (StateT Sessionstate IO) ()
projectView Nothing = do
    ss <- lift get
    let p = sessionProjects ss
    if L.null p
        then return ()
        else do
           mapM_ (\mp -> projectView mp >> outputStrLn "") (map Just (init p))
           projectView (Just $ last p)
projectView prj@(Just p) = do
    ss <- lift get
    let t          = M.toAscList $ M.map show $ allTasksWithProject (sessionTasks ss) prj
        printtasks = map commandViewFormat t
    if L.null t
        then return ()
        else outputStrLn (makeProject p) >> mapM_ outputStrLn printtasks

listContexts :: Maybe String -> InputT (StateT Sessionstate IO) ()
listContexts Nothing = do
    ss <- lift get
    let contexts  = zip [1 .. ] (lsContextsM $ sessionTasks ss)
        printstrs = map commandViewFormat contexts
    mapM_ outputStrLn printstrs
listContexts _ = return ()

listProjects :: Maybe String -> InputT (StateT Sessionstate IO) ()
listProjects Nothing = do
    ss <- lift get
    let projects  = zip [1 .. ] (lsProjectsM $ sessionTasks ss)
        printstrs = map commandViewFormat projects
    mapM_ outputStrLn printstrs
listProjects _ = return ()

listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
listTasks Nothing = do
    ss <- lift get
    let tasksascending = M.assocs (M.map show $ sessionTasks ss)
        printstrs      = map commandViewFormat tasksascending
    mapM_ outputStrLn printstrs
listTasks _ = return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

commandViewFormat :: (Int, String) -> String
commandViewFormat p = (uncurry (printf "%3d -> %s")) p

makeProject p = '+':p

makeContext c = '@':c
