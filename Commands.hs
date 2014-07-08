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
import Data.Maybe (fromJust, isNothing)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import System.Console.Terminfo.Color (withForegroundColor, restoreDefaultColors)
import System.Console.Terminfo.Base (Terminal, getCapability, runTermOutput, termText)
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
                newtaskset      = M.insert 1 task (if M.member 1 (sessionTasks ss) then M.mapKeys (+1) (sessionTasks ss) else sessionTasks ss)
                newprojectset   = if isNothing project then sessionProjects ss else sessionProjects ss `L.union` [fromJust project]
                newcontextset   = if L.null contexts then sessionContexts ss else sessionContexts ss `L.union` contexts
                newautocomptags = let ac   = sessionAutocomp ss
                                      tags = if isNothing project then contexts else fromJust project:contexts
                                   in L.union ac tags
                newss = ss {sessionTasks=newtaskset, sessionProjects=newprojectset, sessionContexts=newcontextset, sessionAutocomp=newautocomptags}
            lift $ put newss
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
    let t = M.toAscList $ M.map show $ allTasksWithContext (sessionTasks ss) ctx
        printtasks = map commandViewFormat t
    unless (L.null t) $ outputStrLn (makeContext c) >> mapM_ outputStrLn printtasks

projectView :: Maybe String -> InputT (StateT Sessionstate IO) ()
projectView Nothing = do
    ss <- lift get
    let p = sessionProjects ss
    unless (L.null p) $ do
           mapM_ ((>> outputStrLn "") . projectView . Just) (init p)
           projectView (Just $ last p)
projectView prj@(Just p) = do
    ss <- lift get
    let t          = M.toAscList $ M.map show $ allTasksWithProject (sessionTasks ss) prj
        printtasks = map commandViewFormat t
    unless (L.null t) $ outputStrLn (makeProject p) >> mapM_ outputStrLn printtasks

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

--listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
--listTasks Nothing = do
    --ss <- lift get
    --let tasksascending = M.assocs (M.map show $ sessionTasks ss)
        --printstrs      = map commandViewFormat tasksascending
    --mapM_ outputStrLn printstrs
----TODO: make tasks searcheable
--listTasks _ = return ()

listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
listTasks Nothing = do
    ss <- lift get
    liftIO $ printColoredTasks (sessionTasks ss) (sessionTerminal ss)
listTasks _ = return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

{-applyColor :: String -> InputT (StateT Sessionstate IO) ()-}
{-applyColor s = do-}
    {-ss <- lift get-}
    {-let terminal = sessionTerminal ss-}
        {-capability = maybe (Just (termText s)) (\t -> getCapability t (withForegroundColor <*> pure prioritycolorA <*> pure (termText s))) (sessionTerminal ss)-}
    {-liftIO $ runTermOutput (fromJust terminal) (fromJust capability)-}

printColoredTasks :: M.IntMap Task -> Maybe Terminal -> IO ()
printColoredTasks taskmap terminal = case terminal of
        Nothing -> mapM_ (putStrLn . commandViewFormat) $ M.assocs (M.map show taskmap)
        Just t  -> mapM_ ((>> putStrLn "") . (runTermOutput t)) $ M.elems $ M.mapMaybeWithKey (applycolor t) taskmap
    where applycolor term k tsk = case getPriority tsk of
            Just 'A' -> usecolor prioritycolorA term k tsk
            Just 'a' -> usecolor prioritycolorA term k tsk
            Just 'B' -> usecolor prioritycolorB term k tsk
            Just 'b' -> usecolor prioritycolorB term k tsk
            _        -> usecolor nopriority term k tsk
          usecolor c t k tsk = getCapability t $ withForegroundColor <*> pure c <*> pure (termText $ commandViewFormat (k, show tsk))

commandViewFormat :: (Int, String) -> String
commandViewFormat = uncurry (printf "%3d -> %s")

makeProject :: String -> String
makeProject p = '+':p

makeContext :: String -> String
makeContext c = '@':c
