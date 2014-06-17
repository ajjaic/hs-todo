module Commands (
    Command(..),
    getCmd,
    cmdNames
) where

import Control.Monad.Trans.State.Strict (StateT, get)
import System.Console.Haskeline (InputT, outputStrLn)
import Text.Printf (printf)
import Control.Monad.Trans.Class (lift)
import qualified Data.IntMap.Lazy as M
import qualified Data.List as L

import Task
import Debug.Trace
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

addTask      = undefined
deleteTask   = undefined
appTask      = undefined

todoHelp :: Maybe String -> InputT (StateT Sessionstate IO) ()
todoHelp Nothing = mapM_ outputStrLn $ map (\c -> uncurry (printf "%3s -> %s") (name c, desc c)) cmds
todoHelp (Just c) = outputStrLn (maybe (unknowncmd c) desc (getCmd c))

{-addTask :: Maybe String -> StateT Sessionstate (InputT IO) ()                                                               -}
{-addTask Nothing     = lift $ outputStrLn "Nothing to add"                                                                   -}
{-addTask (Just xtsk) = do                                                                                                    -}
{-    case parseOnly parseTaskAdd (B.pack xtsk) of                                                                            -}
{-        (Right r) -> do                                                                                                     -}
{-            utc <- lift $ liftIO getCurrentTime                                                                             -}
{-            ss  <- get                                                                                                      -}
{-            let newpriority    = tapriority r                                                                               -}
{-                newtaskcontent = tatask r                                                                                   -}
{-                newproject     = taproject r                                                                                -}
{-                newcontexts    = tacontext r                                                                                -}
{-                newtask        = Task newpriority utc Nothing newtaskcontent newproject newcontexts                         -}
{-                newtaskset     = M.insert 1 newtask $ M.mapKeys (+1) (tasks ss)                                             -}
{-                newprojectset  = if isNothing newproject then (projects ss) else L.union (projects ss) [fromJust newproject]-}
{-                newcontextset  = if L.null newcontexts then (contexts ss) else L.union (contexts ss) newcontexts            -}
{-                newss          = Sessionstate newtaskset newprojectset newcontextset                                        -}
{-            put newss                                                                                                       -}
{-        (Left l) -> lift $ outputStrLn l                                                                                    -}


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

listProjects :: Maybe String -> InputT (StateT Sessionstate IO) ()
listProjects Nothing = do
    ss <- lift get
    let projects  = zip [1 .. ] (lsProjectsM $ sessionTasks ss)
        printstrs = map commandViewFormat projects
    mapM_ outputStrLn printstrs

listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
listTasks Nothing = do
    ss <- lift get
    let tasksascending = M.assocs (M.map show $ sessionTasks ss)
        printstrs      = map commandViewFormat tasksascending
    mapM_ outputStrLn printstrs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Internal API
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

commandViewFormat :: (Int, String) -> String
commandViewFormat p = (uncurry (printf "%3d -> %s")) p

makeProject p = '+':p

makeContext c = '@':c
