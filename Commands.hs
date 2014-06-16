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

projectView  = undefined
contextView  = undefined
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


{-contextView :: Maybe String -> StateT Sessionstate (InputT IO) ()                                                           -}
{-contextView (Just c) = do                                                                                                   -}
{-    ss <- get                                                                                                               -}
{-    let t = M.elems $ tasks ss                                                                                              -}
{-        ctxt = filter ((any (==c)) . context) t                                                                             -}
{-    if L.null ctxt                                                                                                          -}
{-        then return ()                                                                                                      -}
{-        else do lift $ outputStrLn (makeContext c)                                                                          -}
{-                    >> mapM_ (outputStrLn . show) ctxt                                                                      -}
{-contextView _ = do                                                                                                          -}
{-    ss <- get                                                                                                               -}
{-    let c = contexts ss                                                                                                     -}
{-    if L.null c                                                                                                             -}
{-        then return ()                                                                                                      -}
{-        else do                                                                                                             -}
{-            mapM_ (\mc -> contextView mc >> (lift $ outputStrLn "")) (map Just (init c))                                    -}
{-            contextView (Just $ last c)                                                                                     -}


{-projectView :: Maybe String -> StateT Sessionstate (InputT IO) ()                                                           -}
{-projectView prj@(Just p) = do                                                                                               -}
{-    ss <- get                                                                                                               -}
{-    let t    = M.elems $ tasks ss                                                                                           -}
{-        prjt = filter ((==prj) . project) t                                                                                 -}
{-    if L.null prjt                                                                                                          -}
{-        then return ()                                                                                                      -}
{-        else do lift $ outputStrLn (makeProject p)                                                                          -}
{-                    >> mapM_ (outputStrLn . show) prjt                                                                      -}
{-projectView _ = do                                                                                                          -}
{-    ss <- get                                                                                                               -}
{-    let p = projects ss                                                                                                     -}
{-    if L.null p                                                                                                             -}
{-        then return ()                                                                                                      -}
{-        else do                                                                                                             -}
{-           mapM_ (\mp -> projectView mp >> (lift $ outputStrLn "")) (map Just (init p))                                     -}
{-           projectView (Just $ last p)                                                                                      -}


listContexts :: Maybe String -> InputT (StateT Sessionstate IO) ()
listContexts Nothing = do
    ss <- lift get
    let contexts  = zip [1 .. ] (lsContextsM $ sessionTasks ss) :: [(Int, String)]
        printstrs = map (uncurry (printf "%3d -> %s")) contexts
    mapM_ outputStrLn printstrs

listProjects :: Maybe String -> InputT (StateT Sessionstate IO) ()
listProjects Nothing = do
    ss <- lift get
    let projects  = zip [1 .. ] (lsProjectsM $ sessionTasks ss) :: [(Int, String)]
        printstrs = map (uncurry (printf "%3d -> %s")) projects
    mapM_ outputStrLn printstrs

listTasks :: Maybe String -> InputT (StateT Sessionstate IO) ()
listTasks Nothing = do
    ss <- lift get
    let tasksascending = M.assocs (M.map show $ sessionTasks ss)
        printstrs      = map (uncurry (printf "%3d -> %s")) tasksascending
    mapM_ outputStrLn printstrs
