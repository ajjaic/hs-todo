import System.IO (withFile, IOMode(ReadMode))
import Data.Either (rights)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import System.IO.Error (tryIOError, ioError)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.List (isPrefixOf)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn, setComplete)
import System.Console.Haskeline.Completion (simpleCompletion, completeWord, Completion(Completion), CompletionFunc)
import System.Console.Terminfo.Base (setupTermFromEnv, SetupTermError)
import System.Console.Terminfo.Base (Terminal)
import Control.Exception.Base (try)
import qualified Data.ByteString.Char8 as B

import Task
import Commands

main :: IO ()
main = do
    terminal <- try setupTermFromEnv :: IO (Either SetupTermError Terminal)
    case terminal of
        Left _ -> putStrLn (info "Color output not available") >> startwithterminal Nothing
        Right t -> startwithterminal (Just t)
    where
        startwithterminal term = tryIOError (readTaskFile todoFile)
                             >>= (\errOrTasks -> either ioError (startrepl term) errOrTasks)
        startrepl term tasks = let taskmap  = toMap tasks
                                   projects = lsProjects tasks
                                   contexts = lsContexts tasks
                                   ss       = Sessionstate taskmap projects contexts (cmdNames ++ projects ++ contexts) term
                                in evalStateT (runInputT (setComplete autocomp defaultSettings) oneREPloop) ss


    --case terminal of
    --    Left  _ -> putStrLn (info "Color output not available")
    --    Right t -> do
    --        eithererrortasks <- tryIOError (readTaskFile todoFile)
    --        either ioError startrepl eithererrortasks where
    --            startrepl tasks = do
    --                let taskmap  = toMap tasks
    --                    projects = lsProjects tasks
    --                    contexts = lsContexts tasks
    --                    ss       = Sessionstate taskmap projects contexts (cmdNames ++ projects ++ contexts) t
    --                evalStateT (runInputT (setComplete autocomp defaultSettings) oneREPloop) ss

oneREPloop :: InputT (StateT Sessionstate IO) ()
oneREPloop = do
    c <- getInputLine prompt
    case c of
        Nothing      -> return ()
        Just "quit"  -> return ()
        Just "exit"  -> return ()
        Just ""      -> oneREPloop
        Just cmdargs -> parseinp cmdargs
    where
        cmderror err = outputStrLn $ programerror err
        parseinp c   = case parseOnly parseInput (B.pack c) of
            (Right (cmd, args)) -> (maybe (cmderror cmd) (($ args) . func) (getCmd cmd)) >> oneREPloop
            (Left _) -> cmderror c

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Read |Task| from file
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
readTaskFile :: FilePath -> IO [Task]
readTaskFile f = withFile f ReadMode helper where
    helper h = do
        bytes <- B.hGetContents h
        let lines = B.lines bytes
        return $ rights $ map (parseOnly parseTask) lines

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Autocomplete function
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
autocomp :: CompletionFunc (StateT Sessionstate IO)
autocomp = completeWord Nothing [' '] autocomp' where
    noendspace str = Completion str str False
    autocomp' :: String -> (StateT Sessionstate IO) [Completion]
    autocomp' s = do
        ss <- get
        let st = sessionAutocomp ss
        return $ map noendspace $ filter (s `isPrefixOf`) st

