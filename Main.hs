import System.IO (withFile, IOMode(ReadMode))
import Data.Either (rights)
import Control.Monad.Trans.State.Lazy (StateT(..), evalStateT)
import System.IO.Error (tryIOError, ioError)
import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B

import System.Console.Haskeline (InputT(..), runInputT, defaultSettings, getInputLine, outputStrLn, setComplete)
import System.Console.Haskeline.Completion (simpleCompletion, completeWord)

import Task
import Commands

data Runstatus = Continue | Stop | Payload (Maybe String)

main :: IO ()
main = do
    eithererrortasks <- tryIOError (readTaskFile todoFile)
    either ioError startrepl eithererrortasks where

        startrepl tasks = do
            let taskmap  = toMap tasks
                projects = lsProjects tasks
                contexts = lsContexts tasks
                ss       = Sessionstate taskmap projects contexts
                runonce = do
                    ps <- runInputT defaultSettings $ evalStateT oneREPloop ss
                    case ps of
                        Continue -> runonce
                        Stop -> return ()
            runonce

oneREPloop :: StateT Sessionstate (InputT IO) Runstatus
oneREPloop = do c <- lift $ getInputLine prompt
                case c of
                    Nothing      -> return Stop
                    Just ""      -> return Continue
                    Just "quit"  -> return Stop
                    Just "exit"  -> return Stop
                    Just cmdargs -> parsedinp cmdargs
    where
        parsedinp c = case parseOnly parseInput (B.pack c) of
            (Right (cmd, args)) -> (maybe (cmderror cmd) (($ args) . func) (getCmd cmd)) >> oneREPloop
            (Left error)        -> cmderror c >> oneREPloop
        cmderror cmd = (lift $ outputStrLn $ unknowncmd cmd)

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
-- Hardcoded Settings
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
unknowncmd c = "WTF is: " ++ c

todoFile :: String
todoFile = "todoapi.txt"

prompt :: String
prompt = ":: "
