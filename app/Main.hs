module Main (main) where

import Bi.Parser (blobFieldP, intFieldP)
import Bi.Writer (blobFieldW, intFieldW)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hGetContents, hPutStrLn, stderr)
import System.Process (
  CreateProcess (..),
  StdStream (..),
  createProcess,
  shell,
  waitForProcess,
 )
import Text.Parsec (count, runParser)
import Text.Parsec.String (Parser)
import Text.Printf (printf)

data CommandResult = CommandResult String String String Int

instance Show CommandResult where
  show (CommandResult command out err code) =
    intFieldW "code" code
      ++ blobFieldW "shell" command
      ++ blobFieldW "stdout" out
      ++ blobFieldW "stderr" err

commandResultP :: Parser CommandResult
commandResultP = do
  code <- intFieldP "code"
  command <- blobFieldP "shell"
  out <- blobFieldP "stdout"
  err <- blobFieldP "stderr"
  return $ CommandResult command out err code

savedFileP :: Parser [CommandResult]
savedFileP = do
  amount <- intFieldP "count"
  count amount commandResultP

diff :: CommandResult -> CommandResult -> String
diff (CommandResult _ outA errA codeA) (CommandResult _ outB errB codeB)
  | codeA /= codeB = printf "UNEXPECTED EXIT CODE:\n    EXPECTED: %d\n    ACTUAL: %d\n" codeA codeB
  | errA /= errB = printf "UNEXPECTED STDERR:\n    EXPECTED:\n```\n%s```\n    ACTUAL:\n```\n%s```\n" errA errB
  | outA /= outB = printf "UNEXPECTED STDOUT:\n    EXPECTED:\n```\n%s```\n    ACTUAL:\n```\n%s```\n" outA outB
  | otherwise = ""

createCommand :: FilePath -> String -> CreateProcess
createCommand d command =
  (shell command){std_out = CreatePipe, std_err = CreatePipe, cwd = Just d}

execCommand :: FilePath -> String -> IO CommandResult
execCommand d command = do
  (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand d command
  code <- waitForProcess procHandle
  out <- hGetContents hout
  err <- hGetContents herr
  let code' = case code of
        ExitSuccess -> 0
        ExitFailure i -> i
  return (CommandResult command out err code')

recordCommand :: FilePath -> String -> IO String
recordCommand d command = do
  putStrLn $ "Recording `" ++ command ++ "`..."
  result <- execCommand d command
  return $ show result

recordFile :: FilePath -> IO ()
recordFile file = do
  putStrLn $ "--- Recording from " ++ file ++ " ---"
  contents <- readFile file
  d <- getCurrentDirectory
  results <- mapM (recordCommand d) (lines contents)
  writeFile (file ++ ".bi") $ intFieldW "count" (length results) ++ concat results

replayResult :: FilePath -> CommandResult -> IO CommandResult
replayResult d (CommandResult sh _ _ _) = do
  putStrLn $ "Replaying `" ++ sh ++ "`..."
  execCommand d sh

replayFile :: [Char] -> IO ()
replayFile file = do
  putStrLn $ "--- Replaying from " ++ file ++ " ---"
  contents <- readFile (file ++ ".bi")
  d <- getCurrentDirectory
  case runParser savedFileP () file contents of
    Left err -> do
      print err
      exitWith $ ExitFailure 1
    Right results -> do
      newResults <- mapM (replayResult d) results
      putStr $ concatMap (uncurry diff) (zip results newResults)

errorAndUsage :: String -> IO ()
errorAndUsage err = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ progName ++ " [record|replay] <files..>\n"
  hPutStrLn stderr $ "ERROR: " ++ err
  exitWith $ ExitFailure 1

logic :: [String] -> IO ()
logic ["record"] = errorAndUsage "missing files"
logic ["replay"] = errorAndUsage "missing files"
logic ("record" : files) = mapM_ recordFile files
logic ("replay" : files) = mapM_ replayFile files
logic (subcmd : _) = errorAndUsage $ "invalid subcommand " ++ subcmd
logic [] = errorAndUsage "missing subcommand"

main :: IO ()
main = getArgs >>= logic
