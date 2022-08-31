module Passwords (
    passwordPrompt
  , genPasswordPrompt
  ) where

import System.FilePath.Posix (dropExtension, (</>))
import System.Environment (getEnv)
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import Data.List (stripPrefix, sort, isInfixOf)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, spawnProcess)
import Data.List (dropWhileEnd, intercalate)
import Data.Char (isSpace)

import XMonad.Core
import XMonad.Prompt

data PassPrompt = PassPrompt
instance XPrompt PassPrompt where
    showXPrompt PassPrompt = "Copy Password for: "
    completionToCommand PassPrompt = id

data GenPassPrompt = GenPassPrompt
instance XPrompt GenPassPrompt where
    showXPrompt GenPassPrompt = "Generate Password for: "
    completionToCommand GenPassPrompt = id

selectPassword :: String -> X ()
selectPassword p = do
    let command = unwords ["pass", "-c", p
                      , "&&", "notify-send", "-t", "2500"
                      , "'Password copied'", "'Password", p, "copied to clipboard for 45 seconds'"]
    spawn command

generatePassword :: String -> IO ()
generatePassword p = do
    -- let command = unwords ["pass", "generate", "-c", "-n",  p, "10"
    --                   , "&&", "notify-send", "-t", "2500"
    --                   , "'Password generated'", "'Password for", p, "has been generated and copied to clipboard for 45 seconds'"]

    let command = "tesafilm"
    spawnProcess command [p]
    return ()

completionList :: IO [String]
completionList = do
    passDirectory <- fmap (++"/.password-store/") $ getEnv "HOME"
    files <- getFilesRecursively $ passDirectory
    let entries = map (dropExtension . fromJust . stripPrefix passDirectory) files
    return . sort $ filter (/="") entries

getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively path = do
    contents <- fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents path
    files <- forM contents $ \filename -> do
        let filepath = path </> filename
        isDirectory <- doesDirectoryExist filepath
        if isDirectory
            then getFilesRecursively filepath
            else return [filepath]
    return $ concat files

completionFun :: String -> IO [String]
completionFun [] = completionList
completionFun s = do
    completions <- completionList
    return $ filter (isInfixOf s) completions

passwordPrompt :: XPConfig -> X ()
passwordPrompt conf = mempty
    -- completions <- liftIO completionList
    -- name <- rofi completions
    -- selectPassword name
    -- mkXPrompt PassPrompt conf completionFun selectPassword

genPasswordPrompt :: XPConfig -> X ()
genPasswordPrompt conf = io $ (rofi [] >>= generatePassword)
    -- name <- rofi []
    -- generatePassword name
    -- mkXPrompt GenPassPrompt conf (mkComplFunFromList []) generatePassword

rofi :: [String] -> IO String
rofi choices = readProcess "rofi" ["-dmenu"] (intercalate "\n" choices)
