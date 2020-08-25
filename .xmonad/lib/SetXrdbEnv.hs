module SetXrdbEnv (
    setXrdbEnv
  ) where

import XMonad
import XMonad.Util.Run
import System.Process (readProcess)
import Data.Char (toUpper)
import Data.List (isInfixOf)
import System.Environment (setEnv, getEnv)
import Control.Monad (forM_)

resourceNames :: [String]
resourceNames = ["background", "foreground", "theme"] ++ map (\x -> "color" ++ show x) [0..15]

getResource :: String -> [String] -> Maybe String
getResource k xs = fmap (drop 1 . dropWhile (/= '\t')) resource
    where resource = head' $ filter (\x -> (k ++ ":") `isInfixOf` x) xs

head' (x:_) = Just x
head' [] = Nothing

setXrdbEnv :: IO ()
setXrdbEnv = do
    resources <- fmap lines $ runProcessWithInput "xrdb" ["-query"] ""
    let vars = map (\x -> ("XRDB" ++ map toUpper x, getResource x resources)) resourceNames
    forM_ vars (\(name, val) -> do
        case val of
            Just x -> setEnv name x
            Nothing -> return ())
