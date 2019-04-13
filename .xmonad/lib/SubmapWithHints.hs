module SubmapWithHints (
    submapWithHints
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import XMonad
import XMonad.Prompt
import XMonad.Actions.Submap
import XMonad.Util.XUtils
import XMonad.Util.Font
import qualified Data.Map as M
import qualified XMonad.StackSet as W

submapWithHints :: XPConfig -> [(String, (KeyMask, KeySym), X ())] -> X ()
submapWithHints conf xs = do
    xmf <- initXMF "xft:monaco:size=10"
    hints <- getHints (map (\(a, _, _) -> a) xs) xmf Nothing
    w <- createHintWindow conf hints
    submap $ M.fromList submapMap
    hideHintWindow w
    where submapMap = map (\(_, b, c) -> (b, c)) xs

getHints :: [String] -> XMonadFont -> Maybe Int32 -> X [Hint]
getHints hints xmf maybeGap = do
    let gap = fromMaybe 15 maybeGap
    hintExtents <- mapM (textExtentsXMF xmf) hints
    return $ foldl (\a b -> a ++ [Hint (fst b) 5 (gap + (snd . snd) b + yPos a)]) [] (zip hints hintExtents)
    where yPos = fromMaybe 0 . lastMay . map (\x -> getY x)

displaySize :: X (Dimension, Dimension)
displaySize = do
    d <- asks display
    let s = defaultScreen d
    return $ ((fi . displayWidth d) s, (fi . displayHeight d) s)

windowPosition :: [Hint] -> X (Position, Position, Dimension, Dimension)
windowPosition hints = do
    d <- asks display
    s <- withWindowSet (\w -> return . screenRect . W.screenDetail . W.current $ w)
    return $ (rect_x s, rect_y s, fi $ rect_width s, windowHeight hints)

windowHeight :: [Hint] -> Dimension
windowHeight hints = (+15) . fi . fromMaybe 0 . lastMay . map getY $ hints

-- shows a window that displays each string
-- in its own line
createHintWindow :: XPConfig -> [Hint] -> X Window
createHintWindow conf hints = do
    (wX, wY, wWidth, wHeight) <- windowPosition hints
    let windowPosition = Rectangle wX wY wWidth wHeight
    w <- createNewWindow windowPosition Nothing "" True
    showWindow w
    printHintsOnWindow conf w hints
    return w

data Hint = Hint String Int32 Int32
getHint (Hint h _ _) = h
getX (Hint _ x _) = x
getY (Hint _ _ y) = y

lastMay xs = if null xs then Nothing else Just (last xs)

printHintsOnWindow :: XPConfig -> Window -> [Hint] -> X ()
printHintsOnWindow conf w hints = do
    (_, _, wWidth, wHeight) <- windowPosition hints
    d <- asks display
    xmf <- initXMF fnt

    p <- io $ createPixmap d w wWidth wHeight (defaultDepthOfScreen $ defaultScreenOfDisplay d)
    gc <- io $ createGC d p
    io $ setGraphicsExposures d gc False
    [bgColor, fgColor, borderColor] <- mapM (stringToPixel d) [bg, fg, border]
    io $ setForeground d gc borderColor
    io $ fillRectangle d p gc 0 0 wWidth wHeight
    io $ setForeground d gc bgColor
    io $ fillRectangle d p gc 1 1 (wWidth - 2) (wHeight - 2)

    forM_ hints (\h ->
        printStringXMF d p xmf gc fg bg (getX h) (getY h) (getHint h))

    io $ copyArea d p w gc 0 0 wWidth wHeight 0 0
    io $ freePixmap d p
    io $ freeGC d gc

    where fnt = font conf
          bg = bgColor conf
          fg = fgColor conf
          border = borderColor conf

hideHintWindow = deleteWindow
