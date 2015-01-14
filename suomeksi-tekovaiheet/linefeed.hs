import Graphics.UI.Gtk
import Data.IORef
import Control.Monad (when)

data GUI = GUI {
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label
}

data State = State {
  oLabelStrs :: [String],
  currentLine :: Int
}

initState = State {
  oLabelStrs = ["",""],
  currentLine = 0
}

data Settings = Settings {
  lineLen :: Int, startLine :: Int,
  textfile :: String
} deriving (Read, Show)

defaultSettings = Settings {
  lineLen = 40, startLine = 0,
  textfile = "morse.txt"
}

settings = defaultSettings
xxx = replicate (lineLen defaultSettings) 'x'

main = do
  gsRef <- newIORef initState {
    currentLine = startLine settings
  }
  initGUI
  originalText <- readFile (textfile settings)
  let liness = colLines (collectWords (words (originalText)) (lineLen settings))
      lines = map (++" ") liness
  gui <- createGUI
  renewLabels gui (startLine settings) lines gsRef
  onEditableChanged (gEntry gui) (
    whenEntryChanged gui lines gsRef)
  mainGUI

createGUI = do
  window <- windowNew
  onDestroy window mainQuit

  vbox <- vBoxNew False 0

  set window [
    containerBorderWidth := 10,
    windowTitle := "linefeed.hs",
    containerChild := vbox ]

  label1 <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  miscSetPadding   label1 2 0
  boxPackStart vbox label1 PackNatural 0

  label2 <- labelNew (Just xxx)
  miscSetAlignment label2 0 0
  miscSetPadding   label2 2 0
  boxPackStart vbox label2 PackNatural 0

  entry  <- entryNew
  entrySetHasFrame entry False
  boxPackStart vbox entry PackNatural 3

  widgetShowAll window
  
  return GUI {
    gEntry = entry,
    gLabel1 = label1,
    gLabel2 = label2
  }

renewLabels gui currentLine lines gsRef = do
  gs <- readIORef gsRef
  let labelStrs = labelStrings currentLine lines
  set (gLabel1 gui) [ labelLabel := labelStrs !! 0 ]
  set (gLabel2 gui) [ labelLabel := labelStrs !! 1 ]
  writeIORef gsRef gs {
    oLabelStrs = labelStrs
  }
  entrySetText (gEntry gui) ""

labelStrings :: Int -> [String] -> [String]
labelStrings startline lines =
  [lines !! first] ++ [lines !! second]
  where
    first = startline `mod` (length lines)
    second = (startline + 1) `mod` (length lines)

whenEntryChanged gui lines gsRef = do
  gs  <- readIORef gsRef
  txt <- entryGetText (gEntry gui)
  let label1Str = head (oLabelStrs gs)
  when (label1Str == txt) (advanceLine gui lines gsRef gs)
  return ()

advanceLine gui lines gsRef gs = do
  writeIORef gsRef gs {
    currentLine = ncline
  }
  renewLabels gui ncline lines gsRef
  return ()
  where
    ncline = ((currentLine gs) + 1) `mod` (length lines)

colLines (xs:xss) =
  (unwords xs) : colLines xss
colLines [] = []

collectWords [] n = []
collectWords ys n =
  p1 : collectWords p2 n
  where
    (p1,p2) = splitAt (length (untilLen ys 0 n)) ys

untilLen (t:ts) s n 
  | s+x<n || s==0  = t : untilLen ts (s+x) n
  | otherwise      = []
  where
    x = length t + 1
untilLen [] s n = []


