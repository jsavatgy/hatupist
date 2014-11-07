import Graphics.UI.Gtk
import Data.Time.Clock.POSIX
import Data.IORef
import Control.Monad (when)
import Text.Printf

iDuration = 30
rDuration = 120
amountOfIntervals = rDuration `div` iDuration

data Result = Result { 
  rDate :: String, 
  rMrks, rRank, rErrs :: Int
} deriving (Read, Show)

zeroResult = Result {
  rDate = "0000-00-00 00:00:00", 
  rMrks = 0, rRank = 0, rErrs = 0 }

data Timing = Timing {
  sSession :: String, sTotal :: Int, 
  sSecsLeft :: Int,   sSpeed :: Double
} deriving Show

zeroTiming = Timing {
  sSession = "00:00", sTotal = 0,
  sSecsLeft = iDuration, sSpeed = 0.0 }

data Interval = Interval {
  iNum, iMrks, iErrs :: Int 
} deriving Show

zeroInterval = Interval {
  iNum = -1, iMrks = 0, iErrs = 0 }

data GUI = GUI {
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label
}

data GameStatus = Error | Correct | Back | NotStarted
  deriving (Eq, Show)

data State = State {
  startTime :: POSIXTime,
  oldStatus :: GameStatus,
  oldlen :: Int,
  oLabelStrs :: [String],
  currentLine :: Int
}

initState = State {
  startTime = fromIntegral 0 :: POSIXTime,
  oldStatus = NotStarted,
  oLabelStrs = ["",""],
  oldlen = 0,
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
    whenEntryChanged gui settings lines gsRef)
  mainGUI

getStatus :: String -> String -> Int -> GameStatus
getStatus written goal oldlen
  | a == b && c < d  = Correct
  | a == b           = Back
  | otherwise        = Error
  where
    a = written
    b = commonPrefix written goal
    c = oldlen
    d = length written

commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = []

rInitModel = replicate 3 zeroResult
rColTitles = ["Päiväys", "Tulos",         "Sija",        "Virheitä" ]
rColFuncs  = [ rDate,     rSpeed . rMrks,  show . rRank,  rErrorPros]

sInitModel = [zeroTiming]
sColTitles = ["Istunto", "Yhteensä",     "Jakso",          "Jaksonopeus"]
sColFuncs  = [ sSession,  show . sTotal,  show . sSecsLeft, f01 . sSpeed]

iInitModel = replicate amountOfIntervals zeroInterval
iColTitles = ["Alkoi",        "Päättyi",    "Nopeus",       "Virheitä" ]
iColFuncs  = [ iStarts . iNum, iEnds . iNum, iSpeed . iMrks, iErrorPros]

createGUI = do
  window <- windowNew
  onDestroy window mainQuit

  outerVBox  <- vBoxNew False 0
  middleHBox <- hBoxNew False 0
  innerVBox1 <- vBoxNew False 0
  innerVBox2 <- vBoxNew False 0

  rModel <- setupView rInitModel rColTitles rColFuncs innerVBox1
  sModel <- setupView sInitModel sColTitles sColFuncs innerVBox1
  iModel <- setupView iInitModel iColTitles iColFuncs innerVBox2

  boxPackStart middleHBox innerVBox1 PackNatural 0
  boxPackStart middleHBox innerVBox2 PackNatural 6
  boxPackStart outerVBox middleHBox PackNatural 10

  set window [
    containerBorderWidth := 10,
    windowTitle := "timing.hs",
    containerChild := outerVBox ]

  label1 <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  boxPackStart outerVBox label1 PackNatural 0

  label2 <- labelNew (Just xxx)
  miscSetAlignment label2 0 0
  boxPackStart outerVBox label2 PackNatural 0

  entry  <- entryNew
  entrySetHasFrame entry False
  boxPackStart outerVBox entry PackNatural 3

  widgetShowAll window
  
  return GUI {
    gEntry = entry,
    gLabel1 = label1,
    gLabel2 = label2
  }

setupView initModel titles funcs parent = do
  model <- listStoreNew (initModel)
  view  <- treeViewNewWithModel model
  mapM 
    ( \(title, func) -> newcol view model title func )
    ( zip titles funcs )
  set view [ widgetCanFocus := False ]
  boxPackStart parent view PackNatural 3
  return model
  where
    newcol view model title func = do
      renderer <- cellRendererTextNew
      col <- treeViewColumnNew
      cellLayoutPackStart col renderer True
      cellLayoutSetAttributes col renderer model (
        \row -> [ cellText := func row])
      treeViewColumnSetTitle col title
      treeViewAppendColumn view col

rErrorPros rR = 
  f02p (errorPros (rErrs rR) (rMrks rR))

iErrorPros iV = 
  f02p (errorPros (iErrs iV) (iMrks iV))

errorPros errs mrks 
  | errs == 0 && mrks == 0 = 0.0
  | errs /= 0 && mrks == 0 = 100.0
  | otherwise = 100.0 * (intToDouble errs) / (intToDouble mrks)

f01 :: Double -> String
f01 = printf "%.1f"

f02p :: Double -> String
f02p = printf "%.2f%%"

iSpeed mrks = 
  f01 ((intToDouble mrks)* 60.0 / intToDouble iDuration)

rSpeed mrks = 
  f01 ((intToDouble mrks)* 60.0 / intToDouble rDuration)

iStarts n
  | n <= 0    = "00:00"
  | otherwise = mmss (fromIntegral (n*iDuration) :: Double)

iEnds n = iStarts (n+1)

intervalNumber t =
  floor t `div` iDuration

mmss seconds =
  leadingZero (show (floor seconds `div` 60)) ++ 
  ":" ++ 
  leadingZero (show (floor seconds `mod` 60))

leadingZero s
  | length s < 2 = "0" ++ s
  | otherwise    = s

secondsFrom startPt endPt =
  a - b
  where
    a = ptToDouble endPt
    b = ptToDouble startPt

ptToDouble :: POSIXTime -> Double
ptToDouble t  = fromRational (toRational t)
intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

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

whenEntryChanged gui settings lines gsRef = do
  pt  <- getPOSIXTime
  gs  <- readIORef gsRef
  txt <- entryGetText (gEntry gui)
  let label1Str = head (oLabelStrs gs)
      status = getStatus txt label1Str (oldlen gs)
      f = case (status,oldStatus gs) of
        (_,NotStarted)  -> whenNotStarted status
        (Correct,_)     -> whenCorrect
        (Error,Correct) -> whenNewError
        otherwise       -> whenOther status (oldStatus gs)
  newgs <- f gui settings pt gs
  writeIORef gsRef newgs {
    oldStatus = status,
    oldlen = max (length (commonPrefix txt label1Str)) (oldlen gs)
  }
  when (label1Str == txt) (advanceLine gui lines gsRef gs)
  return ()

whenNotStarted status gui settings pt gs = do
  putStrLn ("Started with " ++ (show status))
  return gs { 
    startTime = pt 
  }

whenCorrect gui settings pt gs = do
  print "Correct."
  let s = secondsFrom (startTime gs) pt
      i = intervalNumber s
  print (s,i)
  return gs

whenNewError gui settings pt gs = do
  print "New Error."
  return gs

whenOther status oldStatus gui settings pt gs = do
  putStrLn ("Other with " ++ (show (status,oldStatus)))
  return gs

advanceLine gui lines gsRef gs = do
  writeIORef gsRef gs {
    currentLine = ncline,
    oldlen = 0
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

