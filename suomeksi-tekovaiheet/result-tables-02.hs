import Graphics.UI.Gtk
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.IORef
import Control.Monad (when)
import Data.List
import Text.Printf

iDuration = 30
rDuration = 120
amountOfIntervals = rDuration `div` iDuration

data Result = Result { 
  rDate :: String, 
  rMrks, rRank, rErrs :: Int
} deriving (Read, Show)

instance Eq Result where
  (Result a b c d) == (Result aa bb cc dd) =
    b == bb && a == aa

instance Ord Result where
  (Result a b c d) `compare` (Result aa bb cc dd) = 
    if (b == bb) then (a `compare` aa) else (bb `compare` b)

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
  gLabel1, gLabel2 :: Label,
  gModelR :: ListStore Result,
  gModelS :: ListStore Timing,
  gModelI :: ListStore Interval
}

data GameStatus = Error | Correct | Back | NotStarted
  deriving (Eq, Show)

data State = State {
  startTime :: POSIXTime,
  oldStatus :: GameStatus,
  oldlen :: Int,
  lastShownIv :: Int,
  oLabelStrs :: [String],
  total :: Int,
  intervals :: [Interval],
  results :: [Result], 
  shownResults :: [Result],
  currentLine :: Int
}

initState = State {
  startTime = fromIntegral 0 :: POSIXTime,
  oldStatus = NotStarted,
  oLabelStrs = ["",""],
  oldlen = 0,
  total = 0,
  lastShownIv = -1,
  intervals = [],
  results = [], 
  shownResults = [],
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
  | x == y       = x : commonPrefix xs ys
  | otherwise    = []
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
    gLabel1 = label1, gLabel2 = label2,
    gModelR = rModel, gModelS = sModel, gModelI = iModel
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
  | total == 0 = 0.0
  | otherwise  = 100.0 * (intToDouble errs) / (intToDouble total)
  where
    total = mrks + errs

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

iNumber t =
  floor t `div` iDuration

iLeft t =
  iDuration - (floor t `mod` iDuration)

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

ivsBetween iMin iMax ivs =
  filter (\iv -> iMin <= (iNum iv) && (iNum iv) <= iMax) ivs

ivsFrom iMin ivs =
  filter (\iv -> iMin <= (iNum iv)) ivs

ivsAllBetween iMin iMax ivs =
  [ivExactly n ivs | n <- [iMin .. iMax]]

ivExactly n ivs =
  case find (\iv -> n == (iNum iv)) ivs of
    Just x  -> x
    Nothing -> zeroInterval { iNum = n }

renewTableS gui gs t = do
  listStoreSetValue (gModelS gui) 0 Timing {
    sSecsLeft = iLeft t,
    sSession = mmss t,
    sTotal = total gs,
    sSpeed = 0.0
  }
  return ()

latestResult results = if null results 
  then zeroResult
  else head results

timeFormatted :: ZonedTime -> String
timeFormatted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

addResult showIvs oldResults = do
  pt <- getPOSIXTime
  tz <- getCurrentTimeZone
  let newResult0 = zeroResult {
    rDate  = timeFormatted (utcToZonedTime tz (posixSecondsToUTCTime pt)),
    rMrks = sum [iMrks g | g <- showIvs],
    rErrs = sum [iMrks g | g <- showIvs]
  }
  let newResult = newResult0 {
    rRank = tellRank newResult0 oldResults
  }
  return (take maxRank (insert newResult oldResults),
         replicate 3 zeroResult)

tellRank x xs =
  case findIndex (x <=) xs of
    Just n -> n + 1
    Nothing -> length xs + 1

maxRank  = 5000

renewTableR gui gs iCur = do
  mapM 
    (\(a,b) -> listStoreSetValue (gModelR gui) a b) 
    (zip [0..] (shownResults gs))
  return ()

renewTableI gui gs iCur = do
  mapM 
    (\(a,b) -> listStoreSetValue (gModelI gui) (amountOfIntervals-a) b) 
    (zip [1..] showIvs)
  (newRs,newShownRs) <- addResult showIvs (results gs)
  return gs {
    intervals = newIvs,
    lastShownIv = iCur,
    results = newRs,
    shownResults = newShownRs
  }
  where
    iMaxShow = iCur - 1
    infimum = iMaxShow - amountOfIntervals + 1
    iMinShow = max 0 infimum
    iMinNeed = max 0 (infimum + 1)
    newIvs = ivsFrom iMinNeed (intervals gs)
    showIvs = reverse (ivsAllBetween iMinShow iMaxShow (intervals gs))

renewSeldomTables gui gs iCur = do
  putStrLn ("Interval #" ++ show iCur ++ " started.")
  mapM_ (putStrLn . show) (intervals gs)
  putStrLn ("There was " ++ show (length (intervals gs)) ++ " intervals")
  newGs <- renewTableI gui gs iCur
  renewTableR gui newGs iCur
  putStrLn ("There are now " ++ show (length (intervals newGs)) ++ " intervals\n")
  return newGs

renewTables gui gs t iCur = do
  renewTableS gui gs t 
  newGs <- if (lastShownIv gs /= iCur)
  then renewSeldomTables gui gs iCur
  else return gs
  return newGs

onTimeout gui gsRef = do
  gs <- readIORef gsRef
  pt <- getPOSIXTime
  let t = secondsFrom (startTime gs) pt
      iCur = iNumber t
  newGs <- renewTables gui gs t iCur
  writeIORef gsRef newGs 
  return True

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
  newGs <- f gui settings lines pt gsRef gs
  writeIORef gsRef newGs {
    oldStatus = status,
    oldlen = max (length (commonPrefix txt label1Str)) (oldlen gs)
  }
  when (label1Str == txt) (advanceLine gui lines gsRef newGs)
  return ()

whenNotStarted status gui settings lines pt gsRef gs = do
  putStrLn ("Started with " ++ (show status))
  timeoutAdd (onTimeout gui gsRef) 500
  return gs { 
    total = if status == Correct then 1 else 0,
    startTime = pt,
    intervals = addTime
      status 
      (iNumber 0.0)
      (intervals gs)
  }

whenCorrect gui settings lines pt gsRef gs = do
  print "Correct."
  return gs {
    total = (total gs) + 1,
    intervals = addTime
      Correct 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenNewError gui settings lines pt gsRef gs = do
  print "New Error."
  return gs {
    intervals = addTime
      Error 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenOther status oldStatus gui settings lines pt gsRef gs = do
  putStrLn ("Other with " ++ (show (status,oldStatus)))
  return gs

latestIvNum ivs = if null ivs then -1 else iNum (head ivs)

addTime status i intervals =
  [newHead] ++ tail newIvs
  where
  newHead = case status of
    Correct -> headIv { iMrks = (iMrks headIv) + 1 }
    Error   -> headIv { iErrs = (iErrs headIv) + 1 }
  headIv = head newIvs
  newIvs = if null intervals || i /= latestIvNum intervals
    then [zeroInterval { iNum = i }] ++ intervals
    else intervals

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

