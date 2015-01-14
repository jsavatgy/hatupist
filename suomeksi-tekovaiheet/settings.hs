import Graphics.UI.Gtk hiding (Settings)
import Data.Time.Clock.POSIX
import Data.Time
import System.Directory
import Control.Exception
import System.Locale (defaultTimeLocale)
import Data.IORef
import Control.Monad (when,forM)
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
  (Result date1 mrks1 rnk1 errs1) == (Result date2 mrks2 rnk2 errs2) =
    mrks1 == mrks2 && date1 == date2

instance Ord Result where
  compare = fasterFst

fasterFst (Result date1 mrks1 rnk1 errs1) (Result date2 mrks2 rnk2 errs2) =
  if mrks1 /= mrks2 
    then mrks2 `compare` mrks1
    else date1 `compare` date2 

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

data GUI = NotCreated | GUI {
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label,
  gModelR :: ListStore Result,
  gModelS :: ListStore Timing,
  gModelI :: ListStore Interval
}

data GameStatus = Error | Correct | Back | NotStarted
  deriving (Eq, Show)

data State = State {
  homeDirectory :: String,
  textLines :: [String],
  startTime :: POSIXTime,
  oldStatus :: GameStatus,
  oldlen :: Int,
  lastShownIv :: Int,
  oLabelStrs :: [String],
  total :: Int,
  speedNows :: [(POSIXTime, Int)],
  intervals :: [Interval],
  results :: [Result],
  sessionBest :: Result,
  settings :: Settings,
  gui :: GUI
}

initState = State {
  homeDirectory = "",
  textLines = [],
  startTime = fromIntegral 0 :: POSIXTime,
  oldStatus = NotStarted,
  oLabelStrs = ["",""],
  oldlen = 0,
  total = 0,
  speedNows = [],
  lastShownIv = -1,
  intervals = [],
  results = [],
  sessionBest = zeroResult,
  settings = defaultSettings,
  gui = NotCreated
}

data Settings = Settings {
  startLine :: Int,
  lineLen :: Int,
  textfile :: String,
  font :: String
} deriving (Read, Show)

defaultSettings = Settings {
  startLine = 0,
  lineLen = 60,
  textfile = "morse.txt",
  font = "monospace 10"
}

xxx = replicate (lineLen defaultSettings) 'x'
s gs = settings gs
g gs = gui gs
r gs = results gs

resultsFromFile  fname = do
  structFromFile fname readRs []
settingsFromFile fname = do
  structFromFile fname readSs defaultSettings

structFromFile fname pFunc zero = do
  content <- readFile fname `catch` 
    \(SomeException e) -> return ""
  result <- pFunc content `catch` 
    \(SomeException e) -> return zero
  return result

readRs :: String -> IO [Result]
readRs = readIO

readSs :: String -> IO Settings
readSs = readIO

resultsFile  = "results.txt"
settingsFile = "settings.txt"

getStartupConfig gsRef = do
  gs <- readIORef gsRef
  -- directory
  homedir <- getHomeDirectory
  let dir = homedir ++ "/.hatupist"
  createDirectoryIfMissing False (dir)
  -- settings
  let rname = dir ++ "/" ++ settingsFile
  oldSettings <- settingsFromFile rname
  putStrLn ("Reading " ++ rname)
  putStrLn (show (oldSettings))
  -- results
  let rname = dir ++ "/" ++ resultsFile
  oldResults <- resultsFromFile rname
  putStrLn ("Reading " ++ rname ++ ": " ++ show (length (oldResults)) ++ " rows")
  listStoreSetValue (gModelR (g gs)) 0 (bestResult oldResults)
  -- other
  writeIORef gsRef gs {
    homeDirectory = dir,
    results  = oldResults,
    settings = oldSettings
  }
  setFonts gsRef

getLines gsRef = do
  gs <- readIORef gsRef
  originalText <- readFile (textfile (s gs))
  let textLinesss = colLines (collectWords (words (originalText)) 
                             (lineLen (s gs)))
      textLiness = map (++" ") textLinesss
  writeIORef gsRef gs {
    textLines = textLiness
  }
  renewLabels gsRef

quitProgram gsRef = do
  print "Quitting."
  gs <- readIORef gsRef
  -- results
  let rname = (homeDirectory gs) ++ "/" ++ resultsFile
  writeFile rname (show (r gs))
  putStrLn ("Writing " ++ rname)
  -- settings
  let rname = (homeDirectory gs) ++ "/" ++ settingsFile
  writeFile rname (show (s gs))
  putStrLn ("Writing " ++ rname)
  mainQuit

main = do
  gsRef <- newIORef initState 
  initGUI
  createGUI gsRef
  getStartupConfig gsRef
  getLines gsRef
  gs <- readIORef gsRef
  mainGUI

setFonts gsRef = do
  gs <- readIORef gsRef
  let gui = g gs 
      newFont = font (s gs)
  srcfont <- fontDescriptionFromString newFont
  widgetModifyFont (gLabel1 gui) (Just srcfont)
  widgetModifyFont (gLabel2 gui) (Just srcfont)
  widgetModifyFont (gEntry gui)  (Just srcfont)

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
rColTitles = ["Päiväys", "Tulos",         "Sija",     "Virheitä" ]
rColFuncs  = [ rDate,     rSpeed . rMrks,  rShowRank,  rErrorPros]

sInitModel = [zeroTiming]
sColTitles = ["Istunto", "Yhteensä",     "Jäljellä",       "Nopeus"]
sColFuncs  = [ sSession,  show . sTotal,  show . sSecsLeft, f01 . sSpeed]

iInitModel = replicate amountOfIntervals zeroInterval
iColTitles = ["Alkoi",        "Päättyi",    "Nopeus",       "Virheitä" ]
iColFuncs  = [ iStarts . iNum, iEnds . iNum, iSpeed . iMrks, iErrorPros]

createGUI gsRef = do
  gs <- readIORef gsRef
  window <- windowNew
  onDestroy window (quitProgram gsRef)

  extrmVBox  <- vBoxNew False 0
  outerHBox  <- hBoxNew False 0
  outerVBox  <- vBoxNew False 0
  middleHBox <- hBoxNew False 0
  innerVBox1 <- vBoxNew False 0
  innerVBox2 <- vBoxNew False 0

  menubar <- createMenuBar menuBarDescr gsRef
  boxPackStart extrmVBox menubar PackNatural 0

  rModel <- setupView rInitModel rColTitles rColFuncs innerVBox1
  sModel <- setupView sInitModel sColTitles sColFuncs innerVBox1
  iModel <- setupView iInitModel iColTitles iColFuncs innerVBox2

  boxPackStart middleHBox innerVBox1 PackNatural 0
  boxPackStart middleHBox innerVBox2 PackNatural 6
  boxPackStart outerVBox  middleHBox PackNatural 3
  boxPackStart outerHBox  outerVBox  PackNatural 6
  boxPackStart extrmVBox  outerHBox  PackNatural 0

  set window [
    windowTitle := "Hatupist",
    containerChild := extrmVBox ]

  label1 <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  boxPackStart outerVBox label1 PackNatural 0

  label2 <- labelNew (Just xxx)
  miscSetAlignment label2 0 0
  boxPackStart outerVBox label2 PackNatural 0

  entry <- entryNew
  entrySetHasFrame entry False
  boxPackStart outerVBox entry PackNatural 3
  onEditableChanged entry (
    whenEntryChanged gsRef)

  widgetShowAll window
  writeIORef gsRef gs { 
    gui = GUI {
      gEntry = entry,
      gLabel1 = label1, gLabel2 = label2,
      gModelR = rModel, gModelS = sModel, gModelI = iModel }}

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

menuBarDescr = 
  [("_Tiedosto", 
    [("gtk-open", openFile),
     ("gtk-select-font", openFont),
     ("gtk-preferences", setPreferences),
     ("gtk-about", noop),
     ("gtk-quit", quitProgram)])
  ]

createMenuBar descr gsRef = do 
  bar <- menuBarNew
  mapM_ (createMenu bar) descr
  return bar
  where
    createMenu bar (name,items) = do 
      menu <- menuNew
      item <- menuItemNewWithLabelOrMnemonic name
      menuItemSetSubmenu item menu
      menuShellAppend bar item
      mapM_ (createMenuItem menu) items
    createMenuItem menu (stock,action) = do 
      item <- imageMenuItemNewFromStock stock
      menuShellAppend menu item
      onActivateLeaf item (do action gsRef)
    menuItemNewWithLabelOrMnemonic name
      | elem '_' name = menuItemNewWithMnemonic name
      | otherwise     = menuItemNewWithLabel name

noop gsRef = do
  return ()

setPreferences gsRef = do
  gs <- readIORef gsRef
  result <- chooseLineLen "Rivinpituus" (lineLen (s gs))
  case result of
    Just newLineLen -> do
      writeIORef gsRef gs {
        settings = (s gs) {
          lineLen = newLineLen,
          startLine = 0 }}
      getLines gsRef
    otherwise -> return ()

chooseLineLen prompt oldLineLen = do
  dialog <- dialogNew
  set dialog [ windowTitle := prompt ]
  dialogAddButton dialog stockCancel ResponseCancel
  dialogAddButton dialog stockOk ResponseOk

  nameLabel  <- labelNew $ Just "Rivinpituus (merkkiä):"
  adjustment <- adjustmentNew (fromIntegral oldLineLen) 1 250 5 1 0
  spinbutton <- spinButtonNew adjustment 1.0 0

  upbox <- dialogGetUpper dialog
  boxPackStart upbox nameLabel  PackNatural 0
  boxPackStart upbox spinbutton PackNatural 0
  widgetShowAll upbox
  response <- dialogRun dialog
  print response
  widgetDestroy dialog
  case response of
    ResponseOk -> do
      value <- spinButtonGetValueAsInt spinbutton
      widgetDestroy dialog
      return (Just value)
    ResponseCancel -> do
      widgetDestroy dialog
      return Nothing
    ResponseDeleteEvent -> do
      widgetDestroy dialog
      return Nothing
    _ -> return Nothing

openFont gsRef = do
  gs <- readIORef gsRef
  result <- chooseFont "Valitse kirjasin" (font (s gs))
  case result of
    Just newFont -> do
      writeIORef gsRef gs {
        settings = (s gs) {
          font = newFont }}
      setFonts gsRef
    otherwise -> return ()

chooseFont prompt oldFont = do
  dialog <- fontSelectionDialogNew prompt
  fontSelectionDialogSetFontName dialog oldFont
  widgetShow dialog
  response <- dialogRun dialog
  print response
  case response of
    ResponseOk -> do
      fn <- fontSelectionDialogGetFontName dialog
      widgetDestroy dialog
      return fn
    ResponseCancel -> do
      widgetDestroy dialog
      return Nothing
    ResponseDeleteEvent -> do
      widgetDestroy dialog
      return Nothing
    _ -> return Nothing

openFile gsRef = do
  gs <- readIORef gsRef
  result <- chooseFile "Valitse teksti"
  case result of
    Just newTextFile -> do
      writeIORef gsRef gs {
        settings = (s gs) {
          textfile = newTextFile,
          startLine = 0 }}
      getLines gsRef
    otherwise -> return ()

chooseFile prompt = do
  dialog <- fileChooserDialogNew (Just prompt) Nothing
              FileChooserActionOpen 
                [("gtk-cancel",ResponseCancel),
                 ("gtk-open",  ResponseAccept)]
  fileChooserSetCurrentFolder dialog "."
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do
      fn <- fileChooserGetFilename dialog
      widgetDestroy dialog
      return fn
    ResponseCancel -> do
      widgetDestroy dialog
      return Nothing
    ResponseDeleteEvent -> do
      widgetDestroy dialog
      return Nothing
    _ -> return Nothing

rShowRank rR =
  showRank (rRank rR)

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

speed mrks t = 
  (intToDouble mrks) * 60.0 / (max t 1.0)

iSpeed mrks = 
  f01 (speed mrks (intToDouble iDuration))

rSpeed mrks = 
  f01 (speed mrks (intToDouble rDuration))

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

blankStart n str =
  replicate n ' ' ++ drop n str

renewLabels gsRef = do
  gs <- readIORef gsRef
  let labelStrs = labelStrings (startLine (settings gs)) (textLines gs)
  set (gLabel1 (g gs)) [ labelLabel := labelStrs !! 0 ]
  set (gLabel2 (g gs)) [ labelLabel := labelStrs !! 1 ]
  writeIORef gsRef gs {
    oLabelStrs = labelStrs
  }
  entrySetText (gEntry (g gs)) ""

labelStrings :: Int -> [String] -> [String]
labelStrings startline textLines =
  [textLines !! first] ++ [textLines !! second]
  where
    first = startline `mod` (length textLines)
    second = (startline + 1) `mod` (length textLines)

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

tableRRefreshMs = 500
speedFromMs = 10000
speedCount = speedFromMs `div` tableRRefreshMs

difs speds = 
  if null speds
    then (0.0, 0)
    else (secondsFrom (fst start) (fst end), (snd end) - (snd start))
  where
    start  = last speds
    end    = head speds

renewTableS gs t = do
  pt <- getPOSIXTime
  let newGs = gs {
    speedNows = [(pt, (total gs))] ++ take speedCount (speedNows gs)
  }
  let s = difs (speedNows newGs)
  listStoreSetValue (gModelS (g gs)) 0 Timing {
    sSecsLeft = iLeft t,
    sSession = mmss t,
    sTotal = total gs,
    sSpeed = speed (snd s) (fst s)
  }
  return newGs

bestResult results = if null results 
  then zeroResult
  else head results

timeFormatted :: ZonedTime -> String
timeFormatted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

reRank1 (Result { rDate = a, rMrks = b, rRank = c, rErrs = d }, newRank) =
  Result { rDate = a, rMrks = b, rRank = newRank, rErrs = d }

reRank rs = map reRank1 (zip rs [1..])

addResult showIvs gs = do
  pt <- getPOSIXTime
  tz <- getCurrentTimeZone
  let newResult0 = zeroResult {
    rDate  = timeFormatted (utcToZonedTime tz (posixSecondsToUTCTime pt)),
    rMrks = sum [iMrks g | g <- showIvs],
    rErrs = sum [iErrs g | g <- showIvs]
  }
  let newResult = newResult0 {
    rRank = tellRank newResult0 (results gs)
  }
  let 
    newRs = take maxRank (insert newResult (results gs))
    new2Rs = reRank newRs
    newShownRs = [
      bestResult new2Rs,
      (sessionBest gs) `min` newResult,
      newResult ]
  return (new2Rs, newShownRs)

showRank rank
  | rank <= maxRank = show rank
  | otherwise       = ">" ++ show maxRank

tellRank x xs =
  case findIndex (x <=) xs of
    Just n  -> n + 1
    Nothing -> length xs + 1

maxRank  = 5000

renewTableR gs shownRs = do
  mapM 
    (\(a,b) -> listStoreSetValue (gModelR (g gs)) a b) 
    (zip [0..] shownRs)
  return ()

renewTableI gs iCur = do
  mapM 
    (\(a,b) -> listStoreSetValue (gModelI (g gs)) (amountOfIntervals-a) b) 
    (zip [1..] showIvs)
  (newRs, newShownRs) <- addResult showIvs gs
  return (gs {
    intervals = newIvs,
    lastShownIv = iCur,
    results = newRs,
    sessionBest = newShownRs !! 1
  }, newShownRs)
  where
    iMaxShow = iCur - 1
    infimum = iMaxShow - amountOfIntervals + 1
    iMinShow = max 0 infimum
    iMinNeed = max 0 (infimum + 1)
    newIvs = ivsFrom iMinNeed (intervals gs)
    showIvs = reverse (ivsAllBetween iMinShow iMaxShow (intervals gs))

renewSeldomTables gs iCur = do
  (newGs, shownRs) <- renewTableI gs iCur
  renewTableR newGs shownRs
  return newGs

renewTables gs t iCur = do
  newGs <- renewTableS gs t 
  new2Gs <- if (lastShownIv newGs /= iCur) && iCur >= 1
  then renewSeldomTables newGs iCur
  else return newGs
  return new2Gs

onTimeout gsRef = do
  gs <- readIORef gsRef
  pt <- getPOSIXTime
  let t = secondsFrom (startTime gs) pt
      iCur = iNumber t
  newGs <- renewTables gs t iCur
  writeIORef gsRef newGs 
  return True


whenEntryChanged gsRef = do
  pt  <- getPOSIXTime
  gs  <- readIORef gsRef
  txt <- entryGetText (gEntry (g gs))
  let label1Str = head (oLabelStrs gs)
      status = getStatus txt label1Str (oldlen gs)
      f = case (status,oldStatus gs) of
        (_,NotStarted)  -> whenNotStarted status
        (Correct,_)     -> whenCorrect txt
        (Error,Correct) -> whenNewError
        otherwise       -> whenOther status (oldStatus gs)
      cprfix = length (commonPrefix txt label1Str)
  newGs <- f pt gsRef gs
  set (gLabel1 (g gs)) [ 
    labelLabel := blankStart cprfix label1Str]
  writeIORef gsRef newGs {
    oldStatus = status,
    oldlen = max cprfix (oldlen gs)
  }
  when (label1Str == txt) (advanceLine gsRef newGs)
  return ()

whenNotStarted status pt gsRef gs = do
  putStrLn ("Started with " ++ (show status))
  timeoutAdd (onTimeout gsRef) tableRRefreshMs
  return gs { 
    total = if status == Correct then 1 else 0,
    startTime = pt,
    intervals = addTime
      status 
      (iNumber 0.0)
      (intervals gs)
  }

whenCorrect txt pt gsRef gs = do
  print "Correct."
  return gs {
    total = (total gs) + 1,
    intervals = addTime
      Correct 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenNewError pt gsRef gs = do
  print "New Error."
  return gs {
    intervals = addTime
      Error 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenOther status oldStatus pt gsRef gs = do
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

advanceLine gsRef gs = do
  gs <- readIORef gsRef
  let newStartLine = ((startLine (s gs)) + 1) `mod` (length (textLines gs))
  writeIORef gsRef gs {
    settings = (s gs) {
      startLine = newStartLine},
    oldlen = 0
  }
  renewLabels gsRef
  return ()

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

