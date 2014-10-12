import Graphics.UI.Gtk hiding (Settings)
import Graphics.Rendering.Cairo
import Data.Time.Clock.POSIX
import Data.Time
import System.Directory
import Control.Exception
import System.Locale (defaultTimeLocale)
import Data.IORef
import Control.Monad (when,forM)
import Data.List
import Text.Printf
import Data.Char (toUpper)

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

data GUI = None | GUI {
  gWindow :: Window,
  gErrorCanvas, gTimingCanvas, gHelperCanvas :: DrawingArea,
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label,
  gStatusbar :: Label,
  gStyle :: Style,
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
  showTimingPict :: Bool,
  oldlen :: Int,
  lastShownIv :: Int,
  oLabelStrs :: [String],
  total :: Int,
  lastLetter,nextLetter :: Char,
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
  showTimingPict = False,
  oLabelStrs = ["",""],
  oldlen = 0,
  total = 0,
  lastLetter = ' ', nextLetter = ' ',
  speedNows = [],
  lastShownIv = -1,
  intervals = [],
  results = [],
  sessionBest = zeroResult,
  settings = defaultSettings,
  gui = None
}

data Settings = Settings {
  startLine :: Int,
  lineLen :: Int,
  textfile :: String,
  font :: String,
  useHelper :: Bool,
  keyrow1,keyrow2,keyrow3 :: String
} deriving (Read, Show)

defaultSettings = Settings {
  startLine = 0,
  lineLen = 60,
  textfile = "hatupist.txt",
  font = "monospace 10",
  useHelper = True,
  keyrow1 = keyboard !! 0,
  keyrow2 = keyboard !! 1,
  keyrow3 = keyboard !! 2
}

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
csvFile      = "results.csv"

getStartupConfig gsRef = do
  gs <- readIORef gsRef
  homedir <- getHomeDirectory
  let dir = homedir ++ "/.hatupist"
  createDirectoryIfMissing False (dir)
  let rname = dir ++ "/" ++ settingsFile
  oldSettings <- settingsFromFile rname
  let rname = dir ++ "/" ++ resultsFile
  oldResults <- resultsFromFile rname
  listStoreSetValue (gModelR (g gs)) 0 (bestResult oldResults)
  writeIORef gsRef gs {
    homeDirectory = dir,
    results  = oldResults,
    settings = oldSettings
  }
  afterConfig gsRef

afterConfig gsRef = do
  setHelperVisibility gsRef
  setFonts gsRef
  
proverbs = [
  "Castigat ridendo mores.",
  "Vulpes pilum mutat, non mores!",
  "Aut viam inveniam aut faciam.",
  "Qui multum habet, plus cupit.",
  "Praestat cautela quam medela.",
  "Mali principii malus finis.",
  "Iucundum est narrare sua mala.",
  "Imperare sibi maximum imperium est.",
  "Qui non proficit, deficit.",
  "Quien me amat, amet et canum meum.",
  "Dives est qui sibi nihil deesse putat."
  ]

tryReadFile fname = do
  text <- readFile fname `catch` 
    \(SomeException e) -> ( do 
      dialog <- messageDialogNew Nothing [] MessageWarning ButtonsOk 
        "Avaa tiedosto"
      messageDialogSetSecondaryText dialog 
        ("Tiedostoa " ++ fname ++ " ei voitu lukea.")
      dialogRun dialog
      widgetDestroy dialog
      return (unlines proverbs))
  return text

getLines gsRef = do
  gs <- readIORef gsRef
  originalText <- tryReadFile (textfile (s gs))
  let textLinesss = colLines (collectWords (words (originalText)) 
                             (lineLen (s gs)))
      textLiness = map (++" ") textLinesss
  writeIORef gsRef gs {
    textLines = textLiness
  }
  renewLabels gsRef

quitProgram gsRef = do
  gs <- readIORef gsRef
  let rname = (homeDirectory gs) ++ "/" ++ resultsFile
  writeFile rname (show (r gs))
  let rname = (homeDirectory gs) ++ "/" ++ settingsFile
  writeFile rname (show (s gs))
  mainQuit

main = do
  gsRef <- newIORef initState 
  initGUI
  createGUI gsRef
  getStartupConfig gsRef
  getLines gsRef
  gs <- readIORef gsRef
  setStatusText "Voit aloittaa." (toGtkColor green) gs
  mainGUI

setFonts gsRef = do
  gs <- readIORef gsRef
  let gui = g gs 
      newFont = font (s gs)
  srcfont <- fontDescriptionFromString newFont
  widgetModifyFont (gLabel1 gui) (Just srcfont)
  widgetModifyFont (gLabel2 gui) (Just srcfont)
  widgetModifyFont (gEntry gui)  (Just srcfont)
  labelSetWidthChars (gLabel1 gui) (lineLen (s gs) + 3)

setHelperVisibility gsRef = do
  gs <- readIORef gsRef
  case useHelper (s gs) of
    True  -> widgetShow (gHelperCanvas (g gs))
    False -> widgetHide (gHelperCanvas (g gs))
  resize (gWindow (g gs))

resize window = do
  Requisition w h <- widgetSizeRequest window
  windowResize window w h

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
  style  <- widgetGetStyle window
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

  timingCanvas <- drawingAreaNew
  widgetSetSizeRequest timingCanvas 300 3
  onExpose timingCanvas (
    drawTimingCanvas gsRef timingCanvas)
  boxPackStart innerVBox1 timingCanvas PackNatural 0

  keyrowsCanvas <- drawingAreaNew
  widgetSetSizeRequest keyrowsCanvas 275 73
  onExpose keyrowsCanvas (
    drawKeyrowsCanvas gsRef keyrowsCanvas)
  boxPackStart innerVBox2 keyrowsCanvas PackNatural 0

  boxPackStart middleHBox innerVBox1 PackNatural 0
  boxPackStart middleHBox innerVBox2 PackNatural 6
  boxPackStart outerVBox  middleHBox PackNatural 3
  boxPackStart outerHBox  outerVBox  PackNatural 6
  boxPackStart extrmVBox  outerHBox  PackNatural 0

  set window [
    windowTitle := "Hatupist",
    containerChild := extrmVBox ]

  errorCanvas <- drawingAreaNew
  widgetSetSizeRequest errorCanvas 300 40
  onExpose errorCanvas (
    drawErrorCanvas gsRef errorCanvas)
  boxPackStart outerVBox errorCanvas PackGrow 0

  label1 <- labelNew Nothing
  miscSetAlignment label1 0 0
  miscSetPadding   label1 2 0
  boxPackStart outerVBox label1 PackNatural 0

  label2 <- labelNew Nothing
  miscSetAlignment label2 0 0
  miscSetPadding   label2 2 0
  boxPackStart outerVBox label2 PackNatural 0

  entry <- entryNew
  entrySetHasFrame entry False
  boxPackStart outerVBox entry PackNatural 3
  onEditableChanged entry (
    whenEntryChanged gsRef)

  statusbar <- labelNew Nothing
  miscSetAlignment statusbar 0 0
  miscSetPadding   statusbar 6 0

  eventbox <- eventBoxNew
  containerAdd eventbox statusbar
  boxPackEnd extrmVBox eventbox PackNatural 0

  widgetShowAll window
  writeIORef gsRef gs { 
    gui = GUI {
      gWindow = window,
      gTimingCanvas = timingCanvas, gErrorCanvas = errorCanvas,
      gHelperCanvas = keyrowsCanvas,
      gEntry = entry,
      gLabel1 = label1, gLabel2 = label2,
      gStatusbar = statusbar,
      gStyle = style,
      gModelR = rModel, gModelS = sModel, gModelI = iModel }}

toWord x = round (x*65535.0)
toGtkColor (r,g,b) = Color (toWord r) (toWord g) (toWord b)
toGtkColors xs = [toGtkColor x | x <- xs]

blue   = (0.200, 0.400, 1.000)
green  = (0.451, 0.824, 0.086)
red    = (1.000, 0.200, 0.400)
yellow = (0.988, 0.914, 0.310)
black  = (0.000, 0.000, 0.000)
gray   = (0.502, 0.502, 0.502)
white  = (1.000, 1.000, 1.000)
brkRed = (0.886, 0.031, 0.000)

drawStatusText gsRef = do
  gs <- readIORef gsRef
  if (oldStatus gs) /= Error
    then setStatusText "" (toGtkColor white) gs
    else setStatusText "Korjaa virheet!" (toGtkColor red) gs

drawEmptyPicture canvas = do 
  return True

drawErrorCanvas gsRef widget _evt = do
  gs <- readIORef gsRef
  drawWin <- widgetGetDrawWindow widget
  (wInt,hInt) <- widgetGetSize widget
  let (w,h) = (intToDouble wInt, intToDouble hInt)
  if (oldStatus gs) /= Error
    then drawEmptyPicture widget
    else renderWithDrawable drawWin (drawErrorPicture w h)
  return True

relPolygon (x,y) points (r,g,b) = do
  moveTo x y
  mapM (\(x,y) -> relLineTo x y) points
  closePath
  setSourceRGB r g b
  fill

drawErrorPicture w h = do
  let c = h
      r = 15
  mapM 
    ( \(x,y,points,color) -> relPolygon (x,y) points color)
    [(x,0,[((-c),h),(r,0),(c,(-h))],
     color) | (x,color) <- zip [0,r..w+c] (cycle [blue,red])]
  return True

onTimeToClear canvas = do
  widgetQueueDraw (canvas)
  return False

drawTimingCanvas gsRef canvas _evt = do
  gs <- readIORef gsRef
  if (showTimingPict gs)
    then drawTimingPicture gs canvas
    else drawEmptyPicture canvas
  writeIORef gsRef gs { showTimingPict = False }
  return True

hueLimits  = (0.00, 1.00)
valLimits  = (1.00, 0.25)
limitsFrom = (0.00, 12.3)

ptAlong limitsTo limitsFrom pointFrom =
  to0 + distTo*((ptFrom - from0)/distFrom)
  where
    (to0,to1) = limitsTo
    distTo = to1 - to0
    (from0,from1) = limitsFrom
    distFrom = from1 - from0
    ptFrom = from0 `max` pointFrom `min` from1

drawTimingRect w h (r,g,b) = do
  rectangle 0 0 w h
  setSourceRGB r g b
  fill

drawTimingPicture gs canvas = do
  row <- listStoreGetValue (gModelR (g gs)) 2
  (wInt,hInt) <- widgetGetSize canvas 
  drawWin <- widgetGetDrawWindow canvas
  let (w,h) = (intToDouble wInt, intToDouble hInt)
      rankD = intToDouble (rRank row)
      colorPoint = logBase 2.0 rankD
      sat = 0.40
      hue = ptAlong hueLimits limitsFrom colorPoint
      val = ptAlong valLimits limitsFrom colorPoint
      (r,g,b) = hsvToRgb (hue,sat,val)
  renderWithDrawable drawWin (drawTimingRect w h (r,g,b))
  timeoutAdd (onTimeToClear canvas) 1000
  return True

qwertyColor letter =
  if null as then Nothing else Just (areaColors !! (head as))
  where
    as = [a |(a,i) <- zip [0..] qwertyAreas, letter `elem` i]

qwertyLetter x y =
  if y < length qwerty && x < length (qwerty!!y)
    then (qwerty !! y) !! x
    else ' '

drawRect x y r1 (r,g,b) filled = do
  rectangle x y r1 r1
  setSourceRGB r g b
  if filled then fill else stroke

paintLetter x y letter (r,g,b) = do
  fntDscr <- liftIO (fontDescriptionFromString "Sans 8")
  layout <- createLayout (letter)
  liftIO (layoutSetFontDescription layout (Just fntDscr))
  moveTo x y
  setSourceRGB r g b
  showLayout layout

drawKey x y letter selected dupl = do
  setLineWidth 1.0
  let co = qwertyColor(qwertyLetter x y)
      botColor = if selected then white else black
      selColor = if dupl     then gray  else black
  case co of
    Just c -> drawRect (intToDouble (zentr xx)) 
                       (intToDouble (zentr yy)) 
                       (intToDouble r2)
                       c True
    Nothing -> return ()
  when selected (drawRect (intToDouble xx) 
                          (intToDouble yy) 
                          (intToDouble r1)
                          selColor True)
  drawRect (intToDouble xx) (intToDouble yy) (intToDouble r1) botColor False
  paintLetter (intToDouble (xx+2)) (intToDouble (yy+2)) letter botColor
  where
    r1 = 18
    r2 = 21
    zentr z = z - (r2-r1) `div` 2
    deltaXs = [3,12,0]
    margin = 5
    xx = x*r2+deltaXs!!y + margin
    yy = y*r2            + margin

drawKeyrowsCanvas gsRef canvas _evt = do 
  gs <- readIORef gsRef
  writeIORef gsRef gs {
    lastLetter = nextLetter gs
  }
  let c = nextLetter gs
      selected = [toUpper c]
      dupl = toUpper c == toUpper (lastLetter gs)
      keymap = [keyrow1 (s gs), keyrow2 (s gs), keyrow3 (s gs)] 
      leK y = length (keymap !! y)
      keyK x y = [toUpper ((keymap !! y) !! x)]
  drawWin <- widgetGetDrawWindow canvas
  renderWithDrawable drawWin (do
    mapM 
      ( \(x,y,k) -> drawKey x y k (k == selected) dupl)
      [(x,y,keyK x y)| y <- [0..2], x <- [0..(leK y)-1], keyK x y /= " "]
    return True)

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
        \row -> [ cellText := func row ])
      treeViewColumnSetTitle col title
      treeViewAppendColumn view col

modify parent color text gs = do
  if (text == "")
    then do
      bg <- styleGetBackground (gStyle (g gs)) StateNormal
      widgetModifyBg parent StateNormal bg
    else widgetModifyBg parent StateNormal color

setStatusText text color gs = do
  let label = gStatusbar (g gs)
  labelSetText label text
  parent <- widgetGetParent label
  case parent of
    Nothing -> print "No parent"
    Just parent -> modify parent color text gs
  return ()

menuBarDescr = 
  [("_Tiedosto", 
    [("gtk-open", openFile),
     ("gtk-select-font", openFont),
     ("gtk-preferences", setPreferences),
     ("gtk-about", showResultPics),
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

type RFunction = Result -> Double
data FuncResult = Count | Calc RFunction

collectResults xFunc yFunc sortCrit values =
  zip (xResult xFunc) yResult
  where
    xResult Count = [1.0..]
    xResult (Calc f) = map f sortedByCrit
    yResult = map yFunc sortedByCrit
    sortedByCrit = sortBy sortCrit values

framelist = [
  ("X = Järjestysluku\nY = Nopeus (mrk/min)", 
    Count, speedResult, earlierFst),
  ("X = Päivämäärä\nY = Nopeus (mrk/min)", 
    Calc dateResult, speedResult, earlierFst),
  ("X = Nopeus (mrk/min)\nY = Virheprosentti", 
    Calc speedResult, errorProsResult, slowerFst),
  ("X = Sijoitus\nY = Nopeus (mrk/min)", 
    Count, speedResult, fasterFst),
  ("X = Päivämäärä\nY = Virheprosentti", 
    Calc dateResult, errorProsResult, earlierFst)]

createFrame title func values container = do
  box <- vBoxNew False 0
  drawingArea <- drawingAreaNew
  widgetSetSizeRequest drawingArea 360 300
  boxPackStart box drawingArea PackGrow 0
  label <- labelNew (Just title)
  miscSetAlignment label 0 0
  miscSetPadding   label 10 0
  labelSetSelectable label True

  eventbox <- eventBoxNew
  containerAdd eventbox label
  boxPackStart box eventbox PackGrow 0
  widgetModifyBg eventbox StateNormal (toGtkColor white)

  boxPackStart container box PackGrow 10
  onExpose drawingArea (exposeHandler drawingArea func values)

createFrames list container = do
  forM list (\(s,v) -> createFrame s draw1 v container)

clearBgWhite w h (r,g,b) = do
  rectangle 0 0 w h
  setSourceRGB r g b
  fill

drawOneMarker cr (r,g,b) = do
  rectangle (-cr) (-cr) cr cr
  setSourceRGBA r g b 0.5
  fill

drawMarkerAt x y = do
  save
  translate x y
  drawOneMarker 2.0 brkRed
  restore

data Axis =  Horizontal | Vertical

textMovementX (PangoRectangle x1 y1 x2 y2) =
  ((-x2)-2,(-y2)/2)

textMovementY (PangoRectangle x1 y1 x2 y2) =
  ((-x2)/2,2)

manipulateText layout axis = do
  fntDscr <- fontDescriptionFromString "Sans Bold 8"
  layoutSetFontDescription layout (Just fntDscr)
  (ink,logical) <- layoutGetExtents layout
  return (case axis of
    Vertical -> textMovementX logical
    Horizontal -> textMovementY logical
    )

timeLegendForm :: ZonedTime -> String
timeLegendForm = formatTime defaultTimeLocale "%Y\n%b %d\n%H:%M"
doubleToPt :: Double -> POSIXTime
doubleToPt t  = fromRational (toRational t)

drawLegend legends (r,g,b) = do
  tz <- liftIO getCurrentTimeZone
  setSourceRGB r g b
  setLineWidth w
  moveTo x1 y1
  lineTo x2 y2
  forM yLegendsExtra (\((x,y),str) -> do
    moveTo (x+ww) y
    lineTo (x-tickW) y
    layout <- createLayout (fy str tz)
    (dx,dy) <- liftIO (manipulateText layout Vertical)
    relMoveTo dx dy
    showLayout layout
    )
  moveTo x3 y3
  lineTo x4 y4
  forM xLegendsExtra (\((x,y),str) -> do
    moveTo x (y-ww)
    lineTo x (y+tickW)
    layout <- createLayout (fx str tz)
    (dx,dy) <- liftIO (manipulateText layout Horizontal)
    relMoveTo dx dy
    showLayout layout
    )
  stroke
  where
    t tz str = timeLegendForm (
            utcToZonedTime tz (posixSecondsToUTCTime (doubleToPt str)))
    fx x tz
      | x < 999999 = printf fmtStrX (funcX x)
      | otherwise    = t tz x
    (fmtStrX,funcX) = precisionStr valXs
    fy y tz
      | y < 999999 = printf fmtStrY (funcY y)
      | otherwise    = t tz y
    (fmtStrY,funcY) = precisionStr valYs
    (yLegends, xLegends, values) = legends
    (valMinX,valMaxX,valMinY,valMaxY) = values
    (x1,y1) = head yLegends
    (x2,y2) = last yLegends
    (x3,y3) = head xLegends
    (x4,y4) = last xLegends
    valXs = [valMinX + (dbl i)*(valMaxX-valMinX)/(dbl tcX) | i <- [0..tcX]]
    valYs = [valMinY + (dbl i)*(valMaxY-valMinY)/(dbl tcY) | i <- [0..tcY]]
    xLegendsExtra = zip ticksX valXs
    yLegendsExtra = zip ticksY (reverse valYs)
    ticksX = [(x3+(dbl i)*tickDx,y3) | i <- [0..tcX]]
    ticksY = [(x1,(dbl i)*tickDy+y1) | i <- [0..tcY]]
    tickDx = (x4-x3) / (dbl tcX)
    tickDy = (y2-y1) / (dbl tcY)
    tcX = floor ((x4-x3) / tickDxMin)
    tcY = floor ((y2-y1) / tickDyMin)
    dbl = intToDouble
    tickDxMin = 100
    tickDyMin = 50
    tickW = 3 * w
    ww = 0.5 * w
    w = 2

draw1 w h values = do
  clearBgWhite w h white
  let (markers, legends) = pointsOnScreen w h values
  forM markers (\(x,y) -> drawMarkerAt x y)
  drawLegend legends black

exposeHandler widget func values e = do
  drawWin <- widgetGetDrawWindow widget
  (wInt,hInt) <- widgetGetSize widget
  let (w,h) = (intToDouble wInt, intToDouble hInt)
  renderWithDrawable drawWin (func w h values)
  return True

pointsOnScreen screenMaxX screenMaxY values = 
  ([(posX x, posY y) | (x,y) <- values],
   ([(x1-15,y1),(x1-15,y2)], [(x1,y2+15),(x2,y2+15)],
   (valMinX,valMaxX,valMinY,valMaxY)))
  where
    x1 = posX valMinX
    x2 = posX valMaxX
    y1 = posY valMaxY
    y2 = posY valMinY
    posX x = factorX*(x-valMinX) + legend
    posY y = (screenMaxY - legend) - factorY*(y-valMinY)
    factorX = screenDifX / valDifX
    factorY = screenDifY / valDifY
    screenDifX = max (screenMaxX - (margin+legend)) epsilon
    screenDifY = max (screenMaxY - (margin+legend)) epsilon
    legend = 80
    margin = 30
    valDifY = max (valMaxY - valMinY) epsilon
    valDifX = max (valMaxX - valMinX) epsilon
    valMinY = minimum [y | (x,y) <- values]
    valMaxY = maximum [y | (x,y) <- values]
    valMinX = minimum [x | (x,y) <- values]
    valMaxX = maximum [x | (x,y) <- values]
    delta = 2*pi/100
    epsilon = 0.01

showResultPics gsRef = do
  gs <- readIORef gsRef
  dialog <- dialogNew
  set dialog [ windowTitle := "Tietoja" ]
  let valuess = [(s,collectResults x y c (r gs)) | (s,x,y,c) <- framelist]
  upbox <- dialogGetUpper dialog

  vbox1 <- vBoxNew False 0
  createFrames valuess vbox1
  scrolledWin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWin PolicyAutomatic PolicyAutomatic
  widgetSetSizeRequest scrolledWin 400 400
  boxPackStart upbox scrolledWin PackGrow 0
  scrolledWindowAddWithViewport scrolledWin vbox1

  dialogAddButton dialog stockClose ResponseClose

  widgetShowAll upbox
  response <- dialogRun dialog
  widgetDestroy dialog
  return ()

keyboard = qwerty

qwerty = [
  "qwertyuiopå",
  "asdfghjklöä",
  " zxcvbnm,.-"]

qwertyAreas = ["rfvujm", "edcik,", "wsxol.", "qazpö-"]
areaColors  = [blue, yellow, red, green]

colTitle = ["Muuttuja", "Arvo"]

showBool True  = "Kyllä"
showBool False = "Ei"

settingsTable (Settings a b _ _ c e f g) = [
  ["Aloitusrivi",            show a],
  ["Rivinpituus (mrk)",      show b],
  ["Näytä näppäimistö (K/E)",showBool c],
  ["Näppäimistön ylärivi",   e],
  ["Näppäimistön keskirivi", f],
  ["Näppäimistön alarivi",   g] ]

setPreferences gsRef = do
  oldGs <- readIORef gsRef
  result <- preferencesDialog "Asetukset" oldGs gsRef
  case result of
    Just "OK" -> do
      newGs <- readIORef gsRef
      when ((lineLen   (s oldGs)) /= (lineLen   (s newGs))) (getLines gsRef)
      when ((startLine (s oldGs)) /= (startLine (s newGs))) (renewLabels gsRef)
      afterConfig gsRef
    otherwise -> do
      writeIORef gsRef oldGs

preferencesDialog title gs gsRef = do
  dialog <- dialogNew
  set dialog [ windowTitle := title ]
  dialogAddButton dialog stockCancel ResponseCancel
  dialogAddButton dialog stockOk ResponseOk

  model <- listStoreNew (settingsTable (s gs))
  view  <- treeViewNewWithModel model
  setupSettingsView gsRef view model

  upbox <- dialogGetUpper dialog
  boxPackStart upbox view PackNatural 10
  widgetShowAll upbox
  response <- dialogRun dialog
  widgetDestroy dialog
  case response of
    ResponseOk -> do
      return (Just "OK")
    ResponseCancel -> do
      return Nothing
    ResponseDeleteEvent -> do
      return Nothing
    _ -> return Nothing

setupSettingsView gsRef view model = do
  mapM 
    ( \(title, i) -> newcol view model title i )
    ( zip colTitle [0..] )
  where
    newcol view model title i = do
      renderer <- cellRendererTextNew
      col <- treeViewColumnNew
      cellLayoutPackStart col renderer True
      cellLayoutSetAttributes col renderer model (
        \row -> [ cellText := row !! i, cellTextEditable := (i==1) ])
      treeViewColumnSetTitle col title
      treeViewAppendColumn view col
      on renderer edited (onCellEdited gsRef model)

readInt :: String -> IO Int
readInt s = readIO s

readBool :: String -> IO Bool
readBool s = readIO s

readKBool :: String -> IO Bool
readKBool s = do 
  let c = toUpper (head (s ++ " "))
      result = if c == 'K' then True else False
  return result

v lst i = (lst !! i) !! 1

tryFunc g lst = do
  a <- readInt (v lst 0)
  b <- readInt (v lst 1)
  c <- readKBool (v lst 2)
  return g {
    startLine = a,
    lineLen = b,
    useHelper = c,
    keyrow1 = v lst 3,
    keyrow2 = v lst 4,
    keyrow3 = v lst 5
  } 

refreshSettingsTable model newS = do
  mapM
    ( \(i,newRow) -> listStoreSetValue model i newRow )
    ( zip [0..] (settingsTable newS) )

onCellEdited gsRef model path newText = do
  gs <- readIORef gsRef
  let i = head path
  [key,oldText] <- listStoreGetValue model i
  listStoreSetValue model i [key,newText]
  lst <- listStoreToList model
  newS <- tryFunc (s gs) lst `catch` 
    \(SomeException e) -> return (s gs)
  refreshSettingsTable model newS
  writeIORef gsRef gs { settings = newS }

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

errorProsResult (Result date1 mrks1 rnk1 errs1) = 
  errorPros errs1 mrks1

errorCountResult (Result date1 mrks1 rnk1 errs1) = 
  intToDouble errs1

speedResult (Result date1 mrks1 rnk1 errs1) = 
  speed mrks1 (intToDouble rDuration)

timeUTC date = case (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date) of
  Just x  -> zonedTimeToUTC x
  Nothing -> posixSecondsToUTCTime (doubleToPt 0.0)

dateResult :: Result -> Double
dateResult (Result date1 mrks1 rnk1 errs1) =
  realToFrac ndt
  where
    ndt = utcTimeToPOSIXSeconds (timeUTC date1)

precisionStr values 
  | place >= 0  = (decimalFmt, sigDigits place)
  | otherwise   = (floatFmt place, id)
  where
    decimalFmt = "%.0f"
    floatFmt d = "%."  ++ (show (abs d)) ++ "f"
    place = round lg
    lg | df == 0.0 = 0.0
       | otherwise = logBase 10.0 df
    df = 0.1 * diff1
    diff1 = abs ((values !! 1) - (values !! 0))

sigDigits :: Int -> Double -> Double
sigDigits aExp number = 
  (intToDouble (round (number / factor))) * factor
  where 
    factor = 10.0 ** (intToDouble aExp)

f01 :: Double -> String
f01 = printf "%.1f"

f02 :: Double -> String
f02 = printf "%.2f"
f02p e = f02 e ++ "%"

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

nextChar n str = 
  head (drop n (str ++ " "))

blankStart n str =
  replicate n ' ' ++ drop n str

renewLabels gsRef = do
  gs <- readIORef gsRef
  let labelStrs = labelStrings (startLine (settings gs)) (textLines gs)
  set (gLabel1 (g gs)) [ labelLabel := labelStrs !! 0 ]
  set (gLabel2 (g gs)) [ labelLabel := labelStrs !! 1 ]
  writeIORef gsRef gs {
    oLabelStrs = labelStrs,
    nextLetter = head ((labelStrs !! 0) ++ " ")
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

fasterFst (Result date1 mrks1 rnk1 errs1) (Result date2 mrks2 rnk2 errs2) =
  if mrks1 /= mrks2 
    then mrks2 `compare` mrks1
    else date1 `compare` date2 

slowerFst (Result date1 mrks1 rnk1 errs1) (Result date2 mrks2 rnk2 errs2) =
  mrks1 `compare` mrks2

earlierFst (Result date1 mrks1 rnk1 errs1) (Result date2 mrks2 rnk2 errs2) =
  date1 `compare` date2

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
    newRs = take maxRank (insertBy fasterFst newResult (results gs))
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
  widgetQueueDraw (gTimingCanvas (g gs))
  return newGs { showTimingPict = True }

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
    oldlen = max cprfix (oldlen gs),
    nextLetter = nextChar cprfix label1Str
  }
  drawStatusText gsRef
  widgetQueueDraw (gErrorCanvas  (g gs))
  widgetQueueDraw (gHelperCanvas (g gs))
  when (label1Str == txt) (advanceLine gsRef newGs)
  return ()

whenNotStarted status pt gsRef gs = do
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
  return gs {
    total = (total gs) + 1,
    intervals = addTime
      Correct 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenNewError pt gsRef gs = do
  return gs {
    intervals = addTime
      Error 
      (iNumber (secondsFrom (startTime gs) pt))
      (intervals gs)
  }

whenOther status oldStatus pt gsRef gs = do
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

