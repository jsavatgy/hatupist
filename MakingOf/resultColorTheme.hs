import Graphics.UI.Gtk hiding (Settings)
import Graphics.Rendering.Cairo
import Text.Printf

hueLimits  = (0.00, 1.00)
valLimits  = (1.00, 0.25)
limitsFrom = (0.00, 12.3)

f02 :: Double -> String
f02 = printf "%.2f"

ptAlong limitsTo limitsFrom pointFrom =
  to0 + distTo*((ptFrom - from0)/distFrom)
  where
    (to0,to1) = limitsTo
    distTo = to1 - to0
    (from0,from1) = limitsFrom
    distFrom = from1 - from0
    ptFrom = from0 `max` pointFrom `min` from1

drawCanvas canvas adj2 _evt = do
  rankD <- adjustmentGetValue adj2
  let sat = 0.40
      colorPoint = logBase 2.0 rankD
      hue = ptAlong hueLimits limitsFrom colorPoint
      val = ptAlong valLimits limitsFrom colorPoint
  dw <- widgetGetDrawWindow canvas
  renderWithDrawable dw (drawColorLine hue sat val)
  return True

paintText x y text = do
  fntDscr <- liftIO (fontDescriptionFromString "Sans 8")
  layout <- createLayout (text)
  liftIO (layoutSetFontDescription layout (Just fntDscr))
  moveTo x y
  setSourceRGB 0 0 0
  showLayout layout

drawColorLine hue sat val = do
  let (r,g,b) = hsvToRgb (hue,sat,val)
      text = "hue="++(f02 hue)++"  sat="++(f02 sat)++"  val="++(f02 val)
  setSourceRGB r g b
  rectangle 40 10 300 3
  fill
  rectangle 10 10 20 20
  fill
  setSourceRGB 0 0 0
  rectangle 10 10 20 20
  stroke
  paintText 40 17 text

drawCanvas2 canvas _evt = do
  dw <- widgetGetDrawWindow canvas
  mapM
    (\(y,twoToY) -> renderWithDrawable dw (drawBox y twoToY))
    [(y, 2.0**y) | y <- [0.00..12.00]]
  return True

drawBox y rankD = do
  let colorPoint = logBase 2.0 rankD
      sat = 0.40
      hue = ptAlong hueLimits limitsFrom colorPoint
      val = ptAlong valLimits limitsFrom colorPoint
      (r,g,b) = hsvToRgb (hue,sat,val)
  setSourceRGB r g b
  rectangle 10 (10+y*24) 20 20
  fill
  setSourceRGB 0 0 0
  rectangle 10 (10+y*24) 20 20
  stroke
  paintText 40 (13+y*24) (show (round (rankD)))

main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
 
  vbox <- vBoxNew False 0

  set window [
    containerChild := vbox ]

  canvas <- drawingAreaNew
  widgetSetSizeRequest canvas 380 35
  boxPackStart vbox canvas PackNatural 0

  adj2 <- adjustmentNew 1 1 5001 1 50 1
  hsc2 <- hScaleNew adj2
  scaleSetDigits hsc2 0
  boxPackStart vbox hsc2 PackNatural 0
 
  onExpose canvas (
    drawCanvas canvas adj2)

  canvas2 <- drawingAreaNew
  widgetSetSizeRequest canvas2 380 330
  boxPackStart vbox canvas2 PackGrow 0
  onExpose canvas2 ( drawCanvas2 canvas2 )

  onValueChanged adj2 ( widgetQueueDraw canvas )

  widgetShowAll window
  mainGUI

