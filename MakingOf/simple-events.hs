import Graphics.UI.Gtk
import Data.Time.Clock.POSIX

xxx = "xxxx xx xxx xxxxx xx xxxxxxx xxx xxxxxxxxxx xx"

main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
 
  vbox   <- vBoxNew False 0

  set window [
    containerBorderWidth := 10,
    containerChild := vbox ]

  label1 <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  boxPackStart vbox label1 PackNatural 0

  entry  <- entryNew
  entrySetHasFrame entry False
  boxPackStart vbox entry PackNatural 3
  onEditableChanged entry ( 
    entryTextChanged entry)

  timeoutAdd timeIsOut 1000

  widgetShowAll window
  mainGUI

entryTextChanged entry = do 
  txt <- entryGetText entry
  pt  <- getPOSIXTime
  putStrLn ("Entry ## " ++ (show pt) ++ ": " ++ txt)
  return ()

timeIsOut = do
  pt <- getPOSIXTime
  putStrLn ("Timer ## " ++ show pt)
  return True

