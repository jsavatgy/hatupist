import Graphics.UI.Gtk

xxx = "xxxx xx xxx xxxxx xx xxxxxxx xxx xxxxxxxxxx xx."

main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
 
  vbox <- vBoxNew False 0

  set window [
    containerBorderWidth := 10,
    windowTitle := "Hatupist",
    containerChild := vbox ]

  sep1 <- hSeparatorNew
  boxPackStart vbox sep1 PackNatural 3

  label1 <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  boxPackStart vbox label1 PackNatural 0

  label2 <- labelNew (Just xxx)
  miscSetAlignment label2 0 0
  boxPackStart vbox label2 PackNatural 0

  sep2 <- hSeparatorNew
  boxPackStart vbox sep2 PackNatural 3

  entry <- entryNew
  boxPackStart vbox entry PackNatural 3
 
  widgetShowAll window
  mainGUI

