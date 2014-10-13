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

  sep1       <- hSeparatorNew
  boxPackStart vbox sep1 PackNatural 3

  label1     <- labelNew (Just xxx)
  miscSetAlignment label1 0 0
  boxPackStart vbox label1 PackNatural 0

  label2     <- labelNew (Just xxx)
  miscSetAlignment label2 0 0
  boxPackStart vbox label2 PackNatural 0

  sep2       <- hSeparatorNew
  boxPackStart vbox sep2 PackNatural 3

  textview <- textViewNew
  boxPackStart vbox textview PackNatural 3
 
  statusbar <- statusbarNew
  set statusbar [ statusbarHasResizeGrip := False ]
  boxPackStart vbox statusbar PackNatural 5

  widgetShowAll window
  mainGUI
