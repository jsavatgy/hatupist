This is guide through the programming process of Hatupist, the typing speed practice program using Gtk2hs and Haskell.

A plain window can be created with the following code:

```
import Graphics.UI.Gtk

main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
```

Save this under `plain-ui.hs` and give command `runhaskell plain-ui.hs`. Program will be run in the interpreted mode and will result on following window on screen:

plain-ui.png



