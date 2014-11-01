module Main (main) where

import Graphics.UI.Gtk

createMenuBar descr = do 
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
      onActivateLeaf item action
    menuItemNewWithLabelOrMnemonic name
      | elem '_' name = menuItemNewWithMnemonic name
      | otherwise     = menuItemNewWithLabel name

noop = do
  return ()

menuBarDescr = 
  [("_Tiedosto", 
    [("gtk-open", noop),
     ("gtk-select-font", noop),
     ("gtk-preferences", noop),
     ("gtk-about", noop),
     ("gtk-quit", mainQuit)])
  ]

main = do 
  initGUI
  window <- windowNew
  menuBar <- createMenuBar menuBarDescr
  set window [ windowTitle := "Menubar.hs",
               containerChild := menuBar ]
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

