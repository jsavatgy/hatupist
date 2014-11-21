# Tilarivi

Lisätään graafisen käyttöliittymän tietotyyppiin kenttä `gStatusbar`, joka on tyyppiä `Label`.

```
data GUI = NotCreated | GUI {
  gErrorCanvas :: DrawingArea,
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label,
  gStatusbar :: Label,
  gStyle :: Style,
  gModelR :: ListStore Result,
  gModelS :: ListStore Timing,
  gModelI :: ListStore Interval
}
```

Luodaan tätä vastaava komponentti funktiossa `CreateGUI`. Haluamme muuttaa tilarivin taustaväriä. Tyypin `Label` komponentti ei huolehdi itse taustaväristä, joten upotamme komponentin tapahtumalaatikkoon `eventbox`.

```
  statusbar <- labelNew Nothing
  miscSetAlignment statusbar 0 0
  miscSetPadding   statusbar 6 0

  eventbox <- eventBoxNew
  containerAdd eventbox statusbar
  boxPackEnd extrmVBox eventbox PackNatural 0
```

Määrittelemme joukon värejä. Cairon piirtomallissa värin punainen, vihreä ja sininen komponentti saa liukulukuarvon nollan ja ykkösen väliltä.

```
blue   = (0.200, 0.400, 1.000)
green  = (0.451, 0.824, 0.086)
red    = (1.000, 0.200, 0.400)
yellow = (0.988, 0.914, 0.310)
black  = (0.000, 0.000, 0.000)
gray   = (0.502, 0.502, 0.502)
white  = (1.000, 1.000, 1.000)
brkRed = (0.886, 0.031, 0.000)

```

Tilarivi käyttää Gtk:n piirtomallia, jossa komponentit ovat kokonaislukuarvoja väliltä 0..65535.

```
toWord x = round (x*65535.0)
toGtkColor (r,g,b) = Color (toWord r) (toWord g) (toWord b)
toGtkColors xs = [toGtkColor x | x <- xs]
```

Virheen sattuessa tilariville ilmestyy teksti "Korjaa virheet!" ja tilarivin taustaväri muuttuu punaiseksi. Muussa tapauksessa tilarivi on tyhjä ja läpinäkyvä.

```
drawStatusText gsRef = do
  gs <- readIORef gsRef
  if (oldStatus gs) /= Error
    then setStatusText "" (toGtkColor white) gs
    else setStatusText "Korjaa virheet!" (toGtkColor red) gs
```

Läpinäkyvyyden saavuttamiseksi otamme komponentin oletustaustavärin talteen käyttöliittymää luotaessa kenttään `gStyle`, joka on tyyppiä `Style`. Se tapahtuu funktiolla `widgetGetStyle`. Tapahtumalaatikko on oliohierarkiassa tilarivin vanhempi ja viite siihen saadaan funktiolla `widgetGetParent`.

```
  style  <- widgetGetStyle window

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

```

Ohjelman käynnistyessä muutamme tilarivin vihreäksi tekstillä "Voit aloittaa."

```
  setStatusText "Voit aloittaa." (toGtkColor green) gs
```

Lisäämme vielä funktiokutsun `drawStatusText` tapahtumankäsittelijään `whenEntryChanged`, jolloin saamme kaikki kolme tilarivin muunnelmaa, kuten kuvassa.

![](statusbar123.png)

Ohjelmakoodi: [statusbar.hs](statusbar.hs)

