# Tuloksen mittausindikaattori

Tulos mitataan puolen minuutin välein. Lisäämme ohjelmaan pienen indikaattorin osoittamaan mittaushetken. Indikaattorin väri ilmaisee kuinka korkealle tuloksissa mitattu tulos sijoittuu.

Kokeilemme väriteemaa erillisessä ohjelmassa, jonka laadimme seuraavaksi. 

Värikoodi määräytyy kakkosen logaritmina HSV-värisävyinä. Kylläisyys (Saturation) määritellään vakioksi 0.40. Värisävy (Hue) vaihtelee nollasta ykköseen, likimäärin valon spektrin tavoin. Kirkkaus (Value) vaihtelee välillä (1.00, 0.25).

Tulos väliltä (0,5000) muunnetaan logaritmisena välille (0.00, 12.3).

```
hueLimits  = (0.00, 1.00)
valLimits  = (1.00, 0.25)
limitsFrom = (0.00, 12.3)
```

![](resultColorTheme.png)

```
$ ghci
Prelude> [2^x | x <- [0..12]]
[1,2,4,8,16,32,64,128,256,512,1024,2048,4096]
```

Kun värin HSV-arvo on selvillä, piirretään indikaattori Cairon piirtotoimenpiteiden avulla.

```
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
```

Myös kuvatekstit piirretään Cairon funktioilla.

```
paintText x y text = do
  fntDscr <- liftIO (fontDescriptionFromString "Sans 8")
  layout <- createLayout (text)
  liftIO (layoutSetFontDescription layout (Just fntDscr))
  moveTo x y
  setSourceRGB 0 0 0
  showLayout layout
```

Pienet laatikot piirretään funktiolla `renderWithDrawable`, arvoilla, jotka saadaan aiemmin kuvatulla laskukaavalla.

```
drawCanvas2 canvas _evt = do
  dw <- widgetGetDrawWindow canvas
  mapM
    (\(y,twoToY) -> renderWithDrawable dw (drawBox y twoToY))
    [(y, 2.0**y) | y <- [0.00..12.00]]
  return True
```
Yksittäinen laatikko piirretään Cairon piirtotoimenpiteiden avulla funktiossa `drawBox`.

```
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
```

Ohjelman lähdekoodi: [resultColorTheme.hs](resultColorTheme.hs)

