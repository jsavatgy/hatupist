# Ajanotosta tulostauluihin

Jotta tulostaulujen sisältöä päästään muuttamaan, tarvitaan siis viitteet näiden taulujen malleihin:

```
data GUI = GUI {
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label,
  gModelR :: ListStore Result,
  gModelS :: ListStore Timing,
  gModelI :: ListStore Interval
}
```

Nämä viitteet luotiin funktiossa `createGUI`:

```
  rModel <- setupView rInitModel rColTitles rColFuncs innerVBox1
  sModel <- setupView sInitModel sColTitles sColFuncs innerVBox1
  iModel <- setupView iInitModel iColTitles iColFuncs innerVBox2
  ...
  return GUI {
    gEntry = entry,
    gLabel1 = label1, gLabel2 = label2,
    gModelR = rModel, gModelS = sModel, gModelI = iModel
  }
```

S-taulun ainoa rivi (rivinumero 0) päivitetään nyt funktiolla `listStoreSetValue`. Sen parametrit ovat viite malliin (`gModelS gui`), rivinumero ja tietorakenne `Timing`, joka sisältää näytettävien kenttien saamat arvot.

```
renewTableS gui gs t = do
  listStoreSetValue (gModelS gui) 0 Timing {
    sSecsLeft = iLeft t,
    sSession = mmss t,
    sTotal = total gs,
    sSpeed = 0.0
  }
```

Ensimmäisen näppäimistönpainalluksen seurauksena kutsuttavaan funktioon `whenNotStarted` on lisätty komento `timeoutAdd`, joka käynnistää ajastimen.

```
whenNotStarted status gui settings lines pt gsRef gs = do
  putStrLn ("Started with " ++ (show status))
  timeoutAdd (onTimeout gui gsRef) 500
```

Ajastimen toiminta määritellään seuraavassa:

```
onTimeout gui gsRef = do
  gs <- readIORef gsRef
  pt <- getPOSIXTime
  let t = secondsFrom (startTime gs) pt
      iCur = iNumber t
  renewTables gui gs t iCur
  writeIORef gsRef gs {
    lastShownIv = iCur
  }
  return True
```

Pienet apufunktiot `iNumber` ja `iLeft` kertovat jakson numeron ja paljonko jaksossa on sekunteja jäljellä.

```
iNumber t =
  floor t `div` iDuration

iLeft t =
  iDuration - (floor t `mod` iDuration)
```

Tilastoitavat näppäimistönpainallukset kerätään oikeisiin intervalleihin funktiossa `addTime`: 

```
addTime status i intervals =
  [newHead] ++ tail newIvs
  where
  newHead = case status of
    Correct -> headIv { iMrks = (iMrks headIv) + 1 }
    Error   -> headIv { iErrs = (iErrs headIv) + 1 }
  headIv = head newIvs
  newIvs = if i /= latestIvNum intervals
    then [zeroInterval { iNum = i }] ++ intervals
    else intervals
```

Käytännössä nämä muodostavat kasvavan taulukon, jossa kenttä `iNum` kertoo jakson numeron, ja kentät `iMrks` ja `iErrs` lyöntimäärät ja virheet seuraavaan tapaan:

```
[Interval {iNum = 1, iMrks = 9, iErrs = 0},Interval {iNum = 0, iMrks = 68, iErrs = 2}]
```
![](../pics/timing-02.png)

Jatkamme tulostietojen käsittelemistä ensi kerralla. Ohjelmakoodi tähän mennessä [timing-02.hs](../code/timing-02.hs)

