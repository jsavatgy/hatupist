# Ohjelman tila

Ohjelma on kulloinkin yhdessä seuraavista tilosta:

```
data GameStatus = Error | Correct | Back | NotStarted
  deriving (Eq, Show)
```

`NotStarted`: Ohjelman käynnistyessä, kun yhtään merkkiä ei ole syötetty. Tilarivillä näytetään teksti "Voit aloittaa". Tulostaulut näyttävät nollaa, eikä ajastinta ole käynnistetty.
`Correct`: Käyttäjä on kirjoittanut tekstiä, ja teksti on oikein. Näppäimistönpainallukset rekisteröidään.
`Error`: Käyttäjä on lyönyt virhelyönnin. Virhe rekisteröidään ja käyttäjää pyydetään korjaamaan virheet. Ohjelma on tässä tilassa siihen saakka kunnes teksti on jälleen oikein, jolloin siirrytään takaisin tilaan `Correct`. 
`Back`: Teksti on oikein, mutta käyttäjä (jostain syystä) poistaa merkkejä. Näppäimistönpainalluksista ei tällöin synny rekisteröitävää tietoa.

Tila määräytyy tapahtumankäsittelijässä `whenEntryChanged`. Samalla vanha tila otetaan talteen. Vanha tila `oldStatus` saa arvon nykyiseltä tilalta `status`.

```
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

whenNotStarted status gui settings lines gsRef gs = do
  putStrLn ("Started with " ++ (show status))
  return ()

whenCorrect gui settings lines gsRef gs = do
  print "Correct."
  return ()

whenNewError gui settings lines gsRef gs = do
  print "New Error."
  return ()

whenOther status oldStatus gui settings lines gsRef gs = do
  putStrLn ("Other with " ++ (show (status,oldStatus)))
  return ()
```

Tyypillinen tuloste kokeiltaessa edellistä näyttää tältä:

```
(Correct,NotStarted,"M")
Started with Correct
(Correct,Correct,"Mo")
"Correct."
(Error,Correct,"Mou")
"New Error."
(Back,Error,"Mo")
Other with (Back,Error)
(Correct,Back,"Mor")
"Correct."
(Back,Correct,"Mo")
Other with (Back,Correct)
```

