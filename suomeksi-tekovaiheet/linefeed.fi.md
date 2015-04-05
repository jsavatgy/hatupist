# Rivinvaihto

Jotta komponenttien välittäminen funktion parametreina onnistuisi helpommin, keräämme välitettävät komponentit yhteen tietorakenteeseen.

```
data GUI = GUI {
  gEntry :: Entry,
  gLabel1, gLabel2 :: Label
}
```
Asetukset ovat tietorakenteessa `Settings`, niitä ei tässä vaiheessa ole montaa.

```
data Settings = Settings {
  lineLen :: Int, startLine :: Int,
  textfile :: String
} deriving (Read, Show)
```
Asetukset saavat oletusarvonsa funktiossa `defaultSettings`.

```
defaultSettings = Settings {
  lineLen = 40, startLine = 0,
  textfile = "morse.txt"
}
```

Näkyvissä olevat tekstirivit saadaan yksinkertaisella funktiolla `labelStrings`, joka palauttaa kahden rivin taulukon.

```
labelStrings :: Int -> [String] -> [String]
labelStrings startline lines =
  [lines !! first] ++ [lines !! second]
  where
    first = startline `mod` (length lines)
    second = (startline + 1) `mod` (length lines)
```
Kun tekstikentän sisältö muuttuu, tarkistetaan se tapahtumankäsittelijässä `whenEntryChanged` ja sen saavuttaessa saman arvon kuin kirjoitettavana oleva tekstirivi, vaihdetaan se uuteen funktiolla `advanceLine`.

```
whenEntryChanged gui lines gsRef = do
  gs  <- readIORef gsRef
  txt <- entryGetText (gEntry gui)
  let label1Str = head (oLabelStrs gs)
  when (label1Str == txt) (advanceLine gui lines gsRef gs)
  return ()
```

Funktio advanceLine lisää tilamuuttujassa olevan nykyisen rivinumeron `currentLine` arvoa yhdellä. Kun tiedostossa ei ole enää uusia rivejä kirjoitettavaksi, aloitetaan uudelleen alusta. Tämä tapahtuu pitämällä rivinumero annetuissa rajoissa jakojäännösfunktion `mod` avulla.

```
advanceLine gui lines gsRef gs = do
  writeIORef gsRef gs {
    currentLine = ncline
  }
  renewLabels gui ncline lines gsRef
  return ()
  where
    ncline = ((currentLine gs) + 1) `mod` (length lines)
```
Ohjelman tulostama ikkuna:

![](../pics/linefeed.png)

Lähdekoodi [linefeed.hs](../code/linefeed.hs)

