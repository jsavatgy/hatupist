# Kirjasin

Kirjasin muutetaan funktiossa `setFonts`, missä vaadittava parametri `fontstring` saa arvon "monospace". Tekstin kokoa voi muuttaa lisäämällä merkkikoon, esimerkiksi "monospace 12". Tasalevyistä kirjoituskonekirjasinta tarvitaan, jotta voimme tyhjentää merkkijonon välilyönneillä kun käyttäjä kirjoittaa sitä. Toinen vaihtoehto olisi piirtää merkkijonot itse, jolloin käyttäisimme funktiota `textExtents` selvittämään tyhjennettävän osuuden dimensiot. Tämä mahdollistaisi myös muiden kirjasimien käytön.

```
setFonts gui fontstring = do
  srcfont <- fontDescriptionFromString fontstring
  widgetModifyFont (gLabel1 gui) (Just srcfont)
  widgetModifyFont (gLabel2 gui) (Just srcfont)
  widgetModifyFont (gEntry gui)  (Just srcfont)
```

Olkoon nyt `n` kirjoitettujen merkkien lukumäärä. Näytettävä merkkijono on tällöin `n` kappaletta välilyöntejä lisättynä merkkijonon häntään, joka jää jäljelle pudottamalla `n` merkkiä pois alusta:

```
blankStart n str =
  replicate n ' ' ++ drop n str
```

Käytämme hyväksemme funktiota `commonPrefix`, ja teemme tarvittavat muutokset puskurin muutokseen vastaavaan tapahtumankäsittelijään `whenBufferChanged`:

```
commonPrefix (x:xs) (y:ys)
  | x == y       = x : commonPrefix xs ys
  | otherwise    = []
commonPrefix _ _ = []

cprfix = length (commonPrefix txt label1Str)

  set (gLabel1 gui) [ 
    labelLabel := blankStart cprfix label1Str]
```

# Tulosten tallentaminen

Ohjelman peruslogiikka alkaa olla valmiina, kunhan vielä tallennamme tulokset. Olemme aikaisemmin määritelleet tulostietotyypin periytymään tyyppiluokista `Show` ja `Read`, joten tietorakenteen lukeminen ja kirjoittaminen on automaattista. Periaattessa tulokset luetaan yksinkertaisten funktioiden `readFile` ja `read` avulla.

Vanhoja tuloksia lukiessamme saatamme todennäköisesti törmätä kahteen ongelmaan: tulostiedostoa ei ole olemassa tai tulostiedot eivät ole jäsenneltävässä muodossa. Kirjastosta `Control.Exception` löytyy funktio `catch`, joka todennäköisesti on yksinkertaisin mahdollinen virheenhallintaan soveltuva funktio. Se saa ensimmäisenä parametrinaan suoritettavan funktion, pyrkii suorittamaan sen, ja poikkeuksen eli virheen sattuessa suorittaa toisena parametrina annetun funktion.

Tiedoston sisältö luetaan ensin muuttujaan `content`. Tiedoston luvun epäonnistuessa oletetaan, että luettiin tyhjä merkkijono `""`. Virheen laadusta emme tässä tapauksessa ole kiinnostuneita.

```
  content <- readFile fname `catch` 
    \(SomeException e) -> return ""
```

Tiedoston sisältö jäsennetään nyt funktiolla `readRs`, jonka toteutus on standardi jäsennysfunktio `readIO`. Funktio `readIO` on kuormitettu funktio, joka tarvitsee tyyppimäärittelyn avulla tiedon siitä, minkätyyppiseen muotoon sen on yritettävä jäsentää merkkijono. Tässä tuo muoto on tyyppiä `Result` olevien alkioiden lista `[Result]`.

```
readRs :: String -> IO [Result]
readRs = readIO

```

Mikäli merkkijonon `content` jäsentäminen ei funktiolla `pFunc` onnistu, palautetaan operaation nolla-alkio, joka tässä tapauksessa on tyhjä lista `[]`.

```
  result <- pFunc content `catch` 
    \(SomeException e) -> return zero

```

Edellä käsitelty on koottuna seuraavassa:


```
resultsFromFile  fname = do
  structFromFile fname readRs []

structFromFile fname pFunc zero = do
  content <- readFile fname `catch` 
    \(SomeException e) -> return ""
  result <- pFunc content `catch` 
    \(SomeException e) -> return zero
  return result

readRs :: String -> IO [Result]
readRs = readIO

resultsFile  = "results.txt"
```

Kaikenkaikkiaan alustustietojen luomiseen ja lukemiseen käytettävä funktio `getStartupConfig` näyttää tässä vaiheessa seuraavalta. Oletuksena on, että ohjelman käyttämät tiedostot tallennetaan vastedes piilohakemistoon "/home/user/.hatupist/", joka luodaan, jollei sitä ole:

```
getStartupConfig gui gsRef = do
  gs <- readIORef gsRef
  -- directory
  homedir <- getHomeDirectory
  let dir = homedir ++ "/.hatupist"
  createDirectoryIfMissing False (dir)
  -- savedResults
  let rname = dir ++ "/" ++ resultsFile
  oldResults <- resultsFromFile rname
  putStrLn ("Reading " ++ rname ++ ": " ++ show (length (oldResults)) ++ " rows")
  listStoreSetValue (gModelR gui) 0 (bestResult oldResults)
  -- other
  setFonts gui "monospace"
  writeIORef gsRef gs {
    homeDirectory = dir,
    results = oldResults
  }

resultsFile = "results.txt"
```

Vastaavasti tulokset tallennetaan ohjelman päättyessä funktioiden `writeFile` ja `show` avulla:

```
quitProgram gsRef = do
  print "Quitting."
  gs <- readIORef gsRef
  let rname = (homeDirectory gs) ++ "/" ++ resultsFile
  writeFile rname (show (results gs))
  putStrLn ("Saving " ++ rname)
  mainQuit
```

Ohjelmakoodi tähän mennessä: [savedResults-01.hs](../code/savedResults-01.hs)



