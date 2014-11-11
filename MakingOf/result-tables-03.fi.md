# Tulosten vertailua

Laskimme edellisellä kerralla intervallit, joilla on merkitystä tuloksen määräytymisessä. Nyt käytämme näiden intervallien taulukkoa parametrina, ja laskemme kyseisen tuloksen:

```
addResult showIvs gs = do
  pt <- getPOSIXTime
  tz <- getCurrentTimeZone
  let newResult0 = zeroResult {
    rDate  = timeFormatted (utcToZonedTime tz (posixSecondsToUTCTime pt)),
    rMrks = sum [iMrks g | g <- showIvs],
    rErrs = sum [iErrs g | g <- showIvs]
  }
  let newResult = newResult0 {
    rRank = tellRank newResult0 (results gs)
  }
  let 
    newRs = take maxRank (insert newResult (results gs))
    newShownRs = [
      bestResult newRs,
      (sessionBest gs) `min` newResult,
      newResult ]
  return (newRs, newShownRs)

timeFormatted :: ZonedTime -> String
timeFormatted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

tellRank x xs =
  case findIndex (x <=) xs of
    Just n  -> n + 1
    Nothing -> length xs + 1
```

Muuttuja `newResult0` on päivämäärän, merkkimäärän ja virhemäärän sisältävä nimetty tietue. Sitä tarvitaan välituloksena, jotta voimme selvittää mille sijalle tuloksissa saavutettu kirjoitusnopeus sijoittuu. Muuttuja `newResult` on vastaava tietue, johon on lisätty kyseinen sijoitus, `rRank`. Sijoitus saadaan funktiosta `tellRank`. Tulostauluun määrittelemme tilaa viidelletuhannelle tulostietueelle. Uusi tulos lisätään oikeaan kohtaan taulukkoon `newRs`, ja tämän jälkeen taulukko typistetään tulostaulun maksimipituuteen.

Tulosten vertaileminen toisiinsa perustuu tyyppiluokkien `Eq` ja `Ord` hyväksikäyttöön. Kirjastosta `List` löytyy koko joukko funktioita, jotka osaavat käyttää tätä ominaisuutta hyväksi, esimerkiksi funktio `insert`.  Luomme tietorakenteelle `Result` instanssin näihin tyyppiluokkiin. Tuloksia kohdellaan samana, mikäli niiden merkkimäärä ja päivämäärä ovat samat. Samaan merkkimäärään päädyttäessä aikaisemmin saavutettua tulosta pidetään parempana verrattuna myöhemmin saavutettuun. Virhemäärä on mukana ainoastaan informaation vuoksi.

```
data Result = Result { 
  rDate :: String, 
  rMrks, rRank, rErrs :: Int
} deriving (Read, Show)

instance Eq Result where
  (Result date1 mrks1 rnk1 errs1) == (Result date2 mrks2 rnk2 errs2) =
    mrks1 == mrks2 && date1 == date2

instance Ord Result where
  compare = fasterFst

fasterFst (Result date1 mrks1 rnk1 errs1) (Result date2 mrks2 rnk2 errs2) =
  if mrks1 /= mrks2 
    then mrks2 `compare` mrks1
    else date1 `compare` date2 
```

Funktio `addResult` palauttaa päivitetyn tulostaulun lisäksi toisena tietueen alkiona taulukon `newShownRs`, johon on laskettuna kolme erityistä tulosta näytettäväksi ruudulla:

```
    newShownRs = [
      bestResult newRs,
      (sessionBest gs) `min` newResult,
      newResult ]

bestResult results = if null results 
  then zeroResult
  else head results
```

Ensimmäinen näistä on kaikkien aikojen paras tulos, toinen istunnon paras tulos ja kolmas viimeisin tulos. Näistä kaikkien aikojen paras tulos on sama kuin istunnon paras tulos, sillä emme toistaiseksi lainkaan tallenna tuloksia. Myös tässä funktio `min` käyttää hyväksi järjestysominaisuutta, joka on seurausta tyyppiluokkiin `Eq` ja `Ord` kuulumisesta.

Jätimme aikaisemmin toteuttamatta S-taulun sarakkeen "Jaksonopeus" laskemisen. Tulin ajatelleeksi, että ehkä sittenkin laskemme tuohon sarakkeeseen hetkellisen nopeuden kymmenen viimeisen sekunnin osalta. Muutamme S-taulun päivitysalgoritmia saadaksemme tarvittavat tiedot talteen:

```
renewTableS gui gs t = do
  pt <- getPOSIXTime
  let newGs = gs {
    speedNows = [(pt, (total gs))] ++ take speedCount (speedNows gs)
  }
  let s = difs (speedNows newGs)
  listStoreSetValue (gModelS gui) 0 Timing {
    sSecsLeft = iLeft t,
    sSession = mmss t,
    sTotal = total gs,
    sSpeed = speed (snd s) (fst s)
  }
  putStrLn (show (snd s) ++ " merkkiä " ++ f01 (fst s) ++ " sekunnissa")
  return newGs
```

Tässä `speedNows gs` on taulukko, johon keräämme kellonajan ja kellonaikaa vastaavan kokonaismerkkimäärän `total gs`. Kun nyt laskemme näiden kunkin erotuksen taulukon alusta ja lopusta, saamme hetkellisen kirjoitusnopeuden:

```
difs speds = 
  if null speds
    then (0.0, 0)
    else (secondsFrom (fst start) (fst end), (snd end) - (snd start))
  where
    start  = last speds
    end    = head speds

speed mrks t = 
  (intToDouble mrks) * 60.0 / (max t 1.0)
```

Olemme tässä jälleen käyttäneet epämääräisiä keinoja huolehtimaan, ettei synny nollalla jakoa tai pään etsimistä tyhjästä listasta.

Ohjelman ikkuna näyttää tässä vaiheessa seuraavalta:

![](result-tables-03.png)

Ohjelmakoodi tähän mennessä: [result-tables-03.hs](result-tables-03.hs)


