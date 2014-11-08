# Tekstitiedoston lukeminen

Tekstitiedoston manipulointiin voisi käyttää vaikkapa seuraavanlaista koodia:

```
lineLen = 35

main = do
  originalText <- readFile ("morse.txt")
  print originalText
  let liness = colLines (collectWords (words (originalText)) lineLen)
  let lines = map (++" ") liness
  mapM_ putStrLn (lines)

colLines (xs:xss) =
  (unwords xs) : colLines xss
colLines [] = []

collectWords [] n = []
collectWords ys n =
  p1 : collectWords p2 n
  where
  (p1,p2) = splitAt (length (untilLen ys 0 n)) ys

untilLen (t:ts) s n 
  | s+x<n || s==0  = t : untilLen ts (s+x) n
  | otherwise      = []
  where
  x = length t + 1
untilLen [] s n = []
```

Kirjoitetaan tekstitiedostoon `morse.txt` kokeilumielessä seuraava teksti:

```
Morse-aakkoset.

Graafinen muistisääntö.

Piirretään suuri A-kirjain. Väritetään kirjaimen huippupiste ja 
vaakaviiva. A-kirjain on siten ti-taa.

Seuraavaksi piirretään I-kirjain. Väritetään kirjaimen molemmat päät. 
I-kirjain on ti-ti.

E-kirjain on keskimmäisen poikkiviivan leikkauspiste pystyviivan kanssa. 
E-kirjain on ti.

O-kirjaimessa on pitkästi ympyrän kehää väritettäväksi. Täytetään kehä 
kolmella viivalla. O-kirjain on taa-taa-taa.

U-kirjaimesta väritetään molemmat huippupisteet ja pohjakaari. U-kirjain 
on ti-ti-taa.
```

Edellinen ohjelmakoodi tulostaa nyt:

```
$ runhaskell readFile.hs 
"Morse-aakkoset.\n\nGraafinen muistis\228\228nt\246.\n\nPiirret\228\228n su
uri A-kirjain. V\228ritet\228\228n kirjaimen huippupiste ja vaakaviiva. A-k
irjain on siten ti-taa.\n\nSeuraavaksi piirret\228\228n I-kirjain. V\228rit
et\228\228n kirjaimen molemmat p\228\228t. I-kirjain on ti-ti.\n\nE-kirjain
 on keskimm\228isen poikkiviivan leikkauspiste pystyviivan kanssa. E-kirjai
n on ti.\n\nO-kirjaimessa on pitk\228sti ympyr\228n keh\228\228 v\228ritett
\228v\228ksi. T\228ytet\228\228n keh\228 kolmella viivalla. O-kirjain on ta
a-taa-taa.\n\nU-kirjaimesta v\228ritet\228\228n molemmat huippupisteet ja p
ohjakaari. U-kirjain on ti-ti-taa.\n\n"
Morse-aakkoset. Graafinen 
muistisääntö. Piirretään suuri 
A-kirjain. Väritetään kirjaimen 
huippupiste ja vaakaviiva. 
A-kirjain on siten ti-taa. 
Seuraavaksi piirretään I-kirjain. 
Väritetään kirjaimen molemmat 
päät. I-kirjain on ti-ti. 
E-kirjain on keskimmäisen 
poikkiviivan leikkauspiste 
pystyviivan kanssa. E-kirjain on 
ti. O-kirjaimessa on pitkästi 
ympyrän kehää väritettäväksi. 
Täytetään kehä kolmella viivalla. 
O-kirjain on taa-taa-taa. 
U-kirjaimesta väritetään molemmat 
huippupisteet ja pohjakaari. 
U-kirjain on ti-ti-taa. 
```

Tekstin manipulointiin tarvittavat rutiinit riippuvat hieman tekstitiedoston muodosta, ja periaatteessa tämän vaiheen voisi jopa sivuuttaa muokkaamalla tekstitiedosto valmiiksi tekstieditorilla. Haskell-kielen Prelude-kirjastosta käytettäviä funktioita olivat `words` ja `unwords`, joiden toimintaperiaate seuraavassa:

```
$ ghci
Prelude> let ws = words "U-kirjaimesta väritetään molemmat huippupisteet ja pohjakaari"
Prelude> ws
["U-kirjaimesta","v\228ritet\228\228n","molemmat","huippupisteet","ja","pohjakaari"]
Prelude> unwords ws
"U-kirjaimesta v\228ritet\228\228n molemmat huippupisteet ja pohjakaari"
Prelude> :q
Leaving GHCi.
```

Ohjelmakoodi: [readFile.hs](readFile.hs)

