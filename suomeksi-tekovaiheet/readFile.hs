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


