-- TODO: Martin Aas Eliassen
module Oblig1 where

dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix []_ = True  -- Basistilfelle: Dersom andre param starter med første param, true
isPrefix (x:xs) (y:ys) | x==y = isPrefix xs ys -- sjekker rekursivt til false eller første param er ferdig
                       | otherwise = False


-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int,Int)]
locate _ [] = [] -- basistilfelle, dersom ikkje finn.
locate xs ys = finnInd xs ys 0

-- finnInd: Hjelpemetode for å finne indexane til begge str's
finnInd :: String -> String -> Int -> [(Int,Int)] 
finnInd _ [] _ = [] -- Basis
finnInd xs ys tlr = indexa
      where 
          indexa = if isPrefix xs ys 
                    then [(tlr, tlr + (length xs))] ++ finnInd xs (tail ys) (tlr + 1)
                    else finnInd xs (tail ys) (tlr + 1)
-- Oppgave 3 ----------------------------------------------------
translate :: String -> String 
translate a = unwords[x | (x,y) <- dictionary, a `elem` y]

-- Oppgave 4 ----------------------------------------------------
replace :: [(Int,Int)] -> String -> String 
replace [(x,y)] str = if translate (drop x str) /= ""
                       then take x str ++ translate(drop x str)
                       else stjrn (drop (y-x) str) (drop (y-x) str)  ++ (take y str)
-- Lager stjerner.
stjrn :: String -> String -> String 
stjrn xs ys = [if x `elem` ys then '*' else x | x <- ys]


-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String 
toNewspeak str = if str `elem` ele2Liste then translate str else replace(locate str (unwords ele2Liste)) str
ele2Liste = concat[y | (x,y) <- dictionary] -- for å få "newspeak" ordene

-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int 
analytics x y = prosent jada (length x)
                          where
                              jada = length(fjern x y)

-- Hjelpemetode for å finne prosenten
prosent :: (Integral b, Integral a1, Integral a2) => a1 -> a2 -> b
prosent n m = round((fromIntegral n / fromIntegral m)*100)

-- rem1: Hjelpemetode for å fjerne første forekomst av x i listen
rem1 :: Eq a => [a] -> a -> [a] 
rem1 [] n = []
rem1 (y:ys) x | x==y = ys
              | otherwise = y : rem1 ys x

-- fjern: Hjelpemetode for å fjerne de characters'ene som er endret fra input 1 til 2
fjern :: Eq a => [a] -> [a] -> [a]
fjern [][] = []
fjern []x = []
fjern x[] = []
fjern (x:xs) ys | x `elem` ys = fjern xs (rem1 ys x)
                | otherwise = x : fjern xs ys
-- Oppgave 7 ----------------------------------------------------
main :: String -> (String, Int)
main str = (toNewspeak str, analytics (toNewspeak str) str)
