module TestyTypy where

import AbsLatte
import qualified Data.Map as Map

data Osoba = Osoba { imie :: String, nazwisko :: String, 
                     wiek :: Int } deriving Show

zmienImie :: Osoba -> String -> Osoba
zmienImie o noweImie = 
  Osoba { imie = noweImie, nazwisko = nazwisko o, wiek = wiek o}

zmienWiek :: Osoba -> Int -> Osoba
zmienWiek o nowyWiek = 
  Osoba { imie = imie o, nazwisko = nazwisko o, wiek = nowyWiek}

zwiekszWiek o zmiana =   
  Osoba { imie = imie o, nazwisko = nazwisko o, wiek = wiek o + zmiana}
