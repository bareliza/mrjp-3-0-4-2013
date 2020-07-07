module X86Env where

import AbsLatte
import qualified Data.Map as Map

localVar :: Env -> String -> Bool
localVar e id = Map.lookup id (lokalne e) /= Nothing

getLocalVarRel :: Env -> String -> Int
getLocalVarRel e id =
  case Map.lookup id (lokalne e) of
    Just (_, i) -> i
    Nothing -> error ("Nie ma zmiennej lokalnej "++id)

getLocalVarType :: Env -> String -> Type
getLocalVarType e id = 
  case Map.lookup id (lokalne e) of
    Just (typ, i) -> typ
    Nothing -> error ("Nie ma zmiennej lokalnej "++id)

getFieldVar :: Env -> String -> Int
getFieldVar e id = 
  let klasaM = Map.lookup (klasa e) (klasy e) in
  case klasaM of
    Just k -> case Map.lookup id k of
      Just (_, i) -> 4*i
      Nothing -> error ("Klasa "++(klasa e)++" nie ma pola "++id)
    Nothing -> error ("Nie ma klasy "++(klasa e))
  
getFieldVarType :: Env -> String -> Type
getFieldVarType e id = 
  let klasaM = Map.lookup (klasa e) (klasy e) in
  case klasaM of
    Just k -> case Map.lookup id k of
      Just (typ, _) -> typ
      Nothing -> error ("Klasa "++(klasa e)++" nie ma pola "++id)
    Nothing -> error ("Nie ma klasy "++(klasa e))

globalFun :: String -> Env -> Bool
globalFun id env = Map.lookup id (funkcje env) /= Nothing

isMethod :: String -> Env -> Bool
isMethod id env = 
  if id=="" then False 
  else 
    let klMet = Map.lookup (klasa env) (klasyMetody env) in
    if klMet == Nothing then False else Map.lookup id (unjust klMet) /= Nothing

getFunReturnType :: Env -> String -> Type
getFunReturnType env id = case Map.lookup id (funkcje env) of
  Just (Fun typ _) -> typ
  Nothing -> error ("Nie ma funkcji globalnej "++id)
  
getMethodIndex :: String -> String -> Env -> Int
getMethodIndex klasa id env = 
  snd (unjust (Map.lookup id (unjust (Map.lookup klasa (klasyMetody env)))))
  
getMethodReturnType :: String -> String -> Env -> Type
getMethodReturnType klasa id env  =
  let funType = 
        fst (unjust 
             (Map.lookup id (unjust (Map.lookup klasa (klasyMetody env)))))
  in case funType of
    Fun typ args -> typ

wolnaEtykietaS :: Env -> String
wolnaEtykietaS env = show (wolnaEtykieta env)

data Env = Env { 
  wolnaLokalna :: Int,
  wolnaTymczasowa1 :: Int,
  wolnaEtykieta :: Int,
  lokalnych :: Int,
  tymczasowych :: Int, 
  parametrow :: Int,
  zwracane :: Type,
  klasa :: String,
  lokalne :: Map.Map String (Type, Int),
  -- pod nazwą "this" wskaźnik do EBP+8, typu bieżąca klasa
  funkcje :: Map.Map String Type,
  klasy :: Map.Map String (Map.Map String (Type, Int)),
  klasyMetody :: Map.Map String (Map.Map String (Type, Int)),
  rozmiaryKlas :: Map.Map String Int,
  rozmiaryKlasMetod :: Map.Map String Int
  } deriving Show

{- 
Niejasny jest ten rozdział pomiędzy parametrami a zmiennymi lokalnymi,
najchętniej bym go zaniechał.
Można to zrobić tak: Na starcie wpisać +8,+12,+16 
itd. jako parametry (+8 to this w metodach), 
a ujemne to kolejne lokalne: -4,-8,-12 itd.
-}
{-
lokalne : nazwy zmiennych lokalnych w parę typ i nr w ramce
wolnaLokalna : pierwsze nie przydzielone pole dla zmiennych lokalnych w ramce
wolnaTymczasowa1 : pierwsze nie przydzielone pole dla tymczasowych w ramce
zwracane : typ zwracany przez aktualnie kompilowaną funkcję
klasa : bieżąca klasa jako typ
funkcje : funkcje globalne - ich sygnatury
wolnaEtykieta : nr etykiety do użycia, zwiększane przy rezerwacji
lokalnych : potrzebna ilość pól dla zmiennych lokalnych
tymczasowych : ilość potrzebnych zmiennych tymczasowych
klasy : 
  szczegóły zawartości klas, wraz z mapowaniem w typy pól,
  oraz nr-y: dla pól adresy względem początku obiektu, 
  zerowe pole zarezerwowane na wskaźnik do tablicy metod wirtualnych
klasyMetody :
  dla metod 
  nr indeksu w tablicy wskaźników do metod dla danej klasy. 
  Taka tablica jest zbudowana w pamięci dla każdej klasy.
  Uwaga: sygnatury metod nie zawierają domyślnego pierwszego parametru metody, 
  którym jest this.
rozmiaryKlas:
  ilość pamięci, potrzebna na przechowanie wszystkich pól danej klasy
  liczona w kwantach 32-bitowych (4-bajtowych).
  'new klasa' tłumaczy się jako 
      malloc(4*rozmiaryKlas[klasa])
      mov $tablicaMetodKlasy$klasa, (%eax)
      lea {$endOfFrame}, %esp
      push %eax
      call klasa$init
rozmiaryKlasMetod:
  ilość pól w tablicy metod wirtualnych danej klasy
  pierwsza metoda to konstruktor, zatem na początku inicjowane jedynką
  (a nie zerem).
-}

{- Ramka:

endOfFrame -4*(k+1+m+1+1) adres końca ramki, na wszelki wypadek -4 od 
                          poprzedniego, choć przypuszczam że można by ładować
                          do esp wartość tego poprzedniego adresu. 
tmpm  EBP-4*(k+m+2) dla m+1 wartości tymczasowych i k+1 zmiennych lokalnych 
                    ostatnia wartość tymczasowa ma ten adres
...
tmp1
tmp0  EBP-4*(k+2) wartości tymczasowe, wykorzystywane przy liczeniu wyrażeń
lock  EBP-4*(k+1) dla k+1 zmiennych lokalnych ostatnia ma ten adres
...
loc1  EBP-8
loc0  EBP-4 zmienna lokalna nr 0. Szkoda, że jest rozdział między
            zmiennymi lokalnymi a parametrami. Inaczej, niż w JVM
            Można to jednak ujednolicić przez pamiętanie offsetu do 
            zmiennych lokalnych w ramce
DL <- EBP+0 DL - Dynamic Link, 
            to odłożony na początku wskaźnik ramki funkcji wołającej 
RA    EBP+4  Return Address - adres powrotu z funkcji
arg0  EBP+8  zerowy parametr. w metodach adres 'this' bieżącego obiektu.
arg1  EBP+12 pierwszy parametr
...
argn  EBP+4*(n+2) n-ty parametr (ilość: n+1)

Można zaoszczędzić na wartościach tymczasowych w sytuacji, gdy szczyt 
zmiennych lokalnych nie pokrywa się ze szczytem tymczasowych.
Jednak komplikowałoby to przydział lokacji zmiennym lokalnym.
Dla każdego bloku trzeba by określać ilość zmiennych lokalnych. 
Chyba zrezygnuję z tego na rzecz takiego podziału jak wyżej.
-}

zajmijLokalna :: Env -> Env
zajmijLokalna e = e{wolnaLokalna = wolnaLokalna e + 1}
zajmijEtykiete :: Env -> Env
zajmijEtykiete e = e{wolnaEtykieta = wolnaEtykieta e + 1}
zajmij2Etykiety :: Env -> Env
zajmij2Etykiety e = e{wolnaEtykieta = wolnaEtykieta e + 2}
zajmijTymczasowa :: Env -> Env
zajmijTymczasowa e = e{wolnaTymczasowa1 = wolnaTymczasowa1 e + 1}
zwolnijTymczasowa :: Env -> Env
zwolnijTymczasowa e = e{wolnaTymczasowa1 = wolnaTymczasowa1 e - 1}

dodajLokalna :: String -> Type -> Env -> Env
dodajLokalna id typ e = 
  (zajmijLokalna e){
    lokalne = Map.insert id (typ, ((-4)-4*(wolnaLokalna e))) (lokalne e)
    }

dodajFunkcje :: String -> Type -> Env -> Env
dodajFunkcje id typ e = e{funkcje = Map.insert id typ (funkcje e)}
                                  
emptyEnv :: Env
emptyEnv = Env {
  wolnaLokalna = 0,
  wolnaTymczasowa1 = 0,
  wolnaEtykieta = 0,
  lokalnych = 0,
  tymczasowych = 0, 
  parametrow = 0,
  zwracane = Void,
  klasa = "",
  lokalne = Map.empty,
  -- pod nazwą "this" wskaźnik do EBP+8, typu bieżąca klasa
  funkcje = Map.empty,
  klasy = Map.empty,
  klasyMetody = Map.empty,
  rozmiaryKlas = Map.empty,
  rozmiaryKlasMetod = Map.empty
}

clearEnv :: Env -> Env
clearEnv env =
  env{
    wolnaLokalna = 0,
    wolnaTymczasowa1 = 0,
    lokalnych = 0,
    tymczasowych = 0, 
    parametrow = 0,
    zwracane = Void,
    klasa = "",
    lokalne = Map.empty}

ustalKlase :: String -> Env -> Env
ustalKlase id e = e{klasa=id}

ustalLokalnych :: Int -> Env -> Env
ustalLokalnych i e = e{lokalnych = i}

ustalTymczasowych :: Int -> Env -> Env
ustalTymczasowych i e = e{tymczasowych = i}

dodajKlase id e = e{
  klasy=Map.insert id (Map.empty) (klasy e), 
  klasyMetody=Map.insert id (Map.empty) (klasyMetody e),
  rozmiaryKlas=Map.insert id 1 (rozmiaryKlas e),
  rozmiaryKlasMetod=Map.insert id 1 (rozmiaryKlasMetod e)
  }

unjust :: Maybe a -> a
unjust (Just a) = a
unjust Nothing = error "unjust"

dodajPoleDoKlasy :: String -> Type -> String -> Env -> Env
dodajPoleDoKlasy pole typ kl e =
  e{klasy=
       Map.insert kl 
       (Map.insert pole 
        (typ, unjust (Map.lookup kl (rozmiaryKlas e)))
        (unjust (Map.lookup kl (klasy e)))
       )
       (klasy e),
    rozmiaryKlas=
      Map.insert kl 
      (1+unjust (Map.lookup kl (rozmiaryKlas e))) 
      (rozmiaryKlas e)
  }
  
dodajMetodeDoKlasy :: String -> Type -> String -> Env -> Env
dodajMetodeDoKlasy metoda typ kl e =
  e{klasyMetody=
       Map.insert kl 
       (Map.insert metoda 
        (typ, unjust (Map.lookup kl (rozmiaryKlasMetod e)))
        (unjust (Map.lookup kl (klasyMetody e)))
       )
       (klasyMetody e),
    rozmiaryKlasMetod=
      Map.insert kl 
      (1+unjust (Map.lookup kl (rozmiaryKlasMetod e))) 
      (rozmiaryKlasMetod e)
  }

offsetWolnejTymczasowej :: Env -> String
offsetWolnejTymczasowej env = 
  show ((-4)*((lokalnych env)+1)-4*(wolnaTymczasowa1 env))
  
wolnaTymczasowa :: Env -> String 
wolnaTymczasowa env =(offsetWolnejTymczasowej env)++"(%ebp)"

endOfFrame :: Env -> String
endOfFrame e = (show ((1+lokalnych e+tymczasowych e)*(-4)))++"(%ebp)"

getObjectSize :: String -> Env -> Int
getObjectSize klasa env = unjust (Map.lookup klasa (rozmiaryKlas env))

this :: String
this = "8(%ebp)"

getClass :: String -> Env -> String
getClass id env = 
  let typ = 
        (if localVar env id then getLocalVarType env id 
         else getFieldVarType env id) in
  case typ of
    Class (PIdent (_, klasa)) -> klasa
    _ -> error "Zmienna nie jest obiektem"

getFunRetClass :: String -> Env -> String
getFunRetClass id env = 
  let typ =
        if isMethod id env then getMethodReturnType (klasa env) id env
          else getFunReturnType env id
  in case typ of
    Class (PIdent (_, klasa)) -> klasa
    _ -> error "Typ Zwracanej wartosci nie jest klasa"
