module JVMEnv where

import AbsLatte

import qualified Data.Map as Map

type Env1 = Map.Map String Type
type Env2 = Map.Map String (Type, Int)
type Env = (Env2, Int, Type, Env1, Int, Int, Int, String, Bool)

emptyEnv :: Env
emptyEnv = (Map.empty, 0, Void, Map.empty, 0, 0, 0, "", False)
{- Znaczenie pól tupli tablicy symboli / środowisku:
1 - mapowanie nazw zmiennych w numery lokalnych zmiennych na stosie jvm 
2 - numer pierwszej wolnej zmiennej
3 - typ wartości zwracanej przez aktualnie kompilowaną funkcję
4 - mapa identyfikatorów funkcji na ich typy (czy też sygnatury)
5 - numer jeszcze nie wykorzystanej etykiety. Etykiety: Label0, Label1 itd.
6 - osiągnięty limit zmiennych 
7 - osiągnięty limit stosu
8 - nazwa klasy - wzięta z bazy nazwy kompilowanego pliku
9 - nie używane
-}
getFunType :: Env -> String -> Type
getFunType (_, _, _, envFun, _, _, _, _, _) id = unjust $ Map.lookup id envFun

unjust (Just x) = x
unjust Nothing = error "unjust Nothing"

changeEnv2 :: Env2 -> (Type, Int) -> String -> Env2
changeEnv2 env2 info id = Map.insert id info env2
