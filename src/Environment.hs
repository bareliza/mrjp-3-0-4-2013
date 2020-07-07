module Environment where

import AbsLatte

import qualified Data.Map as Map

{- Environment - Srodowisko - inaczej tez tablica symboli.

Identyfikator jest mapowany po prostu na typ - w koncu po bloku 
po prostu zapominamy zmodyfikowane w bloku 
srodowisko i wracamy do tego przed blokiem. Na potrzeby kontroli semantycznej
to wystarcza.

Wyjasnienie pól tupli tablicy symboli / środowiska:

1 pole - mapa identyfikatorów zmiennych na typy
2 pole - lista list napisów, choć wystarczyłaby 1 lista - 
  w head lista identyfikatorów zmiennych już zadeklarowanych do tej pory w bloku
3 pole - typ - zwracany typ aktualnie sprawdzanej funkcji 
4 pole - mapa identyfikatorów funkcji na ich typy (czy też sygnatury)
-}

type Env1 = Map.Map String Type
type Env = (Env1, [[String]], Type, Env1)

emptyEnv :: Env
emptyEnv = (Map.empty, [], Void, Map.empty)

getCurrFunRetType :: Env -> Type
getCurrFunRetType (_,_,a,_) = a

functionSignature :: Env -> String -> Maybe Type
functionSignature = getFunTypeM

getVarTypeM :: Env -> String -> Maybe Type
getVarTypeM (env1, _, _, _) id = Map.lookup id env1   

getFunTypeM :: Env -> String -> Maybe Type
getFunTypeM (_, _, _, envFun) id = Map.lookup id envFun

changeEnv1 :: Env1 -> Type -> String -> Env1
changeEnv1 env1 typ id = Map.insert id typ env1

enterBlock :: Env -> Env
enterBlock (env1, decls, funType, envFun) = (env1, ([]:decls), funType, envFun)

-- Skąd będę wiedział, że deklaracja identyfikatora się nie powtarza w danym 
-- bloku? I mam powiększyć listę typów zamiast zgłosić błąd?

-- Stąd, że na nie ma go na pierwszej z list w drugim polu tupli.