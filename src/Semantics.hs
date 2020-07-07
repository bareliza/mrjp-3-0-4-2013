module Semantics where

import Environment
import AbsLatte

import Data.List
import qualified Data.Map as Map

checkProgram :: Program -> [String]
checkProgram (Program topdefs) = let env = enterFunTypes emptyEnv topdefs in
  (checkTopDefs topdefs)++(checkCorrectMain env topdefs)++
  concat (map (checkFun env) topdefs)

--- Pierwsza faza - kontrola typów i deklaracji -------------

enterArg :: Env -> Arg -> Env
enterArg (env1, (h:decls), funType, envFun) (Arg typ (PIdent (posn, id))) =
    ((changeEnv1 env1 typ id), ((union [id] h):decls), funType, envFun)  

enterArgs :: Env -> [Arg] -> Env
enterArgs env args = enterArgs1 (enterBlock env) args

enterArgs1 env [] = env
enterArgs1 env (h:s) = enterArgs1 (enterArg env h) s

args2Types :: [Arg] -> [Type]
args2Types args = map (\(Arg typ _) -> typ) args

topDef2FunType :: TopDef -> Type
topDef2FunType (FnDef typ _ args _) = Fun typ (args2Types args)
  
topDef2Id :: TopDef -> String
topDef2Id (FnDef _ (PIdent (_, id)) _ _) = id

enterFunTypes :: Env -> [TopDef] -> Env
enterFunTypes env [] = env
enterFunTypes env (h:s) = enterFunTypes (enterFunType env h) s 

enterFunType :: Env -> TopDef -> Env
enterFunType (env1, decls, typ, envFun) topdef =
  let (id, typ2) = (topDef2Id topdef, topDef2FunType topdef) in
  if Map.lookup id envFun /= Nothing 
  then (env1, decls, typ, envFun)
  else (env1, decls, typ, Map.insert id typ2 envFun)

checkCorrectMain env topdefs =
  case functionSignature env "main" of
    Just (Fun Int []) -> []
    Nothing -> ["Nie zdefiniowana funkcja main"]
    _ -> 
      let pids = map topDefPName topdefs in
       let Just (PIdent (posn, _)) = 
             (find (\(PIdent (_, id)) -> id=="main") pids) 
       in [(showPosn posn)++"Bledna sygnatura funkcji main"]

{- Po namysle jednak usuwam wywolanie enterBlock jako malo sensowne -
zatem zmienne zadeklarowane w glownym bloku koliduja z argumentami.
-}
checkFun :: Env -> TopDef -> [String]
checkFun env (FnDef typ pid args (Block stmts)) =
  let newEnv = {- enterBlock -} (enterArgs (enterCurrFunType env typ) args)
  in (checkArgs args)++(check newEnv stmts)++
     if typ /= Void then (checkFunReturn pid (BStmt (Block stmts))) else []
     
-- Funkcja z założenia w argumencie stmt ma Block
checkFunReturn :: PIdent -> Stmt -> [String]
checkFunReturn (PIdent (posn, id)) stmt =
  if (stmtReturns stmt) then [] else
    [showPosn posn++"Funkcja "++id++
     ": nie kazda sciezka wykonania zwraca wartosc funkcji."]

enterCurrFunType :: Env -> Type -> Env
enterCurrFunType (a,b,_,d) t = (a,b,t,d)

topDefPName :: TopDef -> PIdent
topDefPName (FnDef _ pid _ _) = pid

argPName :: Arg -> PIdent
argPName (Arg _ pid) = pid

pidEq :: PIdent -> PIdent -> Bool
pidEq (PIdent (_, id1)) (PIdent (_, id2)) = id1 == id2

-- for (i=1 to n-1)
--  for (j=i+1 to n) 
--    if fname[i]==fname[j] then wypisz fpos++"Powtorzone def. fun."
-- Niestety nie jest to zbyt funkcyjne, ale nie mialem innego pomyslu,
-- po za tym to rozwiazanie iteracyjne wydaje sie bardzo proste,
-- choć takie operowanie listami jest zapewne kosztowne
checkTopDefs :: [TopDef] -> [String]
checkTopDefs defs = 
  let pids = 
        [(PIdent ((-1,-1), "printInt")), 
         (PIdent ((-1,-1), "printString")),
         (PIdent ((-1,-1), "readInt")),
         (PIdent ((-1,-1), "readString")),
         (PIdent ((-1,-1), "error"))]++
        (map topDefPName defs)
  in
  let iS = [1..(length pids)-1] in
  concat (map (\i -> repetitionAt i msgTopDefs pids) iS)
  
voidArg arg = case arg of 
  Arg Void _ -> True
  _ -> False

checkArgs :: [Arg] -> [String]
checkArgs args = 
  (let voidArgs = filter voidArg args
   in map voidArgsMsg voidArgs
  )++
  (let pids = map argPName args in
    let iS = [1..(length pids)-1] in
    concat (map (\i -> repetitionAt i msgArgs pids) iS)
  )
  
voidArgsMsg (Arg Void pid) =
  (showPosn (takePosn pid))++"Argument niedozwolonego typu Void."

msgArgs :: String
msgArgs = "Powtorzony parametr, poprzednio: "
msgTopDefs :: String
msgTopDefs = "Powtorzona definicja funkcji, poprzednio: "

repetitionAt :: Int -> String -> [PIdent] -> [String]
repetitionAt 0 _ _ = [] -- to chyba nie potrzebne, ale niech tam
repetitionAt i msg pids = 
  let ith = (head (drop (i-1) pids)) in
  case find (pidEq ith) (drop i pids) of
    Nothing -> []
    Just kth -> 
      [(showPosn (takePosn kth))++msg++(showPosn (takePosn ith))]

takePosn :: PIdent -> (Int, Int)  
takePosn (PIdent (posn, _)) = posn
  
funRetType env id = let t = getFunTypeM env id in
  case t of
    Just (Fun typ _) -> Just typ
    _ -> Nothing

getExprTypeM :: Env -> Expr -> Maybe Type
getExprTypeM env tree = case tree of
  EVar (PIdent (_, var)) -> getVarTypeM env var
  ELitInt _ -> Just Int
  ELitTrue _ -> Just Bool
  ELitFalse _ -> Just Bool
  EApp (PIdent (_, "readInt")) _ -> Just Int
  EApp (PIdent (_, "readString")) _ -> Just Str                
  EApp (PIdent (_, "printInt")) _ -> Just Void
  EApp (PIdent (_, "printString")) _ -> Just Void
  EApp (PIdent (_, "error")) _ -> Just Void
  EApp (PIdent (_, id)) _ -> funRetType env id
  EString _ -> Just Str
  Neg e -> if (getExprTypeM env e) /= Just Int then Nothing else Just Int
  DNeg e -> if (getExprTypeM env e) /= Just Int then Nothing else Just Int
  Not e -> if getExprTypeM env e == Just Bool then Just Bool else Nothing
  EMul e1 _ e2 -> intType env e1 e2
  EAdd e1 Plus e2 -> intOrStrType env e1 e2 
  EAdd e1 Minus e2 -> intType env e1 e2
  ERel e1 _ e2 -> sameTypesGivesBool env e1 e2
  EAnd e1 e2 -> bothBoolsGivesBool env e1 e2
  EOr e1 e2 -> bothBoolsGivesBool env e1 e2

intType env e1 e2 = case (getExprTypeM env e1, getExprTypeM env e2) of
  (Just Int, Just Int) -> Just Int
  (_, _) -> Nothing
  
intOrStrType env e1 e2 = case (getExprTypeM env e1, getExprTypeM env e2) of
  (Just Int, Just Int) -> Just Int
  (Just Str, Just Str) -> Just Str
  (_, _) -> Nothing

sameTypesGivesBool env e1 e2 = 
  case (getExprTypeM env e1, getExprTypeM env e2) of
    (Just Int, Just Int) -> Just Bool
    (Just Str, Just Str) -> Just Bool
    (Just Bool, Just Bool) -> Just Bool
    (_, _) -> Nothing

bothBoolsGivesBool env e1 e2 = 
  case (getExprTypeM env e1, getExprTypeM env e2) of
    (Just Bool, Just Bool) -> Just Bool
    (_, _) -> Nothing

check :: Env -> [Stmt] -> [String]
check env [] = []
check env (h:s) = 
  let (env2, errors) = step env h 
  in errors++(check env2 s)
  
-- W przypadku powtórzenia deklaracji ważna będzie ostatnia
-- kontrola powtórzeń w checkItems
enterItemIntoEnv :: Env -> Type -> Item -> Env     
enterItemIntoEnv (env1, (h1:decls), funType, envFun) typ h = case h of
  NoInit (PIdent (psn, id)) -> 
    ((changeEnv1 env1 typ id), ((union [id] h1):decls), funType, envFun)
  Init (PIdent (psn, id)) e -> 
    ((changeEnv1 env1 typ id), ((union [id] h1):decls), funType, envFun)
    
enterItemsIntoEnv env _ [] = env
enterItemsIntoEnv env typ (h:s) = 
  enterItemsIntoEnv (enterItemIntoEnv env typ h) typ s
                               
checkItems :: Env -> Type -> [Item] -> [String]
checkItems _ _ [] = []
checkItems env typ (h:items) =
  (checkItem1 env typ h)++
  (checkItems (enterItemIntoEnv env typ h) typ items)


varDeclInBlock :: Env -> String -> Bool
varDeclInBlock (env1, (h:decls), funType, envFun) id =
  (find (\s -> s==id) h) == Just id

checkItem1 :: Env -> Type -> Item -> [String]
checkItem1 env typ item = case item of
  NoInit (PIdent (psn, id)) ->
    (if varDeclInBlock env id then
      [(showPosn psn)++"Zmienna zadeklarowana po raz kolejny w bloku."]
     else [])
  Init (PIdent (psn, id)) expr -> 
    (if varDeclInBlock env id then
      [(showPosn psn)++"Zmienna zadeklarowana po raz kolejny w bloku."]
     else [])++
    (case (getExprTypeM env expr) of
        Nothing -> []
        Just a -> 
          if a==typ then [] 
          else [(showRangePosn expr)++"Typ inny niz deklarowanej zmiennej."]
    )++
    checkExpr env expr

step :: Env -> Stmt -> (Env, [String])
step env stmt = case stmt of
  Decl typ items -> 
    case typ of
      Void -> (env, 
               [showPosn (getFstItemPosn items)++
                "Zmienna(e) zadeklarowana(e) jako niedozwolonego typu Void."])
      _ -> (enterItemsIntoEnv env typ items, checkItems env typ items)
  _ -> (env, step1 env stmt)
  
getItemPosn :: Item -> (Int, Int)
getItemPosn item = case item of
  NoInit (PIdent (psn, _)) -> psn
  Init (PIdent (psn, _)) _ -> psn 

getFstItemPosn :: [Item] -> (Int, Int)
getFstItemPosn (h:s) = getItemPosn h
  
step1 :: Env -> Stmt -> [String]
step1 env stmt = case stmt of
  Empty -> []
  BStmt (Block stmts) -> check (enterBlock env) stmts
  Ass pid expr -> (checkAss env pid expr)++(checkExpr env expr)
  Incr (PIdent (psn, id)) ->
    case getVarTypeM env id of
      Nothing -> [(showPosn psn)++"Inkrementacja nie zadeklarowanej zmiennej."]
      Just Int -> []
      _ -> [(showPosn psn)++"Inkrementacja zmiennej typu innego niz int."]
  Decr (PIdent (psn, id)) ->
    case getVarTypeM env id of
      Nothing -> [(showPosn psn)++"Dekrementacja nie zadeklarowanej zmiennej."]
      Just Int -> []
      _ -> [(showPosn psn)++"Dekrementacja zmiennej typu innego niz int."]
  Ret (PReturn (psn, _)) expr -> 
    (checkExpr env expr) ++
    case (Just (getCurrFunRetType env), getExprTypeM env expr) of
      (_, Nothing) -> []
      (Just a, Just b) -> 
        if a==b then [] 
        else [(showRangePosn expr)++
              "Zwracana wartosc typu innego niz biezacej funkcji."]
  VRet (PReturn (psn, _)) -> if getCurrFunRetType env==Void then [] 
          else [(showPosn psn)++
                ": Powrot z funkcji bez zwrocenia wartosci odpowiedniego typu."]
  Cond expr stmt -> 
    (checkExpr env expr) ++
    (case getExprTypeM env expr of
        Nothing -> []
        Just Bool -> []
        _ -> [(showRangePosn expr)++
              "Inny typ niz boolean w warunku if-a."]
     )++(snd (step env stmt))
  CondElse expr s1 s2 -> 
    (checkExpr env expr) ++
    (case getExprTypeM env expr of
        Nothing -> []
        Just Bool -> []
        _ -> [(showRangePosn expr)++
              "Inny typ niz boolean w warunku if-a."]
     )++(snd (step env s1))++(snd (step env s2))
  While expr stmt ->
    (checkExpr env expr) ++
    (case getExprTypeM env expr of
        Nothing -> []
        Just Bool -> []
        _ -> [(showRangePosn expr)++
              "Inny typ niz boolean w warunku while-a."]
     )++(snd (step env stmt))
  SExp expr ->
    (checkExpr env expr)

checkAss env (PIdent (psn, id)) expr = 
  case (getVarTypeM env id, getExprTypeM env expr) of
    (Nothing, _) -> 
      [(showPosn psn)++"Przypisanie na nie zadeklarowana zmienna."]    
    (_, Nothing) -> []
    (Just a, Just b) -> 
      if a==b then [] else
        [(showPosn psn)++
         "Przypisanie wyrazenia innego typu, niz docelowej zmiennej."]

checkExprs :: Env -> [Expr] -> [String]
checkExprs env [] = []
checkExprs env (h:s) = (checkExpr env h)++(checkExprs env s)

checkEApp env (PIdent (posn, id)) exprs =
  map ((showPosn posn)++)
  (case getFunTypeM env id of
    Nothing -> ["Funkcja nie zdefiniowana."]
    Just (Fun typ types) ->
      if length types /= length exprs then 
        ["Zla liczba argumentow funkcji."]
      else 
        let (typesM, exprTypesM) = 
              (map (Just) types, map (getExprTypeM env) exprs) 
        in
         if typesM == exprTypesM then []
         else ["Zly typ argumentu(ow) nr: "++
               (formattedDiffPosns (diffPosns typesM exprTypesM))]
  )

-- daje listę numerów tych elementów dwóch list, dla których listy się różnią.
-- Działa dla list o równych długościach.
diffPosns l1 l2 = 
  snd $ unzip $ filter (\(x,_) -> x) $ 
  zip (map (\(x,y) -> x/=y) $ zip l1 l2) [1..]

-- Przetestowac !
-- formattedDiffPosns h = show h
formattedDiffPosns (h:s) = foldl (\s t -> s++", "++(show t)) (show h) s
-- Dziala przy zalozeniu, ze lista sklada sie choc z jednego elementu,
-- co zapewnia w funkcji checkEApp ostatnie porównanie - listy różniące się
-- nie mogą być puste

unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing = error "unjust nie dziala dla Nothing"

checkExpr :: Env -> Expr -> [String]
checkExpr env expr = case expr of
  EVar (PIdent (posn, id)) -> 
    case getVarTypeM env id of
      Nothing -> [(showPosn posn)++"Uzycie nie zadeklarowanej zmiennej."]
      _ -> []
  ELitInt _ -> []
  ELitTrue _ -> []
  ELitFalse _ -> []
  EApp (PIdent (psn, "readInt")) exprs ->
    if exprs /= [] then
      [(showPosn psn)++
       "Inna niz 0 liczba argumentow readInt."] 
    else []
  EApp (PIdent (psn, "readString")) exprs ->
    if exprs /= [] then
      [(showPosn psn)++
       "Inna niz 0 liczba argumentow readString."] 
    else []
  EApp (PIdent (psn, "error")) exprs -> 
    if exprs /= [] then
      [(showPosn psn)++
       "Inna niz 0 liczba argumentow error."] 
    else []
  EApp (PIdent (psn, "printInt")) exprs ->
    (checkExprs env exprs)++
    if length exprs /= 1 then
      [(showPosn psn)++
       "Inna niz jeden liczba argumentow printInt."] 
    else
      case getExprTypeM env (head exprs) of
        Nothing -> []
        Just Int -> []
        _ -> [(showRangePosn (head exprs))++
            "Inny niz Int typ argumentu printInt."]
  EApp (PIdent (psn, "printString")) exprs -> 
    (checkExprs env exprs)++
    if length exprs /= 1 then
      [(showPosn psn)++
       "Inna niz jeden liczba argumentow printString."] 
    else
      case getExprTypeM env (head exprs) of
        Nothing -> []
        Just Str -> []
        _ -> [(showRangePosn (head exprs))++
            "Inny niz String typ argumentu printString."]
  EApp pid exprs -> (checkExprs env exprs)++(checkEApp env pid exprs)
  EString _ -> []
  Neg expr -> 
    (checkExpr env expr) ++ 
    case (getExprTypeM env expr) of 
      Just Int -> [] 
      Nothing -> []
      _ -> [(showRangePosn expr)++
            "Negacja tego wyrazenia majacego typ rozny niz int."]
  DNeg expr -> 
    (checkExpr env expr) ++ 
    case (getExprTypeM env expr) of 
      Just Int -> [] 
      Nothing -> []
      _ -> [(showRangePosn expr)++
            "Negacja tego wyrazenia majacego typ rozny niz int."]
  Not expr -> 
    (checkExpr env expr) ++ 
    case (getExprTypeM env expr) of 
      Just Bool -> [] 
      Nothing -> []
      _ -> [(showRangePosn expr)++
            "Zanegowanie tego wyrazenia majacego typ rozny niz boolean."]
  EMul e1 _ e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Just Int, Just Int) -> []
      (Nothing, Nothing) -> []
      (a, b) -> 
        ["Typ wyrazen(a) inny niz int w mnozeniu, dzieleniu lub modulo:"++
         (if a/=Just Int && a/=Nothing then 
            " Podwyrazenie 1: " ++ (showRangePosn1 e1) else [])++ 
         (if b/=Just Int && a/=Nothing then 
            " Podwyrazenie 2: " ++ (showRangePosn1 e2) else [])]
    ++ (checkExpr env e2)
  EAdd e1 Minus e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Just Int, Just Int) -> []
      (Nothing, Nothing) -> []
      (a, b) -> 
        ["Typ wyrazen(a) inny niz int w odejmowaniu:"++
         (if a/=Just Int && a/=Nothing then 
            " Podwyrazenie 1: " ++ (showRangePosn1 e1) else [])++ 
         (if b/=Just Int && a/=Nothing then 
            " Podwyrazenie 2: " ++ (showRangePosn1 e2) else [])]
    ++ (checkExpr env e2)
  EAdd e1 Plus e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Just Int, Just Int) -> []
      (Just Str, Just Str) -> []
      (Nothing, Nothing) -> []
      (Just Str, Nothing) -> []
      (Just Int, Nothing) -> []
      (Nothing, Just Str) -> []
      (Nothing, Just Int) -> []
      (a, b) -> 
        ["Niewlasciwy(e) typ(y) wyrazen(a) w dodawaniu:"++
         (if a/=Nothing then 
            " Podwyrazenie 1 typu "++(show (unjust a))++": "++ 
            (showRangePosn1 e1) else [])++
         (if b/=Nothing then
            " Podwyrazenie 2 typu "++(show (unjust b))++": " ++ 
            (showRangePosn1 e2) else [])]
    ++ (checkExpr env e2)
  ERel e1 op e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Nothing, _) -> []
      (_, Nothing) -> []
      (Just Str, Just Str) ->
        if op == EQU || op == NE then [] else
          ["Porownanie porzadkowe stringow:"++
           " Podwyrazenie 1: "++ (showRangePosn1 e1)++ 
           " Podwyrazenie 2: "++ (showRangePosn1 e2)]
      (Just Bool, Just Bool) ->
        if op == EQU || op == NE then [] else
          ["Porownanie porzadkowe booleanow:"++
           " Podwyrazenie 1: "++ (showRangePosn1 e1)++ 
           " Podwyrazenie 2: "++ (showRangePosn1 e2)]
      (Just a, Just b) -> 
        if a==b then [] else
          ["Niewlasciwe typy wyrazen w porownaniu:"++
           " Podwyrazenie 1 typu "++(show a)++": " ++ (showRangePosn1 e1)++ 
           " Podwyrazenie 2 typu "++(show b)++": " ++ (showRangePosn1 e2)]
    ++ (checkExpr env e2)
  EAnd e1 e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Nothing, Nothing) -> []
      (Nothing, Just Bool) -> []
      (Just Bool, Nothing) -> []
      (Just Bool, Just Bool) -> []
      (a, b) -> 
        ["Typ(y) wyrazen(a) inne(y) niz boolean w iloczynie logicznym:"++
         (if a/=Just Bool && a/=Nothing then 
            " Podwyrazenie 1 typu "++(show (unjust a))++": " ++ 
            (showRangePosn1 e1)
          else [])++ 
         (if b/=Just Bool && b/=Nothing then 
            " Podwyrazenie 2 typu "++(show (unjust b))++": " ++ 
            (showRangePosn1 e2)
          else [])]
    ++ (checkExpr env e2)
  EOr e1 e2 ->
    (checkExpr env e1) ++
    case (getExprTypeM env e1, getExprTypeM env e2) of
      (Nothing, Nothing) -> []
      (Nothing, Just Bool) -> []
      (Just Bool, Nothing) -> []
      (Just Bool, Just Bool) -> []
      (a, b) -> 
        ["Typ(y) wyrazen(a) inne(y) niz boolean w sumie logicznej:"++
         (if a/=Just Bool && a/=Nothing then 
            " Podwyrazenie 1 typu "++(show (unjust a))++": " ++ 
            (showRangePosn1 e1)
          else [])++ 
         (if b/=Just Bool && b/=Nothing then 
            " Podwyrazenie 2 typu "++(show (unjust b))++": " ++ 
            (showRangePosn1 e2)
          else [])]
    ++ (checkExpr env e2)
    
--- wyświetlanie pozycji błędów --------

showPosn :: (Int, Int) -> String
showPosn (-1, -1) = "Funkcja wbudowana."
showPosn (row, col) = (show row)++":"++(show col)++": "

showRangePosn1 :: Expr -> String
showRangePosn1 expr = 
  let ((startRow,startCol),(endRow,endCol)) = 
        (getStartPosn expr, getEndPosn expr) 
  in (show startRow)++":"++(show startCol)++": - "++
     (show endRow)++":"++(show endCol)++":"

showRangePosn :: Expr -> String
showRangePosn expr = "Wyrazenie "++(showRangePosn1 expr)++" "

getStartPosn :: Expr -> (Int, Int)
getStartPosn expr = case expr of
  EVar (PIdent (posn, _)) -> posn
  ELitInt (PInteger (posn, _)) -> posn
  ELitTrue (PTrue (posn, _)) -> posn
  ELitFalse (PFalse (posn, _)) -> posn
  EApp (PIdent (posn, _)) _ -> posn
  EString (PString (posn, _)) -> posn
  Neg e1 -> getStartPosn e1
  DNeg e1 -> getStartPosn e1
  Not e1 -> getStartPosn e1
  EMul e1 _ e2 -> getStartPosn e1
  EAdd e1 _ e2 -> getStartPosn e1
  ERel e1 _ e2 -> getStartPosn e1
  EAnd e1 e2 -> getStartPosn e1
  EOr e1 e2 -> getStartPosn e1

endStrPosn :: (Int, Int) -> String -> (Int, Int)
endStrPosn (a,b) "" = (a,b)
endStrPosn (a,b) str = (a,b+(length str)-1)

getEndPosn :: Expr -> (Int, Int)
getEndPosn expr = case expr of
  EVar (PIdent (posn, s)) -> endStrPosn posn s
  ELitInt (PInteger (posn, s)) -> endStrPosn posn s
  ELitTrue (PTrue (posn, s)) -> endStrPosn posn s
  ELitFalse (PFalse (posn, s)) -> endStrPosn posn s
  EApp (PIdent (posn, s)) exprs -> 
    case exprs of 
      [] -> endStrPosn posn s
      l -> getEndPosn (last l)
  EString (PString (posn, s)) -> endStrPosn posn s
  Neg e1 -> getEndPosn e1
  DNeg e1 -> getEndPosn e1
  Not e1 -> getEndPosn e1
  EMul e1 _ e2 -> getEndPosn e2
  EAdd e1 _ e2 -> getEndPosn e2
  ERel e1 _ e2 -> getEndPosn e2
  EAnd e1 e2 -> getEndPosn e2
  EOr e1 e2 -> getEndPosn e2

--- Druga faza - sprawdzanie zwracania wartości ------------------

{- Tu zamiast określać typ wyrażenia wystarczy wywołać 
dla każdego wyrażenia każdą z funkcji obliczających ewentualną wartość stałą:

evalConstIntExprM
evalConstStrExprM
evalConstBoolExprM

(jeżeli wyrażenie nie jest stałą, zwracają Nothing)

i jeżeli któraś da wartość, to
oznacza że mamy stały napis określonego typu. Uwaga na dzielenie przez 0. 

Obawiam się tylko złożoności wykładniczej: dla każdego liścia wyrażenia
koszt 3^(długość ścieżki od korzenia).

Ale z kolei dla typowych wyrażeń 
ta głębokość z reguły nie przekracza 3, zatem dla takiej gł.
mamy 2^3 liści * 3^3 wywołań to daje 8*9=72. chyba do zaakceptowania. 

No nie wiem:

((2*3+5*7)>(4*1+8*3)) && ("a"+"b" == "ab"+"")

Tak, ale dla operatorów && i || od razu można wywoływać tylko eval..Bool.
a dla + tylko String i Int, a dla pozostałych arytmetycznych tylko Int.

Ogólnie sam operator prawie determinuje typ argumentów. Jedyny wyjątek to +.
I porównania, ale tylko te równościowe. Te porządkowe zakładam że działają
tylko dla Int-a.

-}

-- Jeżeli możliwe na najwyższym poziomie, (poza dodawaniem)
-- że wyrażenie stałe, to zwraca jego przypuszczalny typ
-- w innym przypadku Nothing
getConstExprTypeM :: Expr -> Maybe Type
getConstExprTypeM e = case e of
  EVar _ -> Nothing
  ELitInt _ -> Just Int
  ELitTrue _ -> Just Bool
  ELitFalse _ -> Just Bool
  EApp _ _ -> Nothing
  EString _ -> Just Str
  Neg _ -> Just Int
  DNeg _ -> Just Int
  Not _ -> Just Bool
  EMul _ _ _ -> Just Int
  EAdd e1 Plus e2 -> case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Int, Just Int) -> Just Int
    (Just Str, Just Str) -> Just Str
    _ -> Nothing
  EAdd _ Minus _ -> Just Int
  ERel _ _ _ -> Just Bool
  EAnd _ _ -> Just Bool
  EOr _ _ -> Just Bool

evalConstIntExprM :: Expr -> Maybe Int
evalConstIntExprM e = case e of
  EVar _ -> Nothing
  ELitInt (PInteger (_, str)) -> Just (read str)
  ELitTrue _ -> Nothing
  ELitFalse _ -> Nothing
  EApp _ _ -> Nothing
  EString _ -> Nothing
  Neg e1 -> let mi = evalConstIntExprM e1 in
    if mi == Nothing then Nothing else Just (negate (unjust mi))
  DNeg e1 -> evalConstIntExprM e1
  Not _ -> Nothing
  EMul e1 op e2 -> 
    let (mi1,mi2) = (evalConstIntExprM e1, evalConstIntExprM e2) in
    case (mi1,mi2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      _ -> case op of 
        Times -> Just ((unjust mi1)*(unjust mi2))
        Div -> if mi2 == Just 0 then Nothing else 
                 Just (div (unjust mi1) (unjust mi2))
        Mod -> if mi2 == Just 0 then Nothing else 
                 Just (mod (unjust mi1) (unjust mi2))
  EAdd e1 Plus e2 -> case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Int, Just Int) -> 
      let (mi1,mi2) = (evalConstIntExprM e1, evalConstIntExprM e2) in
      case (mi1,mi2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        _ -> Just ((unjust mi1)+(unjust mi2))
    _ -> Nothing
  EAdd e1 Minus e2 ->
    let (mi1,mi2) = (evalConstIntExprM e1, evalConstIntExprM e2) in
    case (mi1,mi2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      _ -> Just ((unjust mi1)-(unjust mi2))
  ERel _ _ _ -> Nothing
  EAnd _ _ -> Nothing
  EOr _ _ -> Nothing
  
evalConstStrExprM :: Expr -> Maybe String
evalConstStrExprM e = case e of  
  EVar _ -> Nothing
  ELitInt _ -> Nothing
  ELitTrue _ -> Nothing
  ELitFalse _ -> Nothing
  EApp _ _ -> Nothing
  EString (PString (_, unparsedString)) -> Just (read unparsedString)
  Neg _ -> Nothing
  DNeg _ -> Nothing
  Not _ -> Nothing
  EMul _ _ _ -> Nothing
  EAdd e1 Plus e2 -> case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Int, Just Int) -> Nothing
    (Just Str, Just Str) ->       
      let (mi1,mi2) = (evalConstStrExprM e1, evalConstStrExprM e2) in
      case (mi1,mi2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        _ -> Just ((unjust mi1)++(unjust mi2))
    _ -> Nothing
  EAdd _ Minus _ -> Nothing
  ERel _ _ _ -> Nothing
  EAnd _ _ -> Nothing
  EOr _ _ -> Nothing

evalConstBoolExprM :: Expr -> Maybe Bool
evalConstBoolExprM e = case e of
  EVar _ -> Nothing
  ELitInt _ -> Nothing
  ELitTrue _ -> Just True
  ELitFalse _ -> Just False
  EApp _ _ -> Nothing
  EString _ -> Nothing
  Neg _ -> Nothing
  DNeg _ -> Nothing
  Not e1 -> let mb = evalConstBoolExprM e1 in
    if mb == Nothing then Nothing else Just (not (unjust mb))
  EMul _ _ _ -> Nothing
  EAdd _ _ _ -> Nothing
  ERel e1 op e2 ->
    case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Int, Just Int) ->
      let (mi1,mi2) = (evalConstIntExprM e1, evalConstIntExprM e2) in
      case (mi1,mi2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        _ -> case op of 
          LTH -> Just ((unjust mi1)<(unjust mi2))
          LE -> Just ((unjust mi1)<=(unjust mi2))
          GTH -> Just ((unjust mi1)>(unjust mi2))
          GE -> Just ((unjust mi1)>=(unjust mi2))
          EQU -> Just ((unjust mi1)==(unjust mi2))
          NE -> Just ((unjust mi1)/=(unjust mi2))
    (Just Str, Just Str) ->
      let (mi1,mi2) = (evalConstStrExprM e1, evalConstStrExprM e2) in
      case (mi1,mi2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        _ -> case op of 
          EQU -> Just ((unjust mi1)==(unjust mi2))
          NE -> Just ((unjust mi1)/=(unjust mi2))
          _ -> Nothing
    (Just Bool, Just Bool) ->
      let (mi1,mi2) = (evalConstBoolExprM e1, evalConstBoolExprM e2) in
      case (mi1,mi2) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        _ -> case op of 
          EQU -> Just ((unjust mi1)==(unjust mi2))
          NE -> Just ((unjust mi1)/=(unjust mi2))
          _ -> Nothing
    _ -> Nothing
  EAnd e1 e2 -> 
    case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Bool, Just Bool) ->
      let (mi1,mi2) = (evalConstBoolExprM e1, evalConstBoolExprM e2) in
      case (mi1,mi2) of
        (_, Just False) -> Just False
        (Just False, _) -> Just False
        (Just True, Just True) -> Just True
        _ -> Nothing
    _ -> Nothing
  EOr e1 e2 -> 
    case (getConstExprTypeM e1, getConstExprTypeM e2) of
    (Just Bool, Just Bool) ->
      let (mi1,mi2) = (evalConstBoolExprM e1, evalConstBoolExprM e2) in
      case (mi1,mi2) of
        (_, Just True) -> Just True
        (Just True, _) -> Just True
        (Just False, Just False) -> Just False
        _ -> Nothing
    _ -> Nothing

stmtReturns :: Stmt -> Bool 
stmtReturns st = case st of
  Empty -> False
  BStmt (Block stmts) -> 
    if stmts == [] then False else maximum (map stmtReturns stmts)
  Decl _ _ -> False
  Ass _ _ -> False
  Incr _ -> False
  Decr _ -> False
  Ret _ _ -> True
  VRet _ -> True
  Cond e st1 -> 
    if evalConstBoolExprM e == Just True then stmtReturns st1 else False
  CondElse e st1 st2 ->
    case evalConstBoolExprM e of
      Just True -> stmtReturns st1 
      Just False -> stmtReturns st2 
      Nothing -> stmtReturns st1 && stmtReturns st2
  While e st1 ->
    if evalConstBoolExprM e == Just False then False else stmtReturns st1
  SExp (EApp (PIdent (_, "error")) _) -> True
  SExp _ -> False
