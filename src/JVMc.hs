module JVMc where

import JVMEnv
import qualified Semantics as Smt
import AbsLatte

import qualified Data.Map as Map
import Data.List

findLastIdx :: Char -> String -> Int
findLastIdx c s = case findIndices (\a -> a == c) s of
  [] -> -1
  lst -> last lst 

basename1 :: FilePath -> String
basename1 s = if (length s > 4) then take ((length s) - 4) s else s 

basename :: FilePath -> String
basename s = 
  case (findLastIdx '/' s, findLastIdx '.' s) of
  (-1, -1) -> s
  (-1, dot) -> take dot s 
  (slash, -1) -> drop (slash+1) s
  (slash, dot) -> drop (slash+1) (take dot s)
                   

jvmc :: FilePath -> Program -> String
  
jvmc fp prog = 
  ".class public "++(basename fp)++"\n"++
  ".super java/lang/Object\n"++
  "\n"++
  ".method public <init>()V\n"++
  "  .limit stack 1\n"++
  "  .limit locals 1\n"++
  "  aload_0\n"++
  "  invokespecial java/lang/Object/<init>()V\n"++
  "  return\n"++
  ".end method\n"++
  "\n"++
  ".method public static main([Ljava/lang/String;)V\n"++
  "  .limit stack 1\n"++
  "  .limit locals 1\n"++
  "  invokestatic "++(basename fp)++"/main()I\n"++
  "  invokestatic java/lang/System/exit(I)V\n"++
  "  return\n"++
  -- "  .throws java/io/IOException\n"++
  ".end method\n\n"++
  (compile fp prog)

compile :: FilePath -> Program -> String
compile filepath (Program topdefs) = 
  compile1 
  (enterClassName (enterFunTypes emptyEnv topdefs) (basename filepath))
  topdefs 

enterClassName :: Env -> String -> Env
enterClassName (a,b,c,d,e,f,g,_,i) classname =
  (a,b,c,d,e,f,g,classname,i)

compile1 :: Env -> [TopDef] -> String
compile1 env [] = ""
compile1 env (td:tds) =
  (compileFun env td)++(compile1 env tds)
  
min1Stack :: Env -> Env
min1Stack (a,b,c,d,e,f,_,h,i) = (a,b,c,d,e,f,1,h,i)

compileFun :: Env -> TopDef -> String
compileFun env f = (funHead f (Smt.topDef2FunType f))++
               (if funReturnsValue env (funName f) then 
                  (funBody (min1Stack env) f)
                else (funBody env f))++
               ".end method\n\n"
               
funHead :: TopDef -> Type -> String
funHead topdef (Fun typ argtypes) =
  ".method public static "++
  (funName topdef)++"("++(types2Str argtypes)++")"++(type2Str typ)++"\n"
  
funName :: TopDef -> String
funName (FnDef _ (PIdent (_,id)) _ _) = id
  
type2Str :: Type -> String
type2Str t = case t of
  Int -> "I"
  Str -> "Ljava/lang/String;"
  Bool -> "Z"
  Void -> "V"
  
types2Str :: [Type] -> String
types2Str types = concat (map type2Str types)

enterArg :: Env -> Arg -> Env
enterArg (env2,i,funType,envFun,j,k,l,s,t) (Arg typ (PIdent (posn, id))) =
  if (i+1)>k then
    ((changeEnv2 env2 (typ, i) id), i+1, funType, envFun, j, i+1, l, s, t)  
  else
    ((changeEnv2 env2 (typ, i) id), i+1, funType, envFun, j, k, l, s, t)  

enterArgs :: Env -> [Arg] -> Env
enterArgs env [] = env
enterArgs env (h:s) = enterArgs (enterArg env h) s

enterFunTypes :: Env -> [TopDef] -> Env
enterFunTypes env [] = env
enterFunTypes env (h:s) = enterFunTypes (enterFunType env h) s 

enterFunType :: Env -> TopDef -> Env
enterFunType (env1, i, typ, envFun, j, k, l, s, t) topdef =
  let (id, typ2) = (Smt.topDef2Id topdef, Smt.topDef2FunType topdef) in
  (env1, i, typ, Map.insert id typ2 envFun, j, k, l, s, t)
  
funBody :: Env -> TopDef -> String
funBody (env2, i, _, envFun, j, k, l, s, t) (FnDef typ _ args block) =
  let (envAfter, code) = 
        (compileStmt 
         (enterArgs (env2, i, typ, envFun, j, k, l, s, t) args) 
         (BStmt block)) in
  "  .limit stack "++(show (getStackLimit envAfter))++"\n"++
  "  .limit locals "++
  (show (max (minStack typ) (getLocalsLimit envAfter)))++"\n"++
  code++
  (case typ of
      Int -> "  iconst_0\n  ireturn\n"
      Bool ->  "  iconst_0\n  ireturn\n"
      Str -> "  ldc \"\"\n  areturn\n"
      Void ->"  return\n")++
  (if stmtThrows (BStmt block) then 
       "  .throws java/io/IOException\n"
   else "")

minStack :: Type -> Int
minStack t = case t of
  Int -> 1
  Bool -> 1
  Str -> 1
  Void -> 0

getStackLimit :: Env -> Int
getStackLimit(_,_,_,_,_,_,l,_,_)=l

getLocalsLimit :: Env -> Int
getLocalsLimit(_,_,_,_,_,l,_,_,_)=l

currFunType :: Env -> Type
currFunType (_, _, typ, _, _, _, _, _, _) = typ

compileItems :: Env -> Type -> [Item] -> (Env, String)
compileItems env typ items = foldl (nextItem typ) (env, "") items 

neutralExpr :: Type -> Expr
neutralExpr typ = case typ of
  Int -> ELitInt (PInteger ((1,1), "0"))
  Str -> EString (PString ((1,1), "\"\""))
  Bool -> ELitFalse (PFalse ((1,1), "false"))

nextItem :: Type -> (Env, String) -> Item -> (Env, String)
nextItem typ (env, s) item = case item of
  NoInit (PIdent (pos, id)) -> 
    let (envAfter, code) =
            compileStmt 
            (addVar typ id env) (Ass (PIdent (pos, id)) (neutralExpr typ))
    in (envAfter, s++code)
  Init (PIdent (pos, id)) expr -> 
    let (envAfter, code) = compileInit env (addVar typ id env) id expr
    in (envAfter, s++code)

compileInit :: Env -> Env -> String -> Expr -> (Env, String)
compileInit env0 env1 id expr =
     (updateEnv env1 expr, 
      (compileExpr env0 expr)++
      case getVarType env1 id of
        Int -> "  istore"++(num2arg (getVarNum env1 id)) ++ "\n"
        Bool -> "  istore"++(num2arg (getVarNum env1 id)) ++ "\n"
        Str -> "  astore"++(num2arg (getVarNum env1 id)) ++ "\n")
  
nextStmt :: (Env, String) -> Stmt -> (Env, String)
nextStmt (env, s) stmt = 
  let (envAfter, src) = compileStmt env stmt in (envAfter, s++src)

addVar :: Type -> String -> Env -> Env
addVar typ id env = enterArg env (Arg typ (PIdent ((1,1), id))) 

forgetLocals :: Env -> Env -> Env
forgetLocals (a,b,c,d,e,f,g,h,i) (a1,b1,c1,d1,e1,f1,g1,h1,i1) =
  (a,b,c1,d1,e1,f1,g1,h1,i1)
  
compileStmt :: Env -> Stmt -> (Env, String)
compileStmt env s = case s of
  Empty -> (env, "")
  BStmt (Block stmts) -> 
    let (envAfter, code) = (foldl nextStmt (env, "") stmts)
    in (forgetLocals env envAfter, code)
  Decl typ items -> compileItems env typ items
  Ass (PIdent (_, id)) expr ->
     (updateEnv env expr, 
      (compileExpr env expr)++
      case getVarType env id of
        Int -> "  istore"++(num2arg (getVarNum env id)) ++ "\n"
        Bool -> "  istore"++(num2arg (getVarNum env id)) ++ "\n"
        Str -> "  astore"++(num2arg (getVarNum env id)) ++ "\n")
  Incr (PIdent (_, id)) -> 
    (env, "  iinc "++(show (getVarNum env id))++" 1\n")
  Decr (PIdent (_, id)) -> 
    (env, "  iinc "++(show (getVarNum env id))++" -1\n")
  Ret _ expr -> 
    (updateEnv env expr, 
     (compileExpr env expr)++ 
     (case currFunType env of
         Int -> "  ireturn\n"
         Bool -> "  ireturn\n"
         Str -> "  areturn\n"))
  VRet _ -> (env, "  return\n")
  Cond expr stmt ->
    let envAfterExpr = updateEnv env expr in
    let labelIfFalse = getLabel envAfterExpr in
    let (envAfterCond, stmtCode) = compileStmt (incLabels envAfterExpr) stmt in
    (envAfterCond,
     (compileExpr env expr)++
     "  ifeq "++labelIfFalse++"\n"++
     stmtCode++
     labelIfFalse++":\n")    
  CondElse expr s1 s2 ->
    let envAfterExpr = updateEnv env expr in
    let labelIfFalse = getLabel envAfterExpr in
    let labelEndIf = getLabel (incLabels envAfterExpr) in
    let envAftExNLbls = incLabels (incLabels envAfterExpr) in
    let (envAftS1, s1Code) = compileStmt envAftExNLbls s1 in
    let (envAftS2, s2Code) = 
          compileStmt (forgetLocals envAftExNLbls envAftS1) s2 in
    ((forgetLocals envAftExNLbls envAftS2),
     (compileExpr env expr)++
     "  ifeq "++labelIfFalse++"\n"++
     s1Code++
     "  goto "++labelEndIf++"\n"++
     labelIfFalse++":\n"++
     s2Code++
     labelEndIf++":\n")
  While expr stmt ->
    let envAfterExpr = updateEnv env expr in
    let labelWhileLoop = getLabel envAfterExpr in
    let labelWhileCond = getLabel (incLabels envAfterExpr) in    
    let envAftExNLbls = incLabels (incLabels envAfterExpr) in
    let (envAftStmt, stmtCode) = compileStmt envAftExNLbls stmt in
    ((forgetLocals envAftExNLbls envAftStmt),
     "  goto "++labelWhileCond++"\n"++
     labelWhileLoop++":\n"++
     stmtCode++
     labelWhileCond++":\n"++
     (compileExpr env expr)++
     "  ifne "++labelWhileLoop++"\n")
  SExp expr -> 
    (updateEnv env expr,
     (compileExpr env expr)++ 
     (case getExprType env expr of
         Void -> ""
         _ -> "  pop\n"))
{-
CondElse ->
   let envAfterExpr = updateEnv env expr in
   let labelIfFalse = getLabel envAfterExpr in
   let labelendIf = getLabel (incLabels envAfterExpr) in
   let envAftExNLbls = incLabels (incLabels envAfterExpr) in
   let (envAftS1, s1Code) = compile envAftExNLbls s1 in
   let (envAftS2, s2Code) = compile (forgetLocals envAftExNLbls envAftS1) s2 in
   ((forgetLocals envAftExNLbls envAftS2),
    compileExpr env expr
      ifeq ifFalse
      s1Code
      goto endIf
    ifFalse:
      s2Code
    endIf:

While ->
   goto whileCond
whileLoop:
   stmtCode
whileCond:
   compileExpr env expr
     ifne whileLoop
-}

-- napis argument operacji iload istore. Zakladam arg w zakresie
--  0..255
num2arg :: Int -> String
num2arg a 
  | a<0 = " sth wrong"
  | a>=0 && a<4 = "_"++(show a) 
  | a>=4 = " "++(show a)

getVarNum :: Env -> String -> Int
getVarNum (env2, _, _, _, _, _, _, _, _) id = snd (unjust (Map.lookup id env2))

getVarType :: Env -> String -> Type
getVarType (env2, _, _, _, _, _, _, _, _) id = fst (unjust (Map.lookup id env2))

-- funkcja różni się o wykomentowane linie w porównaniu z wersją z Semantics.hs
-- Podyktowane jest to założeniem leniwego wyliczenia wyrażeń od lewej do prawej
-- Tylko gdy wyrażenie po lewej jest określone, możemy zignorować to po prawej,
-- gdy jest odwrotnie wyrażenie po lewej trzeba wyliczyć. Tak to rozumiem.
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
    case (Smt.getConstExprTypeM e1, Smt.getConstExprTypeM e2) of
    (Just Int, Just Int) ->
      let (mi1,mi2) = (Smt.evalConstIntExprM e1, Smt.evalConstIntExprM e2) in
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
      let (mi1,mi2) = (Smt.evalConstStrExprM e1, Smt.evalConstStrExprM e2) in
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
    case (Smt.getConstExprTypeM e1, Smt.getConstExprTypeM e2) of
    (Just Bool, Just Bool) ->
      let (mi1,mi2) = (evalConstBoolExprM e1, evalConstBoolExprM e2) in
      case (mi1,mi2) of
        -- (_, Just False) -> Just False
        (Just False, _) -> Just False
        (Just True, Just True) -> Just True
        _ -> Nothing
    _ -> Nothing
  EOr e1 e2 -> 
    case (Smt.getConstExprTypeM e1, Smt.getConstExprTypeM e2) of
    (Just Bool, Just Bool) ->
      let (mi1,mi2) = (evalConstBoolExprM e1, evalConstBoolExprM e2) in
      case (mi1,mi2) of
        -- (_, Just True) -> Just True
        (Just True, _) -> Just True
        (Just False, Just False) -> Just False
        _ -> Nothing
    _ -> Nothing

-- inne niż w Semantics liczenie stałych boolowskich
compileExpr :: Env -> Expr -> String
compileExpr env expr = case Smt.getConstExprTypeM expr of
  Just Int -> 
    let constIntM = Smt.evalConstIntExprM expr in
    case constIntM of
      Nothing -> compileExpr1 env expr
      Just i -> "  ldc "++(show i)++"\n"
  Just Bool -> 
    let constBoolM = evalConstBoolExprM expr in
    case constBoolM of
      Nothing -> compileExpr1 env expr
      Just b -> case b of
        True -> "  iconst_1\n"
        False -> "  iconst_0\n"
  Just Str -> 
    let constStrM = Smt.evalConstStrExprM expr in
    case constStrM of
      Nothing -> compileExpr1 env expr
      Just str -> "  ldc "++(show str)++"\n"
  _ -> compileExpr1 env expr
                             
compileExpr1 :: Env -> Expr -> String
compileExpr1 env expr = case expr of
  EVar (PIdent (_,id)) -> case getVarType env id of
    Int -> "  iload" ++ (num2arg (getVarNum env id)) ++ "\n"
    Bool -> "  iload" ++ (num2arg (getVarNum env id)) ++ "\n"
    Str -> "  aload" ++ (num2arg (getVarNum env id)) ++ "\n"
  ELitInt (PInteger (_, istr)) -> 
    "  ldc "++(show ((read istr)::Int))++"\n"
  ELitTrue _ -> "iconst_1\n"
  ELitFalse _ -> "  iconst_0\n"
  EApp (PIdent (_,id)) exprs -> case id of
    "printInt" -> 
      (compileExpr env (head exprs))++
      "  invokestatic lib/Runtime/printInt(I)V\n"
    "printString" ->
      (compileExpr env (head exprs))++
      "  invokestatic lib/Runtime/printString(Ljava/lang/String;)V\n"
    "readString" ->
      "  invokestatic lib/Runtime/readString()Ljava/lang/String;\n"
    "readInt" ->
      "  invokestatic lib/Runtime/readInt()I\n"
    "error" ->
      "  invokestatic lib/Runtime/error()V\n"
    _ ->
      concat (map (compileExpr env) exprs)++
      "  invokestatic "++(getClassName env)++"/"++
      id++(funSignature2Str env id)++"\n"
  EString (PString (_, str)) -> "  ldc "++str++"\n"
  DNeg e -> compileExpr env e
  Neg e -> (compileExpr env e)++"  ineg\n"
  Not e -> (compileExpr env e)++"  iconst_1\n  ixor\n"
  EMul e1 op e2 -> 
    (compileExpr env e1)++(compileExpr (updateLabels env e1) e2)++
    "  "++(getJVMMulOp op)++"\n"
  EAdd e1 op e2 -> case getExprType env e1 of
    Str ->
      "  new java/lang/StringBuilder\n"++
      "  dup\n"++
      "  invokespecial java/lang/StringBuilder/<init>()V\n"++
      (compileExpr env e1)++
      "  invokevirtual "++
      "java/lang/StringBuilder/append(Ljava/lang/String;)"++
      "Ljava/lang/StringBuilder;\n"++
      (compileExpr (updateLabels env e1) e2)++
      "  invokevirtual "++
      "java/lang/StringBuilder/append(Ljava/lang/String;)"++
      "Ljava/lang/StringBuilder;\n"++
      "  invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;\n"
    _ -> (compileExpr env e1)++(compileExpr (updateLabels env e1) e2)++
         "  "++(getJVMAddOp op)++"\n"
  ERel e1 op e2 ->
    let envLabelsAfter_e1 = updateLabels env e1 in
    let envLabelsAfter_e2 = updateLabels envLabelsAfter_e1 e2 in
    let labelResultTrue = (getLabel envLabelsAfter_e2) in
    let labelAfterExpr = getLabel (incLabels envLabelsAfter_e2) in
    (compileExpr env e1)++
    (compileExpr envLabelsAfter_e1 e2)++
    "  "++
    (case getExprType env e1 of 
      Str -> getJVMaop op
      _ -> getJVMiop op)++" "++labelResultTrue++"\n"++
    "  iconst_0\n"++
    "  goto "++labelAfterExpr++"\n"++
    labelResultTrue++":\n"++
    "  iconst_1\n"++
    labelAfterExpr++":\n"
  EAnd e1 e2 -> 
    let envLabelsAfter_e1 = updateLabels env e1 in
    let labelResultFalse = (getLabel envLabelsAfter_e1) in
    let labelAfterExpr = getLabel (incLabels envLabelsAfter_e1) in
    (compileExpr env e1)++
    "  ifeq "++labelResultFalse++"\n"++
    (compileExpr ((incLabels (incLabels envLabelsAfter_e1))) e2)++
    "  ifeq "++labelResultFalse++"\n"++
    "  iconst_1\n"++
    "  goto "++labelAfterExpr++"\n"++
    labelResultFalse++":\n"++
    "  iconst_0\n"++
    labelAfterExpr++":\n"
  EOr e1 e2 -> 
    let envLabelsAfter_e1 = updateLabels env e1 in
    let labelResultTrue = (getLabel envLabelsAfter_e1) in
    let labelAfterExpr = getLabel (incLabels envLabelsAfter_e1) in
    (compileExpr env e1)++
    "  ifne "++labelResultTrue++"\n"++
    (compileExpr ((incLabels (incLabels envLabelsAfter_e1))) e2)++
    "  ifne "++labelResultTrue++"\n"++
    "  iconst_0\n"++
    "  goto "++labelAfterExpr++"\n"++
    labelResultTrue++":\n"++
    "  iconst_1\n"++
    labelAfterExpr++":\n"

getJVMMulOp :: MulOp -> String
getJVMAddOp :: AddOp -> String
getJVMMulOp op = case op of
  Times -> "imul"
  Div -> "idiv"
  Mod -> "irem"
getJVMAddOp op = case op of
  Plus -> "iadd"
  Minus -> "isub"

getJVMaop :: RelOp -> String
getJVMiop :: RelOp -> String
getJVMaop op = case op of
  EQU -> "if_acmpeq"
  NE -> "if_acmpne"
getJVMiop op  = case op of
  LTH -> "if_icmplt"
  LE -> "if_icmple"
  GTH -> "if_icmpgt"
  GE -> "if_icmpge"
  EQU -> "if_icmpeq"
  NE -> "if_icmpne"

getLabel :: Env -> String
getLabel (_,_,_,_,e,_,_,_,_) = "Label"++(show e)

getClassName :: Env -> String  
getClassName (a,b,c,d,e,f,g,h,i) = h

funSignature2Str :: Env -> String -> String
funSignature2Str env id = case getFunType env id of
  Fun typ argtypes -> "("++(types2Str argtypes)++")"++(type2Str typ)

exprStackSize :: Env -> Expr -> Int
exprStackSize env expr = 
  if exprIsConst expr then 1 else exprStackSize1 env expr

exprStackSize1 :: Env -> Expr -> Int
exprStackSize1 env expr = case expr of
  EVar _ -> 1
  ELitInt _ -> 1
  ELitFalse _ -> 1
  ELitTrue _ -> 1
  EApp (PIdent (_,id)) exprs -> case id of
    "readInt" -> 1
    "readString" -> 1
    _ -> maximumNat 
         (map (\(a,b) -> a+b) (zip (map (exprStackSize env) exprs) [0..]))
  EString _ -> 1
  DNeg e -> exprStackSize env e
  Neg e -> exprStackSize env e
  Not e -> max (exprStackSize env e) 2
  EMul e1 _ e2 -> max (exprStackSize env e1) ((exprStackSize env e2) + 1)
  EAdd e1 _ e2 -> case getExprType env e1 of
    Str -> maximum [2, (exprStackSize env e1) + 1, (exprStackSize env e2) + 1] 
    _ -> max (exprStackSize env e1) ((exprStackSize env e2) + 1)
  ERel e1 _ e2 -> 
    max (exprStackSize env e1) ((exprStackSize env e2) + 1)
  EAnd e1 e2 ->
    max (exprStackSize env e1) (exprStackSize env e2)
  EOr e1 e2 ->
    max (exprStackSize env e1) (exprStackSize env e2)
    
-- stackSize call [a,b,c,d] = max [a,b+1,c+2,d+3,..ss_i+i-1]

exprIsConst :: Expr -> Bool
exprIsConst e = case Smt.getConstExprTypeM e of
  Just Int -> Smt.evalConstIntExprM e /= Nothing
  Just Bool -> evalConstBoolExprM e /= Nothing
  Just Str -> Smt.evalConstStrExprM e /= Nothing
  _ -> False
  
exprLabelsConsumed :: Expr -> Int
exprLabelsConsumed expr = 
  if exprIsConst expr then 0 else exprLabelsConsumed1 expr

exprLabelsConsumed1 :: Expr -> Int
exprLabelsConsumed1 e = case e of
  EVar _ -> 0
  ELitInt _ -> 0
  ELitTrue _ -> 0
  ELitFalse _ -> 0
  EApp _ exprs -> foldr (+) 0 (map exprLabelsConsumed exprs)
  EString _ -> 0
  DNeg e1 -> exprLabelsConsumed e1
  Neg e1 -> exprLabelsConsumed e1
  Not e1 -> exprLabelsConsumed e1
  EMul e1 _ e2 -> exprLabelsConsumed e1 + exprLabelsConsumed e2
  EAdd e1 _ e2 -> exprLabelsConsumed e1 + exprLabelsConsumed e2
  ERel e1 _ e2 -> 2 + exprLabelsConsumed e1 + exprLabelsConsumed e2
  EAnd e1 e2 -> 2 + exprLabelsConsumed e1 + exprLabelsConsumed e2
  EOr e1 e2 -> 2 + exprLabelsConsumed e1 + exprLabelsConsumed e2
  
maximumNat :: [Int] -> Int
maximumNat [] = 0
maximumNat l = maximum l

updateEnv :: Env -> Expr -> Env
updateEnv env expr = updateLabels (updateStackLimit env expr) expr

updateStackLimit :: Env -> Expr -> Env
updateStackLimit (a,b,c,d,e,f,g,h,i) expr  =
  let stackSize = exprStackSize (a,b,c,d,e,f,g,h,i) expr 
  in if stackSize > g then (a,b,c,d,e,f,stackSize,h,i) 
     else (a,b,c,d,e,f,g,h,i)

updateLabels :: Env -> Expr -> Env
updateLabels (a,b,c,d,e,f,g,h,i) expr  =
  (a,b,c,d,e+(exprLabelsConsumed expr),f,g,h,i) 

incLabels :: Env -> Env
incLabels (a,b,c,d,e,f,g,h,i) = (a,b,c,d,e+1,f,g,h,i) 

funRetType :: Env -> String -> Type
funRetType env id = let t = getFunType env id in
  case t of Fun typ _ -> typ

funReturnsValue :: Env -> String -> Bool
funReturnsValue env id = if funRetType env id == Void then False else True

getExprType :: Env -> Expr -> Type
getExprType env tree = case tree of
  EVar (PIdent (_, var)) -> getVarType env var
  ELitInt _ -> Int
  ELitTrue _ -> Bool
  ELitFalse _ -> Bool
  EApp (PIdent (_, "readInt")) _ -> Int
  EApp (PIdent (_, "readString")) _ -> Str                
  EApp (PIdent (_, "printInt")) _ -> Void
  EApp (PIdent (_, "printString")) _ -> Void
  EApp (PIdent (_, "error")) _ -> Void
  EApp (PIdent (_, id)) _ -> funRetType env id
  EString _ -> Str
  Neg e -> Int
  DNeg e -> Int
  Not e -> Bool
  EMul e1 _ e2 -> Int
  EAdd e1 Plus e2 -> case (getExprType env e1, getExprType env e2) of
    (Str, Str) -> Str
    (Int, Int) -> Int
    _ -> error ("Niepoprawne - argumenty dodawania niespodziewanych typow.\n"++
         "(Nie powinno sie wyswietlic).")
  EAdd e1 Minus e2 -> Int
  ERel e1 _ e2 -> Bool
  EAnd e1 e2 -> Bool
  EOr e1 e2 -> Bool

stmtThrows :: Stmt -> Bool
stmtThrows stmt = case stmt of
  Empty -> False
  BStmt (Block stmts) -> maximumBool (map stmtThrows stmts)
  Decl _ items -> maximumBool (map itemThrows items)
  Ass _ expr -> exprThrows expr  
  Incr _ -> False
  Decr _ -> False
  Ret _ expr -> exprThrows expr
  VRet _ -> False
  Cond expr stmt -> exprThrows expr || stmtThrows stmt
  CondElse expr s1 s2 -> exprThrows expr || stmtThrows s1 || stmtThrows s2
  While expr stmt -> exprThrows expr || stmtThrows stmt
  SExp expr ->  exprThrows expr
    
itemThrows :: Item -> Bool
itemThrows (NoInit _) = False
itemThrows (Init _ expr) = exprThrows expr

exprThrows :: Expr -> Bool
exprThrows expression = case expression of
  EVar _ -> False
  ELitInt _ -> False
  ELitTrue _ -> False
  ELitFalse _ -> False
  EApp (PIdent (_, id)) exprs -> 
    maximumBool (map exprThrows exprs) || funThrows id
  EString _ -> False
  DNeg expr -> exprThrows expr
  Neg expr -> exprThrows expr
  Not expr -> exprThrows expr
  EMul e1 _ e2 -> exprThrows e1 || exprThrows e2
  EAdd e1 _ e2 -> exprThrows e1 || exprThrows e2
  ERel e1 _ e2 -> exprThrows e1 || exprThrows e2
  EAnd e1 e2 -> exprThrows e1 || exprThrows e2
  EOr e1 e2 -> exprThrows e1 || exprThrows e2
  
maximumBool :: [Bool] -> Bool
maximumBool [] = False
maximumBool l = maximum l

funThrows :: String -> Bool
funThrows s = s == "readInt" || s == "readString"
