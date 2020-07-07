module X86c where

import AbsLatte
import X86Env

import qualified Data.Map as Map

enterTopDefs :: [TopDef] -> Env
enterTopDefs topdefs = foldl enterTopDef emptyEnv topdefs

args2Types :: [Arg] -> [Type]
args2Types args = map (\(Arg typ _) -> typ) args

topDef2FunType :: TopDef -> Type
topDef2FunType (FnDef typ _ args _) = Fun typ (args2Types args)

topDef2Id :: TopDef -> String
topDef2Id (FnDef _ (PIdent (_, id)) _ _) = id

enterTopDef :: Env -> TopDef -> Env
enterTopDef env topdef = case topdef of
  FnDef _ _ _ _ -> 
    let (id, typ) = (topDef2Id topdef, topDef2FunType topdef) in
    dodajFunkcje id typ env
  ClassDef _ _ _ _ -> env -- Poprawic, zmienic, rozszerzyc (hierarchia klas)

enterBuildInFunctions :: Env -> Env
enterBuildInFunctions e =
  (dodajFunkcje "error" (Fun Void []) 
   (dodajFunkcje "readInt" (Fun Int []) 
    (dodajFunkcje "readString" (Fun Str []) 
     (dodajFunkcje "printInt" (Fun Void [Int]) 
      (dodajFunkcje "printString" (Fun Void [Str]) e)))))
  
enterArgs :: Env -> [Arg] -> Env  
enterArgs env args = foldl enterArg env args

enterArg :: Env -> Arg -> Env
enterArg e (Arg typ (PIdent (_, id))) =
  e{parametrow=(parametrow e+1),
    lokalne=Map.insert id (typ, (8+4*(parametrow e))) (lokalne e)}
  
compileFunction :: Env -> TopDef -> (Env, String)
compileFunction env (FnDef typ (PIdent (_, id)) args block) = 
  let stmt = BStmt block in
  let entryEnv = (enterArgs env args){
          lokalnych=(snd $ calcStmtUsedLocals stmt),
          tymczasowych=calcStmtUsedTemps stmt} in
  let (envPo, kod) = compileStmt entryEnv stmt in
  (envPo,
   ".globl "++id++"\n"++
   "  .type "++id++", @function\n"++
   id++":\n"++
   "  pushl %ebp\n"++
   "  movl %esp, %ebp\n"++
   "  lea "++(endOfFrame entryEnv)++", %esp\n"++      
   kod++
   "  mov %ebp, %esp\n"++
   "  pop %ebp\n"++
   "  ret\n"++
   "\n\n")
compileFunction env _ = (env, "")

compileProgram :: Program -> String
compileProgram (Program topdefs) =
  let startEnv = enterBuildInFunctions (enterTopDefs topdefs) in
  "String$1$1:\n"++
  "  .string \"\"\n"++
  extractStrings topdefs++
  compile1 startEnv topdefs
  
extractStrings :: [TopDef] -> String
extractStrings topdefs = concat $ map extractTopDefStrings topdefs

extractTopDefStrings :: TopDef -> String
extractTopDefStrings td = case td of
  FnDef _ _ _ block -> extractStmtStrings (BStmt block)
  ClassDef _ _ _ fields -> extractClassFieldsStrings fields
  
extractClassFieldsStrings :: [Field] -> String
extractClassFieldsStrings fields =
  concat $ map extractClassFieldString fields
  
extractClassFieldString :: Field -> String
extractClassFieldString f = case f of
  VarField _ _ -> ""
  FnField _ _ _ block -> extractStmtStrings (BStmt block)
  
extractStmtStrings :: Stmt -> String
extractStmtStrings stmt = case stmt of
  Empty -> ""
  BStmt (Block stmts) -> concat $ map extractStmtStrings stmts
  Decl _ items -> concat $ map extractItemStrings items
  Ass fss expr -> (extractFSsStrings fss)++(extractExprStrings expr)
  Incr fss -> extractFSsStrings fss
  Decr fss -> extractFSsStrings fss
  Ret _ expr -> extractExprStrings expr
  VRet _ -> ""
  Cond expr stmt -> (extractExprStrings expr)++(extractStmtStrings stmt)
  CondElse expr stmt1 stmt2 ->
    (extractExprStrings expr)++(extractStmtStrings stmt1)++
    (extractStmtStrings stmt2)
  While expr stmt -> (extractExprStrings expr)++(extractStmtStrings stmt)
  SExp expr -> (extractExprStrings expr)

extractItemStrings :: Item -> String
extractItemStrings item = case item of
  NoInit _ -> ""
  Init _ expr -> extractExprStrings expr
  
extractFSsStrings :: [FieldSelector] -> String
extractFSsStrings fss = concat $ map extractFsStrings fss

extractFsStrings :: FieldSelector -> String
extractFsStrings fs = case fs of
  FVar _ -> ""
  FFun _ exprs -> concat $ map extractExprStrings exprs
  
extractExprStrings :: Expr -> String
extractExprStrings expr = case expr of
  EFields fss -> extractFSsStrings fss
  ECast _ expr1 -> extractExprStrings expr1
  EString (PString ((w,k), str)) ->
           ("String$"++(show w)++"$"++(show k)++":\n"++
           "  .string "++str++"\n")
  DNeg expr1 -> extractExprStrings expr1
  Neg expr1 -> extractExprStrings expr1
  Not expr1 -> extractExprStrings expr1
  EMul e1 _ e2 -> (extractExprStrings e1)++(extractExprStrings e2)
  EAdd e1 _ e2 -> (extractExprStrings e1)++(extractExprStrings e2)
  ERel e1 _ e2 -> (extractExprStrings e1)++(extractExprStrings e2)
  EAnd e1 e2 -> (extractExprStrings e1)++(extractExprStrings e2)
  EOr e1 e2 -> (extractExprStrings e1)++(extractExprStrings e2)
  _ -> ""
  
compile1 :: Env -> [TopDef] -> String
compile1 env [] = ""
compile1 env (td:tds) =
  let(envPo,kod) = (compileFunction env td) in
  kod++(compile1 (clearEnv envPo) tds)
  
compileStmt :: Env -> Stmt -> (Env, String)
compileStmt env s = case s of
  Empty -> (env, "")
  BStmt (Block stmts) -> 
    let (envAfter, code) = (foldl nextStmt (env, "") stmts)
    in (forgetLocals env envAfter, code)
  Decl typ items -> compileItems env typ items
  Ass lval expr ->
    let (kod1, envPoExpr) = compileExpr env expr in
    let (kod2, envPo) = 
          compileFieldsLValue (zajmijTymczasowa envPoExpr) lval in
    (zwolnijTymczasowa envPo,
     "# Ass\n"++
     "#####\n"++
     kod1++
     "  mov %eax, "++(wolnaTymczasowa env)++
     "   # tmp"++(show (wolnaTymczasowa1 env))++"= expr. value \n"++
     kod2++
     "  mov "++(wolnaTymczasowa env)++
     ", %ecx   # ebx=tmp"++(show (wolnaTymczasowa1 env))++"\n"++
     "  mov %ecx, (%eax)\n")     
  Incr lval ->
    let (kod, envPo) = compileFieldsLValue env lval in
    (envPo,
     "# Incr\n"++
     "######\n"++
     kod++
     "  incl (%eax)\n")
  Decr lval ->
    let (kod, envPo) = compileFieldsLValue env lval in
    (envPo,
     "# Decr\n"++
     "######\n"++
     kod++
     "  decl (%eax)\n")
  Ret _ expr -> 
    let (kod, envPo) = compileExpr env expr in
    (envPo,
     "# Ret\n"++
     "#####\n"++
     kod++
     "  mov %ebp, %esp\n"++
     "  pop %ebp\n"++
     "  ret\n")
  VRet _ -> 
    (env,
     "# VRet\n"++
     "######\n"++
     "  mov %ebp, %esp\n"++
     "  pop %ebp\n"++
     "  ret\n")
  Cond expr stmt ->
    let (kodE, envPoE) = compileExpr env expr in
    let (envPo, kodS) = compileStmt envPoE stmt in
    (zajmijEtykiete envPo,
     "# Cond\n"++
     "######\n"++
     kodE++
     "  cmp $1, %eax\n"++
     "  jne ifEnd"++(wolnaEtykietaS envPo)++"\n"++
     kodS++
     "ifEnd"++(wolnaEtykietaS env)++":\n")
  CondElse expr s1 s2 ->
    let (kodE, envPoE) = compileExpr env expr in
    let (envPoS1, kodS1) = compileStmt envPoE s1 in
    let (envPo, kodS2) = compileStmt envPoS1 s2 in
    (zajmijEtykiete envPo,
     "# CondElse\n"++
     "##########\n"++
     kodE++
     "  cmp $1, %eax\n"++
     "  jne ifElse"++(wolnaEtykietaS envPo)++"\n"++
     kodS1++
     "  jmp ifEnd"++(wolnaEtykietaS envPo)++"\n"++
     "ifElse"++(wolnaEtykietaS envPo)++":\n"++
     kodS2++
     "ifEnd"++(wolnaEtykietaS envPo)++":\n")
  While expr stmt ->
    let (kodE, envPoE) = compileExpr env expr in
    let (envPo, kodS) = compileStmt envPoE stmt in
    (zajmijEtykiete envPo,
     "# While\n"++
     "#######\n"++
     "  jmp whileCond"++(wolnaEtykietaS envPo)++"\n"++
     "whileLoop"++(wolnaEtykietaS envPo)++":\n"++
     kodS++
     "whileCond"++(wolnaEtykietaS envPo)++":\n"++
     kodE++
     "  cmp $1, %eax\n"++
     "  je whileLoop"++(wolnaEtykietaS envPo)++"\n")
  SExp expr ->
    let (kod, envPo) = compileExpr env expr in (envPo, kod)
-- compileExpr :: Env -> Expr -> (String, Env)

calcStmtUsedTemps :: Stmt -> Int
calcStmtUsedTemps stmt = case stmt of
  Empty -> 0
  BStmt (Block stmts) -> maximumInt $ map calcStmtUsedTemps stmts
  Decl _ items -> maximumInt $ map calcItemUsedTemps items
  Ass lval expr -> 
    max (calcExprUsedTemps expr) (1+(calcFieldsLValueUsedTemps lval))
  Incr lval -> calcFieldsLValueUsedTemps lval
  Decr lval -> calcFieldsLValueUsedTemps lval
  Ret _ expr -> calcExprUsedTemps expr
  VRet _ -> 0
  Cond expr s1 -> max (calcExprUsedTemps expr) (calcStmtUsedTemps s1)
  CondElse expr s1 s2 -> 
    maximum [(calcExprUsedTemps expr),
             (calcStmtUsedTemps s1),
             (calcStmtUsedTemps s1)]
  While expr s1 -> max (calcExprUsedTemps expr) (calcStmtUsedTemps s1)
  SExp expr -> calcExprUsedTemps expr  

calcItemUsedTemps :: Item -> Int
calcItemUsedTemps item = case item of
  NoInit _ -> 1
  Init _ expr -> calcExprUsedTemps expr

-- Zalozenie: 
--nie ma w instrukcjach warunkowych podinstrukcji prostych bedacych deklaracjami
calcStmtUsedLocals :: Stmt -> (Int, Int)
calcStmtUsedLocals stmt = case stmt of
  Decl _ items -> (length items, 0)
  BStmt (Block stmts) -> 
    let (fsts,snds) = unzip $ map calcStmtUsedLocals stmts in
    (0, (sum fsts) + (maximumInt snds))
  Cond _ s1 -> calcStmtUsedLocals s1
  CondElse _ s1 s2 -> max (calcStmtUsedLocals s1) (calcStmtUsedLocals s2)
  While _ s1 -> calcStmtUsedLocals s1
  _ -> (0, 0)
    
nextStmt :: (Env, String) -> Stmt -> (Env, String)
nextStmt (env, s) stmt = 
  let (envAfter, src) = compileStmt env stmt in (envAfter, s++src)

calcFieldsLValueUsedTemps :: [FieldSelector] -> Int
calcFieldsLValueUsedTemps fss = calcFieldsUsedTemps (EFields fss)

compileFieldsLValue :: Env -> [FieldSelector] -> (String, Env)
compileFieldsLValue env fs = case fs of
    [FVar (PIdent (_,id))] -> 
      (if localVar env id then
         "  lea "++(show (getLocalVarRel env id))++
         "(%ebp), %eax   # eax=adres "++id++"\n"
       else
         "  mov "++this++", %eax   # eax=this\n"++
         "  lea "++(show (getFieldVar env id))++
         "(%eax), %eax   # eax=adres "++id++"\n", 
       env)
    fs1 ->
      case last fs1 of
        FVar (PIdent (_,id)) -> 
          let (kod, prevEnv, envPo) = 
                compileFields env (EFields (take (length fs1-1) fs1)) in
          (kod++
           "  lea "++(show (getFieldVar prevEnv id))++
           "(%eax), %eax   # eax=adres ... ."++id++"\n", envPo)

compileItems :: Env -> Type -> [Item] -> (Env, String)
compileItems env typ items = foldl (nextItem typ) (env, "") items 

neutralExpr :: Type -> Expr
neutralExpr typ = case typ of
  Int -> ELitInt (PInteger ((1,1), "0"))
  Str -> EString (PString ((1,1), "\"\""))
  Bool -> ELitFalse (PFalse ((1,1), "false"))
  _ -> ENull (PNull ((1,1), "null"))
  
nextItem :: Type -> (Env, String) -> Item -> (Env, String)
nextItem typ (env, s) item = case item of
  NoInit (PIdent (pos, id)) -> 
    let (envAfter, code) =
          compileStmt 
          (dodajLokalna id typ env) 
          (Ass [FVar (PIdent (pos, id))] (neutralExpr typ))
    in (envAfter, s++code)
  Init (PIdent (pos, id)) expr -> 
    let (envAfter, code) = compileInit env (dodajLokalna id typ env) id expr
    in (envAfter, s++code)

compileInit :: Env -> Env -> String -> Expr -> (Env, String)
compileInit env0 env1 id expr =
  let (kod, envPo)=(compileExpr env0 expr) in
  (env1{wolnaEtykieta=(wolnaEtykieta envPo)}, 
      kod++
      "  mov %eax, "++(show (getLocalVarRel env1 id))++
      "(%ebp)   # "++id++"=eax\n")
  
forgetLocals :: Env -> Env -> Env
forgetLocals env0 envPo = 
  envPo{wolnaLokalna=(wolnaLokalna env0),lokalne=(lokalne env0)}

compileFields :: Env -> Expr -> (String, Env, Env)
compileFields env expr = case expr of  
  EFields fs -> case fs of  
    [FVar (PIdent (_,id))] -> 
      (if localVar env id then
         "  mov "++(show (getLocalVarRel env id))++
         "(%ebp), %eax   # eax="++id++"\n"
       else
         "  mov "++this++", %eax   # eax=this\n"++
         "  mov "++(show (getFieldVar env id))++
         "(%eax), %eax   # eax="++id++"\n", 
       env{klasa=(getClass id env),lokalne=Map.empty,funkcje=Map.empty}, env)
    [FFun (PIdent (_,id)) exprs] -> 
      let (kod, envPo)=(foldr nextExprPush ("", env) exprs) in
      (kod++
       (if isMethod id env then
          "  push "++this++"   # push this \n"++
          "  mov  "++this++", %eax   #eax=this\n"++
          "  mov  (%eax), %eax   # eax=tablicaMetod virtualnych odp. klasy\n"++
          "  c "++(show (4*(getMethodIndex (klasa env) id env)))++
          "(%eax)   # klasa:"++(klasa env)++" metoda:"++id++"\n"++
          "  add $"++(show (4*(length exprs+1)))++", %esp\n"
        else
          "  call "++id++"\n"++
          "  add $"++(show (4*(length exprs)))++", %esp\n"
       ),
       env{klasa=(getFunRetClass id env),lokalne=Map.empty,funkcje=Map.empty}, 
       envPo)
    fs1 -> -- fields dluzsze niz 1
      let (kod, prevEnv, envPo) = 
            compileFields env (EFields (take (length fs1-1) fs1)) in
       case last fs1 of
         FVar (PIdent (_,id)) -> 
           let envField = prevEnv{klasa=(getClass id prevEnv)} in
           (kod++
            "  mov "++(show (getFieldVar prevEnv id))++
            "(%eax), %eax   # eax=... ."++id++"\n", envField, envPo)
         FFun (PIdent (_,id)) exprs -> 
           let envField = prevEnv{klasa=(getFunRetClass id prevEnv)} in
           let (kod1, envPo)=
                 (foldr nextExprPush ("", zajmijTymczasowa env) exprs) in
           (
             (kod++
              "  mov %eax, "++(wolnaTymczasowa env)++
              "   #tmp"++(show (wolnaTymczasowa1 env))++"=current this \n"++
              kod1++
              "  push "++(wolnaTymczasowa env)++"   # current this \n"++
              "  mov "++(wolnaTymczasowa env)++", %eax   # eax=curr. this \n"++
              "  mov  (%eax), %eax   # eax=tablicaMetod virt. odp. klasy\n"++
              "  call "++(show(4*(getMethodIndex (klasa prevEnv) id prevEnv)))++
              "(%eax)   # klasa:"++(klasa prevEnv)++" metoda:"++id++"\n"++
              "  add $"++(show (4*(length exprs+1)))++", %esp\n"
             )
              ,envField, zwolnijTymczasowa envPo)
          
compileExprSameEnv :: Env -> Expr -> String
compileExprSameEnv env expr = case expr of
  ELitInt (PInteger (_,istr)) ->
    "  mov $"++(show ((read istr)::Int))++", %eax\n"
  ELitTrue _ ->
    "  mov $1, %eax   # eax=true\n"
  ELitFalse _ ->
    "  xor %eax, %eax   # eax=false\n"
  ENull _ ->
    "  xor %eax, %eax   # eax=null\n"
  EThis _ ->
    "  mov "++this++", %eax   # eax=this\n"
  EConstr _ (PIdent (_, id)) ->
    "  push $"++(show (4*getObjectSize id env))++"\n"++
    "  call malloc\n"++
    "  add 4, %esp\n"++
    "  mov $tablicaMetodKlasy$"++id++", (%eax)\n"++
    "  mov %eax, "++(wolnaTymczasowa env)++"\n"++
    "  push %eax\n"++
    "  call "++id++"$$init"++
    "  add 4, %esp\n"++
    "  mov "++(wolnaTymczasowa env)++", %eax\n"
  EString (PString ((w,k), str)) ->
    "  mov $String$"++(show w)++"$"++(show k)++", %eax\n"
      
nextExprPush :: Expr -> (String, Env) -> (String, Env)
nextExprPush expr (s, env) =
  let (src, envPo) = compileExpr env expr in 
  (s++src++"  push %eax\n", envPo)

compileExpr :: Env -> Expr -> (String, Env)
compileExpr env expr = case expr of
  EFields fs -> let (kod,_,envPo) = compileFields env expr in (kod, envPo)
  ECast _ e -> compileExpr env e
  DNeg e -> compileExpr env e
  Neg e -> let (kod, envPo)=compileExpr env e in (kod++"  neg %eax\n",envPo)
  Not e -> let (kod, envPo)=compileExpr env e in (kod++"  xor $1, %eax\n",envPo)
  EMul e1 op e2 ->
    let (kod, envPo)=(compileE2tmpE1 e1 e2 env) in
    (kod++
     (case op of
         Times ->
           "  imul "++(wolnaTymczasowa env)++", %eax   # eax=e1*e2\n"
         Div ->
           "  mov  %eax, %edx\n"++
           "  sar  $31, %edx\n"++
           "  idivl "++(wolnaTymczasowa env)++"   # eax=e1/e2\n"
         Mod ->
           "  mov  %eax, %edx\n"++
           "  sar  $31, %edx\n"++
           "  idivl "++(wolnaTymczasowa env)++"\n"++   
           "  mov %edx, %eax   # eax=e1%e2\n")
     , envPo)
  EAdd e1 Plus e2 -> case getExprType env e1 of
    Str -> 
      let (kod, envPo)=(compileE2tmpE1 e1 e2 env) in
      (kod++
       "  push "++(wolnaTymczasowa env)++"\n"++
       "  push %eax\n"++
       "  call __concat   # eax=e1+e2 (Str)\n"++
       "  add $8, %esp\n"
       , envPo)
    Int ->
      let (kod, envPo)=(compileE2tmpE1 e1 e2 env) in
      (kod++
      "  add "++(wolnaTymczasowa env)++", %eax   # eax=e1+e2\n"
      , envPo)
  EAdd e1 Minus e2 ->
    let (kod, envPo)=(compileE2tmpE1 e1 e2 env) in
    (kod++
     "  sub "++(wolnaTymczasowa env)++", %eax   # eax=e1-e2\n"
     , envPo)
  ERel e1 op e2 ->    
    let (kod, envPo)=(compileE2tmpE1 e1 e2 env) in
    (kod++
     "  cmp "++(wolnaTymczasowa env)++", %eax   # e2, e1\n"++
     "  "++(relCondSet op)++" %al\n"++
     "  and $1, %eax   # if e1 relOp e2 then eax=1 else eax=0\n"
     , envPo)
  EAnd e1 e2 -> (
    let (kod1, envPoE1)=compileExpr (zajmijEtykiete env) e1 in
    let (kod2, envPo)=compileExpr envPoE1 e2 in
    (kod1++
     "  cmp $1, %eax\n"++ 
     "  je AndFirstTrue"++(show (wolnaEtykieta env))++"\n"++
     "  jmp AndFalse"++(show (wolnaEtykieta env))++"\n"++
     "AndFirstTrue"++(show (wolnaEtykieta env))++":\n"++
     kod2++
     "  jmp AndEnd"++(show (wolnaEtykieta env))++"\n"++
     "AndFalse"++(show (wolnaEtykieta env))++":\n"++
     "  xor %eax,%eax\n"++
     "AndEnd"++(show (wolnaEtykieta env))++":\n",
     envPo))
  EOr e1 e2 -> (
    let (kod1, envPoE1)=compileExpr (zajmijEtykiete env) e1 in
    let (kod2, envPo)=compileExpr envPoE1 e2 in
    (kod1++
     "  cmp $0, %eax\n"++
     "  je OrFirstFalse"++(show (wolnaEtykieta env))++"\n"++
     "  jmp OrTrue"++(show (wolnaEtykieta env))++"\n"++
     "OrFirstFalse"++(show (wolnaEtykieta env))++":\n"++
     kod2++
     "  jmp OrEnd"++(show (wolnaEtykieta env))++"\n"++
     "OrTrue"++(show (wolnaEtykieta env))++":\n"++
     "  mov $1, %eax\n"++
     "OrEnd"++(show (wolnaEtykieta env))++":\n",
     envPo))
  _ -> (compileExprSameEnv env expr, env)

compileE2tmpE1 :: Expr -> Expr -> Env -> (String, Env)
compileE2tmpE1 e1 e2 env =
  let (kod2, envPo) = (compileExpr env e2) in
  let (kod1, envPo2) = (compileExpr (zajmijTymczasowa envPo) e1) in
  (kod2++
   "  mov %eax, "++(wolnaTymczasowa env)++
   "   # tmp"++(show (wolnaTymczasowa1 env))++"=eax\n"++
   kod1,
   zwolnijTymczasowa envPo2)

calcExprUsedTemps :: Expr -> Int
calcExprUsedTemps expr = case expr of
  ELitInt _ -> 0
  ELitTrue _ -> 0
  ELitFalse _ -> 0
  ENull _ -> 0
  EThis _ -> 0
  EConstr _ _ -> 1
  EString _ -> 0
  EFields fs -> calcFieldsUsedTemps expr 
  ECast _ e -> calcExprUsedTemps e
  DNeg e -> calcExprUsedTemps e
  Neg e -> calcExprUsedTemps e
  Not e -> calcExprUsedTemps e
  EMul e1 _ e2 -> max (calcExprUsedTemps e2) (1+(calcExprUsedTemps e1))
  EAdd e1 _ e2 -> max (calcExprUsedTemps e2) (1+(calcExprUsedTemps e1))
  ERel e1 _ e2 -> max (calcExprUsedTemps e2) (1+(calcExprUsedTemps e1))
  EAnd e1 e2 -> max (calcExprUsedTemps e2) (calcExprUsedTemps e1)
  EOr e1 e2 -> max (calcExprUsedTemps e2) (calcExprUsedTemps e1)
  
calcFieldsUsedTemps :: Expr -> Int
calcFieldsUsedTemps expr = case expr of
  EFields fs -> case fs of  
    [FVar _] -> 0  
    [FFun _ exprs] -> maximumInt $ map calcExprUsedTemps exprs
    fs1 ->
      let maxfs1bezOst = 
            calcFieldsUsedTemps (EFields (take (length fs1-1) fs1)) in
      case last fs1 of
         FVar _ -> maxfs1bezOst
         FFun _ exprs -> 
           let lastTemps = 1+(maximumInt $ map calcExprUsedTemps exprs)
           in max maxfs1bezOst lastTemps
  
maximumInt :: [Int] -> Int
maximumInt [] = 0
maximumInt a = maximum a

{- Błędne jest: użycie env do ustalenia wolnej etykiety i jednoczesne
 kompilowanie zagnieżdżonych wyrażeń z tym samym środowiskiem
 Bo wyrażenie zagnieżdżone (lub równoległe) zawsze może potrzebować etykiety - 
 leniwe wyliczanie wyrażeń && i ||.
 Już poprawnie.
 Tak samo zapewne compileE2TmpE1 jest błędne 
 - za to wolnaTymczasowa może być użyta do obliczenia wcześniejszego
 wyrażenia. Patrz: compileExpr e1a expr6 -}

relCond :: RelOp -> String
relCond op = case op of
  LTH -> "l"
  LE  -> "le"
  GTH -> "g"
  GE -> "ge"
  EQU -> "e"
  NE  -> "ne"
  
relCondSet :: RelOp -> String
relCondSet op = "set"++(relCond op)

-- compileExpr musi zmieniać env? Niekoniecznie.
-- W celu aktualizacji rezerwacji zmiennych tymczasowych?
-- Może to robić online.
  
getExprType :: Env -> Expr -> Type
getExprType env expr = case expr of
  EFields fs -> case fs of
    [FVar (PIdent (_,id))] -> 
      if localVar env id then getLocalVarType env id 
      else getFieldVarType env id
    [FFun (PIdent (_,"readInt")) _] -> Int
    [FFun (PIdent (_,"readString")) _] -> Str
    [FFun (PIdent (_,"printInt")) _] -> Void
    [FFun (PIdent (_,"printString")) _] -> Void
    [FFun (PIdent (_,"error")) _] -> Void
    [FFun (PIdent (_,id)) _] -> 
      if globalFun id env then getFunReturnType env id
      else getMethodReturnType (klasa env) id env
  ELitInt _ -> Int
  ELitTrue _ -> Bool
  ELitFalse _ -> Bool
  ENull _ -> Void
  EThis _ -> Class (PIdent ((-1,-1), (klasa env)))
  EConstr _ (PIdent (_, id)) -> Class (PIdent ((-1,-1), id))
  ECast (PIdent (_, id)) _ -> Class (PIdent ((-1,-1), id))
  EString _ -> Str
  DNeg e -> Int
  Neg e -> Int
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
    

expr1 = 
  EAdd (EFields [FVar (PIdent ((-1,-1),"c"))]) Minus
  (EFields [FVar (PIdent ((-1,-1),"d"))])
expr2 = 
  EMul (EFields [FVar (PIdent ((-1,-1),"c"))]) Times
  (EFields [FVar (PIdent ((-1,-1),"d"))])
expr3 = 
  EMul (EFields [FVar (PIdent ((-1,-1),"c"))]) Div
  (EFields [FVar (PIdent ((-1,-1),"d"))])
expr4 = 
  EMul (EFields [FVar (PIdent ((-1,-1),"c"))]) Mod
  (EFields [FVar (PIdent ((-1,-1),"d"))])
expr5 = EMul expr1 Times (ELitInt (PInteger ((-1,-1), "11")))

expr6 = EMul expr1 Times expr1
  
exprc = (EFields [FVar (PIdent ((-1,-1),"c"))]) 
exprd = (EFields [FVar (PIdent ((-1,-1),"d"))])

fsc = [FVar (PIdent ((-1,-1),"c"))]

expr7 = ERel exprc GE exprd 
expr8 = EAnd exprc exprd
expr8c = EAnd exprc exprc
expr8d = EAnd exprd exprd
expr9 = EAnd exprc expr8

expr10 = EFields [FFun (PIdent ((-1,-1), "c")) [expr8c, expr8d]]

e1 = dodajLokalna "c" Int emptyEnv
e1a = (dodajLokalna "d" Int e1){lokalnych = 2}
e1b = dodajFunkcje "c" (Fun Void [Bool, Bool]) e1a

e2 = ustalKlase "k" emptyEnv
e3 = dodajKlase "k" e2
e4 = dodajPoleDoKlasy "c" (Class (PIdent ((1,1), "k"))) "k" e3
e5 = dodajPoleDoKlasy "d" (Class (PIdent ((1,1), "k"))) "k" e4
e6 = ustalLokalnych 2 e5
e7 = dodajPoleDoKlasy "e" (Class (PIdent ((1,1), "k"))) "k" e5

e8 = dodajMetodeDoKlasy "c" ((Fun (Class (PIdent ((1,1), "k"))) [Int])) "k" e7
e9 = ustalTymczasowych 1 e8
e10 = e9{lokalnych=2}

f8 = dodajMetodeDoKlasy "c" ((Fun (Class (PIdent ((1,1), "k2"))) [Int])) "k" e7
f9 = dodajFunkcje "c" ((Fun (Class (PIdent ((1,1), "k"))) [Int])) f8
f10 = dodajKlase "k2" (ustalKlase "" f9{lokalnych=2,tymczasowych=2})
f11 = dodajPoleDoKlasy "g" (Class (PIdent ((1,1), "k2"))) "k2" f10
f12 = dodajPoleDoKlasy "h" (Class (PIdent ((1,1), "k2"))) "k2" f11
f13 = dodajMetodeDoKlasy "c" ((Fun (Class (PIdent ((1,1), "k"))) [Int])) "k2" f12
f14 = dodajPoleDoKlasy "i" (Class (PIdent ((1,1), "k2"))) "k2" f13
f15 = dodajLokalna "c" Int (dodajLokalna "d" Int f14) 
f16 = f15{lokalnych=4}
{-
f14:

k c(int i){...};

class k{
  k c,d,e;
  k2 c(int i){...};
}

class k2{
  k g,h,i;
  k c(int i){...};
}

funkcja(...){
  int a,b;

expr14:
  c(1).c(1).g.h.c(1).c.c(1).g.i

f16
funkcja(...){
  int a,b,c,d;

-}
expr11 = 
  EFields [FVar (PIdent ((1,1), "c")), 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e")),
           FVar (PIdent ((1,1), "c")), 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e"))]

expr12 = 
  EFields [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e")),
           FVar (PIdent ((1,1), "c")), 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e"))]

expr13 = 
  EFields [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e")),
           FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FVar (PIdent ((1,1), "c")), 
           FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FVar (PIdent ((1,1), "d")),
           FVar (PIdent ((1,1), "e"))]

expr14 = EFields fs14
  
fs14 = [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
        FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
        FVar (PIdent ((1,1), "g")),
        FVar (PIdent ((1,1), "h")),
        FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
        FVar (PIdent ((1,1), "c")), 
        FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
        FVar (PIdent ((1,1), "g")),
        FVar (PIdent ((1,1), "i"))]

expr15 =   
  EFields [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))]]

expr16 = 
  EFields [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
           FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))]] 


expr17 = EFields fs17
fs17 =  
  [FFun (PIdent ((1,1), "c")) [(ELitInt (PInteger ((1,1), "1")))], 
   FFun (PIdent ((1,1), "c")) [expr5], 
   FVar (PIdent ((1,1), "g"))]

  
expr18 = EFields fs18
fs18 = [FVar (PIdent ((1,1), "a"))]
expr19 = EFields fs19
fs19 = [FVar (PIdent ((1,1), "b"))]

expr20 = EAdd expr18 Plus expr19

stmt1 = Decl Int [(NoInit (PIdent ((1,1), "a"))), 
                  (NoInit (PIdent ((1,1), "b")))]
stmt2 = 
  BStmt (Block [Decl Int [(NoInit (PIdent ((1,1), "a")))],
                Decl Int [(NoInit (PIdent ((1,1), "b")))],
                Ass fs14 expr16])
stmt3 = Decl Int [(Init (PIdent ((1,1), "a"))) expr14]

stmt4 =
  BStmt (Block [Decl Int [(NoInit (PIdent ((1,1), "a")))],
                Decl Int [(NoInit (PIdent ((1,1), "b")))],
                Ass fs19 expr20,
                Cond expr19 stmt6,
                CondElse expr18 stmt5 stmt6,
                While expr19 stmt6,
                Incr fs17
               ])

stmt5 = Incr fs18
stmt6 = Decr fs19
  
stmt7 =  
  BStmt (Block [Decl Int [(NoInit (PIdent ((1,1), "a")))],
                Decl Int [(NoInit (PIdent ((1,1), "b")))],
                Ass fs17 expr6])

stmt8 = Ass fsc (neutralExpr Int)
stmt9 = Decl Int [(Init (PIdent ((1,1), "a"))) expr6]

stmt10 = 
  BStmt (Block [Decl Int [(NoInit (PIdent ((1,1), "a")))],
                Decl Int [(NoInit (PIdent ((1,1), "b")))],
                stmt7,
                Decl Int [(NoInit (PIdent ((1,1), "c")))],
                Decl Int [(NoInit (PIdent ((1,1), "d")))],
                BStmt (Block [stmt9])
                ])


b (_,b,_) = b
a (a,_,_) = a
c (_,_,c) = c