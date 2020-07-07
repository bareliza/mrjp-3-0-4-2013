import qualified Data.Map as Map

a :: Map.Map Int Int
a = Map.fromList (zip [0..1000] [x*x | x<-[0..1000]])

type SEnv = Map.Map Int Int
type DEnv = Int

type Env = (SEnv, DEnv)

unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing = error "unjust dla Nothing"

step :: Int -> Env -> Env
step liczba2 (map, l1m) =
  let l2m = mod liczba2 1000 in
  (Map.insert l2m l2m map, 
   mod ((unjust (Map.lookup l1m map)) + (unjust (Map.lookup l2m map))) 1000 )
  
obliczenia :: Int -> Int -> Int
obliczenia i j = snd (foldr step (a, i) [1..j]) 

step2 liczba2 l1m =
  let l2m = mod liczba2 1000 in
   mod ((unjust (Map.lookup l1m a)) + (unjust (Map.lookup l2m a))) 1000

obliczenia2 :: Int -> Int -> Int
obliczenia2 i j = foldr step2 i [1..j]