-- inak to je church's one
apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i

-- CHURCH'S NUMERAL  
-- ?FX.X
ZERO  = K I  
-- ?FX.(F X)
-- ONE  
ONE = \F -> \X -> F X  
ONE''   = I  
--ONE''' = (S (S K))
ONE'  = (S ( (S (K S)) ((S (K K) I)) )) (K I)
TWO   = \F -> \X -> F (F X)
TWO'   = (S (S (K S) K)) I  
THREE = (S (S (K S) K)) (S (S (K S) K) I)  
FOUR  = (S (S (K S) K)) ((S (S (K S) K)) (S (S (K S) K) I))  

incr = \n -> \f -> \x -> f (n f x)     
incr' n = \f -> \x -> f (n f x)     

add = \m -> \n -> \f -> \x -> m f (n f x)     
add' m n = \f -> \x -> m f (n f x)     
  
S (S K) a b =   ((S K) a) (a b) = K (a b) = (a b)
  
--zero
\f.\x.x = \f.I = K I

-- one
\f.\x.(f x) =\f.((S (\x.f)) (\x.x)) = \f.((S (K f)) I) = (S (\f.(S (K f)))) (\f.I) = S (S ((\f.S) (\f.(K f)))) (K I)
 = S (S ((\f.S) (\f.(K f)))) (K I)
 S (S ((K S) (\f.(K f)))) (K I)
 S (S ((K S) (S(\f.K \f.f)))) (K I)
 S (S ((K S) (S((K K) I)))) (K I)
 (S ( (S (K S)) ((S (K K) I)) )) (K I)
-- incr
\n.\f.\x.(f ((n f) x)
...
((S (K (S ((S (K S)) ((S (K K)) I))))) ((S ((S (K S)) ((S (K (S (K S)))) ((S (K (S (K K)))) ((S ((S (K S)) ((S (K K)) I))) (K I)))))) (K (K I))))

(incr zero)

(incr one)

(incr two)
((S (K (S ((S (K S)) ((S (K K)) I))))) ((S ((S (K S)) ((S (K (S (K S)))) ((S (K (S (K K)))) ((S ((S (K S)) ((S (K K)) I))) (K I)))))) (K (K I)))) ((S (S (K S) K)) I)
