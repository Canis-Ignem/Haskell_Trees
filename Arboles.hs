--Dado el siguiente tipo de �rboles binarios: 
data ArBin a = Vac | Nodo (ArBin a) a (ArBin a) deriving Eq 

--Ejemplos:
a1 = Nodo (Nodo (Nodo Vac 2 Vac)
                3 
                (Nodo Vac 5 Vac)) 
          6 
          (Nodo (Nodo Vac 7 Vac) 
                8 
                (Nodo Vac 9 Vac))              
a2 = Nodo (Nodo (Nodo Vac 17 Vac) 
                 18 
                (Nodo Vac 19 Vac) ) 
           27 
          (Nodo (Nodo Vac 37 Vac) 
                100 
                (Nodo (Nodo Vac 110 Vac)
                       115 
                      (Nodo Vac 1000 Vac)))   
a3 = Nodo (Nodo (Nodo a1 10 Vac) 
                11 
                (Nodo Vac 12 Vac)) 
           14 
          (Nodo (Nodo Vac 15 Vac) 
                16 
                a2)              


instance (Show a) => Show (ArBin a) where
         show Vac = ""
         show t = show' t 0 (maxLong t + 1)

-- Definir 
show' :: (Show a) => ArBin a -> Int -> Int -> String
-- (show' t c a)  muestra el arbol t empezando en la columna c 
-- y usando a caracteres para cada nodo.
show' Vac _ _ = ""
show' (Nodo ai r ad) desde_col long_nodo 
     = dibujo_ai ++ "\n" ++ dibujo_raiz ++ dibujo_ad
       where dibujo_raiz = [' '| i<-[1..desde_col]] ++ show r
             dibujo_ai = show' ai (desde_col+long_nodo) long_nodo
             dibujo_ad = show' ad (desde_col+long_nodo) long_nodo
             
maxLong :: (Show a) => ArBin a -> Int
-- La max longitud (n�mero de car�cteres) de un 
-- nodo (como string) en el arbol dato
--maxLong Vac = 0
--maxLong (Nodo ai r ad) = maximum [(length.show) r,maxLong ai,maxLong ad]
----------------------------------------------------------------------
-- Definir una funci�n foldArBin 
foldArBin :: (a -> b -> b -> b) -> b -> ArBin a -> b
foldArBin f e Vac = e
foldArBin f e (Nodo ai r ad) 
    = f r (foldArBin f e ai) (foldArBin f e ad)

-- Redefinir, usando foldArBin, la funci�n maxLong de arriba 
maxLong = foldArBin (\r mi md -> maximum [long r,mi,md]) 0 
          where long = length.show

-- Definir la funci�n numVerif que, dados un predicado (sobre  
-- los elementos del �rbol) y un �rbol, devuelve el n�mero de
-- nodos que verifican el predicado. 
-- Esta funci�n debe definirse en t�rminos de foldArBin.

numVerif :: (a -> Bool) -> ArBin a -> Int

{-
numVerif p = foldArBin f 0
             where f r nvi nvd =  (fromEnum (p r)) + nvi + nvd
                --      | p r = 1 + nvi + nvd
                --      | otherwise = nvi + nvd
-}
numVerif p = foldArBin (\r vi vd -> (fromEnum (p r)) + vi + vd) 0
     
-- EJERCICIO PARA CASA
-- Sea el tipo 
type ArPares a = ArBin (a,a)
-- type ArTriples a b c = ArBin (a,b,c)
-- Definir una funci�n que, dado un �rbol del tipo ArPares a 
-- y empleando la funci�n del ejercicio numVerif determine 
-- el n�mero de nodos (x,y) tales que x es menor que y.

--------------------------------------------------------------------
-- Supongamos el siguiente tipo de �rboles binarios con informaci�n  
-- de dos tipos en nodos internos y hojas: 

data Arbol a b  =  Hoja a | NodoT  (Arbol a b) b (Arbol a b)
                   deriving Show
-- definimos el tipo de las expresiones aritm�ticas 
-- en t�rminos de este tipo de �rboles
type ExpArit = Arbol Integer String
-- Ejemplos 
exp1 :: ExpArit
exp1 = Hoja 9
exp2 = NodoT (Hoja 3) "*" (Hoja 5)
exp3 = NodoT exp1 "-" (NodoT (Hoja 10) "+" (Hoja 6))
exp4 = NodoT exp3 "+" exp2
--exp4  representa a la expresi�n (9 - (10+6)) + (3*5)             
-- Definir una funci�n 
foldArbol :: (t1 -> t2) -> (t3 -> t2 -> t2 -> t2) -> Arbol t1 t3 -> t2
foldArbol f g (Hoja a) = f a
foldArbol f g (NodoT ai r ad) 
                     = g r (foldArbol f g ai) (foldArbol f g ad)

-- Definir, en funci�n de foldArbol, funciones que permitan:
-- (a) calcular el n�mero de operadores de una expresi�n 
--    (es lo mismo que el n�mero de nodos internos)

--numOpers :: Arbol t1 t3 -> Integer
numOpers:: ExpArit -> Integer
numOpers = foldArbol (\x->0) (\_ ni nd-> 1+ni+nd)

-- (b) evaluar una expresi�n
evaluar:: ExpArit -> Integer
-- id = (\x->x)
evaluar = foldArbol id (\s ei ed-> case s of 
                                        "+" -> ei + ed             
                                        "-" -> ei - ed
                                        "*" -> ei * ed
                                        otherwise -> error "operador desconocido")
                                        
--------- ARBOLES N-�RIOS
data ArbN a = NodoN a [ArbN a]
--Ejemplo:
arb1 = NodoN 1 [NodoN 2 [],
                NodoN 3 [NodoN 4 [],
                         NodoN 5 [NodoN 6 [NodoN 7 []]]],
                NodoN 8 [NodoN 9 [NodoN 10 []]]]
-- funcion de plegado
foldt :: (a -> [b] -> b) -> ArbN a -> b
foldt f (NodoN r ts) = f r (map (foldt f) ts)  

--(a) Funci�n que calcula el n�mero de nodos
numNodos :: ArbN a -> Integer
numNodos = foldt (\_ zs -> 1 + sum zs)
                --(\_ zs -> 1 + (foldl (+) 0 zs))   
                
--(b) Funci�n que devuelve la rama m�s larga como una lista
ramaMasLarga :: ArbN a -> [a]
ramaMasLarga = foldt consToMasLarga
               where
               consToMasLarga r [] = [r]
               consToMasLarga r zs = r : masLarga zs
               masLarga = foldr1 (\xs ys -> if length xs > length ys 
                                            then xs else ys)
 
--EJERCICIOS PARA CASA
--(c) Funci�n que calcula la profundidad
--(d) Funci�n que devuelve la lista de todos los nodos en preorden
