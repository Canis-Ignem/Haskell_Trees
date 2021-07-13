--- BINARY SEARCH TREES
data BST a = EmptyBST | NodeBST (BST a) a (BST a) 
             deriving Eq

---------------------------------------------------
instance (Show a) => Show (BST a) where
         show EmptyBST = ""
         show t   = show' t 0 (maxLong t + 1)
                    where 
                    maxLong :: (Show a) => BST a -> Int
                    maxLong EmptyBST = 0
                    maxLong (NodeBST ai r ad) 
                         = maximum [maxLong ai,length(show r),maxLong ad]
                    show' :: (Show a) => BST a -> Int -> Int -> String
                    show' EmptyBST _ _ = " "
                    show' (NodeBST ai r ad) desde_col long_nodo
                      = dibujo_ai ++ "\n" ++ dibujo_raiz ++ dibujo_ad
                        where 
                        dibujo_raiz = [' '|i<-[1..desde_col]] ++ show r
                        dibujo_ai  = show' ai (desde_col + long_nodo) long_nodo
                        dibujo_ad = show' ad (desde_col + long_nodo) long_nodo

-----------------------------------------------------------------

createTree :: [a] -> BST a
createTree [] = EmptyBST
createTree xs = NodeBST (createTree i) r (createTree d)
                where n = length xs
                      (i,r:d) = splitAt (div n 2) xs
                      --(i,d') = splitAt (div n 2) xs
                      --r:d = d'
                      
bst1 = createTree [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
bst2 = createTree [1,3,5,7,9,11,13,15,17,19]
nobst = createTree [5,6,3,4,9,8,1,2,3,5,4,7,9]

-------------------------------------------------
-- Tratamos BSTs sin nodos repetidos.

isEmptyBST :: Eq a => BST a -> Bool
--isEmptyBST EmptyBST = True
--isEmptyBST _ = False
isEmptyBST t = (t == EmptyBST) --usando Eq

isBST :: Ord a => BST a -> Bool
isBST EmptyBST = True
isBST (NodeBST ai r ad) 
     = isBST ai && isBST ad &&
       (isEmptyBST ai || nodoMasDcha ai < r ) &&
       (isEmptyBST ad || nodoMasIzda ad > r )
       where 
       nodoMasDcha (NodeBST bi r' bd) 
          = if isEmptyBST bd then r' else nodoMasDcha bd
       nodoMasIzda (NodeBST bi r' bd) 
          = if isEmptyBST bi then r' else nodoMasDcha bi
                                    
searchBST :: Ord a => a -> BST a -> Bool
searchBST v EmptyBST = False
searchBST v (NodeBST ai r ad)
      | v == r = True
      | v < r = searchBST v ai
      | v > r = searchBST v ad

insertBST :: Ord a => a -> BST a -> BST a
insertBST v EmptyBST = (NodeBST EmptyBST v EmptyBST)
insertBST v (NodeBST ai r ad)
      | v == r = NodeBST ai r ad
      | v < r = NodeBST (insertBST v ai) r ad
      | v > r = NodeBST ai r (insertBST v ad)

deleteBST :: Ord a => a -> BST a -> BST a
deleteBST v EmptyBST = EmptyBST
deleteBST v (NodeBST ai r ad)
      | v < r = NodeBST (deleteBST v ai) r ad
      | v > r = NodeBST ai r (deleteBST v ad)  
      | v == r = if isEmptyBST ai then ad else NodeBST ai' r' ad
                 where     -- (NodeBST ai v ad) | ai no es vacio
                 (r',ai') = deleteMax ai
                 deleteMax :: Ord a => BST a -> (a, BST a)
                 --(deleteMax arbol) = (m,arbol')
                 -- m es el máximo nodo  de arbol 
                 -- arbol' es el resultado de quitar m a arbol
                 deleteMax (NodeBST bi r EmptyBST) = (r, bi)
                 deleteMax (NodeBST bi r bd) 
                    = let (r',bd') = deleteMax bd 
                      in (r', NodeBST bi r bd')
