data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where               
  map func Empty = Empty
  map func (Node x y z) = Node (map func x) (func y) (map func z)

--- fold example
totalLen : List String -> Nat
totalLen xs = foldr (\str, accumulator => accumulator + length str) Z xs

Foldable Tree where
  foldr func init Empty = init
  -- foldr starts by folding the left subtree recursively, so init gets passed only to lTree foldr. That value is the fed to rtTree.
  foldr func init (Node lTree valu rTree) = func valu (foldr func (foldr func init lTree) rTree)
  foldl func init Empty = init
  foldl func init (Node lTree valu rTree) = func (foldl func (foldl func init rTree) lTree) valu
