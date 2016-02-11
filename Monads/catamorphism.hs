type Algebra f a = f a -> a 
newtype Mu f = InF { outF :: f (Mu f) } 

cata :: Functor f => Algebra f a -> Mu f -> a 
cata f = f . fmap (cata f) . outF

---------------------------------------------------

data StrF x = Cons Char x | Nil 
type Str = Mu StrF

instance Functor StrF where     
    fmap f (Cons a as) = Cons a (f as)     
    fmap f Nil = Nil
    
clength :: Str -> Int 
clength = cata phi where     
    phi (Cons a b) = 1 + b     
    phi Nil = 0
    
sam= InF { outF = Cons 'S'
    (InF { outF = Cons 'a'
    (InF { outF = Cons 'm'
    (InF { outF = Nil })})})}
    
------------------------------------------------------

data TreeF x = Node Int [x]
type Tree = Mu TreeF

instance Functor TreeF where
  fmap f (Node e xs) = Node e (fmap f xs)

cdepth :: Tree -> Int
cdepth = cata phi where
  phi :: Algebra TreeF Int
  phi (Node x sons) = 1 + foldr max 0 sons

samTree = InF { outF = Node 4 [ 
          InF { outF = Node 1 [
          InF { outF = Node 3 [
          InF { outF = Node 2 [] }]}]}]}
