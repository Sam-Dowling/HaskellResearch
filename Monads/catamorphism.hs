type Algebra f a = f a -> a 
newtype Mu f = InF { outF :: f (Mu f) } 
cata :: Functor f => Algebra f a -> Mu f -> a 
cata f = f . fmap (cata f) . outF

data StrF x = Cons Char x | Nil 
type Str = Mu StrF
instance Functor StrF where     
    fmap f (Cons a as) = Cons a (f as)     
    fmap f Nil = Nil
    
cata_length :: Str -> Int 
cata_length = cata phi where     
    phi (Cons a b) = 1 + b     
    phi Nil = 0
    
foo=InF { outF = Cons 'f'
        (InF { outF = Cons 'o'
            (InF { outF = Cons 'o'
                (InF { outF = Nil })})})}
    
                
example = cata_length foo
