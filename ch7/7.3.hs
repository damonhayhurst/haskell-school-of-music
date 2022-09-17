import Euterpea

class Temporal a where
    durT :: a -> Dur
    cutT :: Dur -> a -> a
    removeT :: Dur -> a -> a

instance Temporal (Music a) where
    durT a = dur a
    cutT d a = cut d a
    removeT d a = remove d a

instance Temporal (Primitive a) where
    durT (Note d p) = d
    durT (Rest d) = d
    cutT d (Note nd p) = Note (nd - d) p
    cutT d (Rest rd) = Rest (rd - d)
    removeT d (Note nd p) = Note (nd - d) p
    removeT d (Rest rd) = Rest (rd - d)