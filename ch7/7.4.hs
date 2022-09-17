instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
    f == f' = fmap f allArgs == map f' allArgs
        where allArgs = [minBound..maxBound] 