power :: (a -> a) -> Int -> a -> a

power f n a = if n==0 then a else f(power f (n-1) a)