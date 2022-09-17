fix :: (a -> a) -> a
fix f = f(fix f)

remainder :: Integer -> Integer -> Integer
remainder a b = fix (\f n -> if n < b then n else n - b) a