data Color = Red
    | Blue
    | Green

instance Eq Color where
    Red == Red = True
    Blue == Blue = True
    Green == Green = True
    _ == _ = False

instance Ord Color where
    Blue <= Red = False
    Green <= Blue = False
    _ <= _ = True

instance Enum Color where
    fromEnum Red = 0
    fromEnum Blue = 1
    fromEnum Green = 2

    toEnum 0 = Red
    toEnum 1 = Blue
    toEnum 2 = Green
