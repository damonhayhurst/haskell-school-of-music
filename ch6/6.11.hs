import Euterpea

insideOut :: Music a -> Music a
insideOut = mFold Prim (:=:) (:+:) Modify
