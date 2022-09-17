import Euterpea

allPerc :: Music Pitch
allPerc = instrument Percussion $ line $ map (\p -> note qn (pitch p)) [35..86]