module Block2 where

------------------------------ TASK 1 ------------------------------

stringSum :: String -> Maybe Int
stringSum s = saveSum (map read (words s))
  where
    saveSum = undefined
