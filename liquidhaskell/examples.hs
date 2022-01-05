import Prelude hiding (gcd, mod)

{- 
    Maximun function

    El primer ejemplo del paper, pero en Haskell
-}

{-@ max :: x:Int -> y:Int -> {v:Int | v >= x && v >= y} @-}

max :: Int -> Int -> Int
max x y 
    | x > y = x
    | otherwise = y


{- 
    Average
-}

{-@ type NonEmpty a = {v:[a] | 0 < len v } @-}
{-@ avg :: NonEmpty Int -> Int @-}

avg :: [Int] -> Int
avg xs = div total n
    where
        total = sum xs
        n = length xs


{- 
    Greatest common divisor

    Da problemas si se especifica gcd con el segundo parámetro solo como Nat.
    La especificación en ese caso no se verifica aún añadiendo el caso a<b a parte.
-}

{-@ mod :: a:Nat -> b:{v:Nat| 0 < v} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int

mod a b
    | a < b = a
    | otherwise = mod (a - b) b


{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Nat @-}

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)


{- 
    Filter natural numbers
    No lo verifica correctamente :( ni con este filter' ni con el filter de Haskell. 
    -> Meter uninterpreted function en el output de filter' ??


{-@ filter' :: f:(a -> Bool)-> l:[a] -> [a] @-}
filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs



{-@ onlyNat :: [Int] -> [Nat] @-}
onlyNat :: [Int] -> [Int]
onlyNat = filter' (>=0)

-}

{- 
    Filter natural numbers '
-}

{-@ onlyNat' :: [Int] -> [Nat] @-}
onlyNat' :: [Int] -> [Int]
onlyNat' [] = []
onlyNat' (x:xs)
    | x>=0      = x : onlyNat' xs
    | otherwise = onlyNat' xs

