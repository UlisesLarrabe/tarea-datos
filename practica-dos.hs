sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = (n + 1) : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = if (x == True)
                      then conjuncion xs
                      else False

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = if (x == True)
                      then True
                      else conjuncion xs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> a -> [a] -> Bool
pertenece _ [] = False
pertenece e (n:ns) = if (e == n)
                      then True
                      else pertenece e ns

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (n:ns) = if (e == n)
                        then 1 + apariciones e ns
                        else 0 + apariciones e ns

losMenoresA :: Int -> [[a]] -> [[a]]
losMenoresA _ [] = []
losMenoresA e (n:ns) = if ( n > e)
                        then n : losMenoresA e ns
                        else losMenoresA e ns

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA e [] = []
lasDeLongitudMayorA e (n:ns) = if (longitud n > e)
                                then [n] ++ lasDeLongitudMayorA e ns
                                else lasDeLongitudMayorA e ns

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal x s = x ++ [s]

agregar :: [a] -> [a] -> [a]
agregar x s = x ++ s

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]