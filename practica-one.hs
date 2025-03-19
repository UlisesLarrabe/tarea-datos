--2 Numeros enteros
--a
sucesor :: Int -> Int
sucesor x = x + 1

--b
sumar :: Int -> Int -> Int
sumar a b = a + b

--c
divisionYResto :: Int -> Int -> (Int, Int)
--PRECOND: Y no es igual a 0
divisionYResto x y = (div x y, mod x y)

--d
maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = if (x > y)
                    then x
                    else y

{- 2.
  Ejemplo 1: 
    sumar (maxDelPar (divisionYResto 100 10)) (sucesor (-1))
  
  Ejemplo 2:
    sucesor (maxDelPar (divisionYResto (sumar 80 1) 9))

  Ejemplo 3:
    maxDelPar (divisionYResto (sumar 9 1) (sucesor 0))

  Ejemplo 4:
    sumar 9 (maxDelPar (divisionYResto 1 (sucesor 0)))
-}

--3 Tipos enumerativos

--a
data Dir = Norte | Sur | Este | Oeste
  deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

--b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales a b = False

--c
siguiente :: Dir -> Dir
--PRECOND: x no puede ser Oeste
siguiente x = case x of
                Norte -> Este
                Este -> Sur
                Sur -> Oeste

--Es una funcion parcial, pues se toma en cuenta solo 3 casos de los 4 tipos de direcciones que hay

--2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
  deriving Show

--a
primeroYUltimo :: (DiaDeSemana, DiaDeSemana)
primeroYUltimo = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False 

--c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False

--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _      = True