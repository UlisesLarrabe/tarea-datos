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

--3

--a
negar :: Bool -> Bool
negar True = False
negar False = True

--b
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--c
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

--d
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

--4. REGISTROS
data Persona = P String Int
                --Nombre Edad
                deriving Show


nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n1 e1) (P n2 e2) = if (e1 > e2)
                                        then True
                                        else False

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if (e1 > e2)
                                        then (P n1 e1)
                                        else (P n2 e2)

--2
data TipoDePokemon = Agua | Fuego | Planta
              deriving Show
data Pokemon = Pk TipoDePokemon Int
                            --Porcentaje de energia
              deriving Show
data Entrenador = E String Pokemon Pokemon 
                --Nombre
                deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pk tipo1 _) (Pk tipo2 _) = if (tipoEsSuperior tipo1 tipo2)
                                    then True
                                    else False

tipoEsSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
tipoEsSuperior  Agua Fuego = True
tipoEsSuperior  Fuego Planta = True
tipoEsSuperior Planta Agua = True
tipoEsSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E n pk1 pk2) = (esDelTipo pk1 tipo) + (esDelTipo pk2 tipo)

esDelTipo :: Pokemon -> TipoDePokemon -> Int
esDelTipo (Pk tipoPk p) tipo = if (sonIguales tipoPk tipo)
                                  then 1 
                                  else 0

sonIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonIguales Agua Agua = True
sonIguales Fuego Fuego = True
sonIguales Planta Planta = True
sonIguales _ _ = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (E n pk1e1 pk2e1, E n2 pk1e2 pk2e2 ) = pk1e1 : pk2e1 : pk1e2 : pk2e2 : []

--5. FUNCIONES POLIMORFICAS

--a
loMismo :: a -> a 
loMismo a = a

--b
siempreSiete :: a -> Int
siempreSiete _ = 7

--c
swap :: (a,b) -> (b, a)
swap (n, x) = (x, n)
--Respuesta c: Existen dos variables de tipo diferentes ya que las tuplas pueden contener elementos de distintos tipos.

--RESPUESTA: Estas funciones son polimorficas ya que se permite definir una funcion generica para cualquier tipos de datos, porque en estos casos no importa el tipo de dato.

--6. PATTERN MATCHING

--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (_:_) = False

--3
elPrimero :: [a] -> a 
--PRECOND: La lista no puede ser vacia
elPrimero (c:xs) = c

sinElPrimero :: [a] -> [a]
--PRECOND: La lista no puede ser vacia
sinElPrimero (c:xs) = xs

splitHead :: [a] -> (a, [a])
--PRECOND: La lista no puede ser vacia
splitHead (c:xs) = (c, xs)