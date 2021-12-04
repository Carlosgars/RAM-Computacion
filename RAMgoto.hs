-- MAQUINAS RAM Y LENGUAJE GOTO
-- ALUMNO: CARLOS GARCIA SANCHO
-- FECHA: 21/01/2021 -- 

module RAM where

------------------------------------------------------------------------------------------
--------------------------------------- R A M --------------------------------------------
------------------------------------------------------------------------------------------

-- Creamos los tipos InstruccionRAM y ProgramaRAM.

-- Un programa RAM sera una lista de instrucciones RAM.
-- Cada instruccion esta codificada por un par.

type Registro = [Int]

type ProgramaRAM = [InstruccionRAM]

type InstruccionRAM = (Tipo,Variable)
type Tipo = Int
type Variable = Int

                            ----------------------------
                            ----- CODIFICACION RAM -----
                            ----------------------------

-- Tipos de instrucciones RAM:
--1 READ    r0 = i_j 
--2 READ1   r0 = i_rj

--3 STORE   rj = r0
--4 STORE1  r_rj = r0

--5 LOAD    r0 = rj
--6 LOAD1   r0 = r_rj
--7 LOAD2   r0 = j

--8 ADD     r0 = r0 + rj
--9 ADD1    r0 = r0 + r_rj
--10 ADD2    r0 = r0 + j

--11 SUB     r0 = r0 - rj
--12 SUB1    r0 = r0 - r_rj
--13 SUB2    r0 = r0 - j

--14 HALF    r0 = [r0 / 2]
--15 JUMP    k = j
--16 JPOS    if r0 > 0 then k = j
--17 JZERO   if r0 = 0 then k = j
--18 JNEG    if r0 < 0 then k = j
--19 HALT    k = -1


--------------------------------------------------------------------------------------------
-- Cada instruccion es codificada por un par ( , ) donde el primer elemento es
-- el tipo de instruccion y el segundo elemento es la variable que ocurre en la instrucción.
-- La variable j puede representar una posicion del registro, una posicion del input
-- o un entero.

codIns :: (String,Int) -> InstruccionRAM
codIns ("READ",v) = (1,v)
codIns ("READ1",v) = (2,v)
codIns ("STORE",v) = (3,v)
codIns ("STORE1",v) = (4,v)
codIns ("LOAD",v) = (5,v)
codIns ("LOAD1",v) = (6,v)
codIns ("LOAD2",v) = (7,v)
codIns ("ADD",v) = (8,v)
codIns ("ADD1",v) = (9,v)
codIns ("ADD2",v) = (10,v)
codIns ("SUB",v) = (11,v)
codIns ("SUB1",v) = (12,v)
codIns ("SUB2",v) = (13,v)
codIns ("HALF",_) = (14,0)
codIns ("JUMP",v) = (15,v)
codIns ("JPOS",v) = (16,v)
codIns ("JZERO",v) = (17,v)
codIns ("JNEG",v) = (18,v)
codIns ("HALT",_) = (19,0)
codIns (_,_) = (19,-1000)


--------------------------------------------------------------------------------------------
-- instruccionRAM recibe una instruccion x, un input (lista de Int), un registro r
-- (lista de Int) y un Int k que representa el contador de la computacion del programa RAM.
-- La funcion devuelve un par formado por el registro y el contador obtenidos tras ejecutar
-- x sobre input, r, k.
-- Distinguimos por casos segun el tipo de instruccion RAM.

instruccionRAM :: InstruccionRAM -> [Int] -> Registro -> Int -> (Registro,Int)
instruccionRAM x input r k =
    let j = var x  in
    if tipo x == 1 then (replace 0 (input!!(j-1)) r,k+1)
    else if tipo x == 2 then ((replace 0 (input!!(r!!(j-1))) r),k+1)
    else if tipo x == 3 then ((replace j (r!!0) r),k+1)
    else if tipo x == 4 then (replace (r!!j) (r!!0) r,k+1)
    else if tipo x == 5 then (replace 0 (r!!j) r,k+1)
    else if tipo x == 6 then (replace 0 (r!!(r!!j)) r,k+1)
    else if tipo x == 7 then (replace 0 j r,k+1)
    else if tipo x == 8 then (replace 0 (r!!0 + r!!j) r,k+1)
    else if tipo x == 9 then (replace 0 (r!!0 + r!!(r!!j)) r,k+1)
    else if tipo x == 10 then (replace 0 (r!!0 + j) r,k+1)
    else if tipo x == 11 then (replace 0 (r!!0 - r!!j) r,k+1)
    else if tipo x == 12 then (replace 0 (r!!0 - r!!(r!!j)) r,k+1)
    else if tipo x == 13 then (replace 0 (r!!0 - j) r,k+1)
    else if tipo x == 14 then (replace 0 (div (r!!0)  2) r,k+1)
    else if tipo x == 15 then (r,j)
    else if tipo x == 16 then
         if r!!0 > 0 then (r,j)
         else (r,k+1)
    else if tipo x == 17 then
         if r!!0 == 0 then (r,j)
         else (r,k+1)
    else if tipo x == 18 then
         if r!!0 < 0 then (r,j) else (r,k+1)
    else if tipo x == 19 then (r,0)
    else ([-1000],0)



                              ---------------------------
                              ------ SIMULADOR RAM ------
                              ---------------------------

-------------------------------------------------------------------------------------------- 
-- computacionRAM recibe un programa RAM, un input, un registro r y el contador k 
-- (La instruccion k es la instruccion que ocupa el numero k en el programa).

-- La funcion aplica de forma recursiva cada instruccion que corresponde en el programa
-- partiendo de la k-ésima.
-- La recursion consiste en cambiar el contador k y actualizar el valor del
-- registro segun la instruccion que se aplique.

-- Devuelve el valor que obtenemos si el programa partiendo de la instruccion k-esima para.

computacionRAM :: ProgramaRAM -> [Int] -> Registro -> Int -> Int
computacionRAM p input r k = let (r',k') =(instruccionRAM (p!!(k-1)) input r k)
               in if k == 0 then (head r) else computacionRAM p input r' k'


------------------------------------------------------------------------------------------- 
-- ram realiza la computacion del programa RAM sobre un input, haciendo uso de la funcion
-- anterior, inicializando el registro = [0,0,..] y haciendo k = 1. 

ram p input = computacionRAM p input [0,0..] 1



                               ----------------------------
                               ------ AUXILIARES RAM ------
                               ----------------------------

tipo = fst
var = snd


replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replace (n-1) newVal xs





-------------------------------------------------------------------------------------------
----------------------------------------- G O T O -----------------------------------------
-------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------- 
-- Codificamos cada instruccion por un par (a, (b,c) ) donde el segundo elemento es otro
-- par.

-- a representa la etiqueta, b el tipo de instruccion y c la variable que ocurre en la
-- instruccion.

-- Tipos de instruccion: 0 SKIP, 1 INCREMENTO, 2 DECREMENTO, >=3 IF..GOTO.

-- Variables: Las ordenamos del siguiente modo [Y,X1,Z1,X2,Z2,...] y a cada una le hacemos
-- corresponder la posicion que ocupan en la lista (empezando a contar desde 0).

-- Las instrucciones de tipo salto son de la siguiente forma: (a,(b,c)) donde se salta a 
-- la instruccion con etiqueta (b-2).

-- Un programa GOTO sera una lista de instrucciones GOTO.

type InstruccionGOTO = (Int,(Int,Int))
type ProgramaGOTO = [InstruccionGOTO]




                          -----------------------------------
                          ------ COMPILADOR GOTO A RAM ------
                          -----------------------------------

------------------------------------------------------------------------------------------- 
-- Cada instruccion en GOTO es equivalente a un conjunto de instrucciones en RAM.

-- instrucciondeGOTOaRAM recibe una instruccion GOTO y un Int k que representa la posicion
-- que ocupa en la numeracion del programa RAM.
-- La numeracion es necesaria para las instrucciones que incluyen saltos (IF y decremento)

-- Decremento debe incluir un salto en RAM, ya que hay que comprobar si la variable es 0.
-- Si es 0, devuelve 0,
-- si es mayor que cero, devuelve la variable menos 1.

-- La variable que en GOTO es codificada por c en RAM es codificada por c+1.


instrucciondeGOTOaRAM :: InstruccionGOTO -> Int -> [InstruccionRAM]
instrucciondeGOTOaRAM (a,(0,c)) k =
                      map codIns [("ADD",0)]
instrucciondeGOTOaRAM (a,(1,c)) k =
                      map codIns [("LOAD",c+1),("ADD2",1),("STORE",c+1)]
instrucciondeGOTOaRAM (a,(2,c)) k =
                      map codIns [ ("LOAD",c+1), ("JPOS",k+5), ("LOAD2",0), ("STORE", c+1), ("JZERO",k+7),("SUB2",1), ("STORE",c+1)]
instrucciondeGOTOaRAM (a,(b,c)) k =
                      map codIns [ ("LOAD",c+1), ("JPOS",k)]



------------------------------------------------------------------------------------------- 
-- inicializarRegistro devuelve una lista de instrucciones RAM que copian el input en el
-- registro, de la siguiente manera:
-- input [X1,X2,...,Xn] --> registro [0,0,X1,0,X2,0,...,0,Xn,0,0,...]
-- donde Xi se corresponde con la posicion 2i del registro, r_2i.
-- Recordemos que el primer elemento del registro es r_0.

inicializarRegistro :: [Int] -> ProgramaRAM
inicializarRegistro input = concat
                    [[codIns ("READ",y),codIns ("STORE",2*y)] | y <- [1..(length input)] ]



------------------------------------------------------------------------------------------- 
-- compilator recibe un programa GOTO y un input y genera un programa RAM que lo calcula.
-- Necesitamos introducir el input para inicializar el registro.

-- El programa RAM estará formado por 3 bloques:
-- 1. Inicializacion del registro
-- 2. Traduccion instruccion a instruccion del programa GOTO 
-- 3. Instrucciones para parar y devolver el resultado.

-- Tenemos en cuenta que si hacemos un salto a una etiqueta que no aparece en el programa,
-- para y devuelve el resultado.
-- (Codificamos este caso con a = -1).

-- NOTA: El programa GOTO recibe un tratamiento (ver --AUXILIARES PARA COMPILADOR--) para
-- poder traducir correctamente las instrucciones de salto.

compilator:: ProgramaGOTO -> [Int] -> ProgramaRAM
compilator p input = let
           xs = auxiliar p
           l = length (inicializarRegistro input)
           in
           (inicializarRegistro input)
           ++
           concat [ if b<3 then instrucciondeGOTOaRAM (a,(b,c)) (k+l)
           else if a == -1 then (map codIns [("LOAD",1),("HALT",0)])
           else instrucciondeGOTOaRAM (a,(b,c)) ((fst (xs!!a))+l)
           | (k,(a,(b,c))) <- xs ]
           ++
           (map codIns [("LOAD",1),("HALT",0)])



                      -----------------------------------------------
                      ------ SIMULADOR RAM PARA PROGRAMAS GOTO ------
                      -----------------------------------------------

-------------------------------------------------------------------------------------------
-- Recibimos un programa GOTO p y un input. Aplicamos el simulador RAM al programa RAM
-- obtenido al aplicar el compilador a p y al input.

ramGOTO  :: ProgramaGOTO -> [Int] -> Int
ramGOTO p input = ram (compilator p input) input



                         ----------------------------------------
                         ------ AUXILIARES PARA COMPILADOR ------
                         ----------------------------------------

-------------------------------------------------------------------------------------------
-- En estas dos funciones modificamos el programa GOTO codificado con etiquetas pasando
-- a un programa en el que las instrucciones de tipo salto son:
-- ( posicion que ocupa la instruccion a la que se salta, (3,c) )

posicionsaltoGOTO :: ProgramaGOTO -> Int -> Int
posicionsaltoGOTO p etiq = head ([ x | (x,y) <- zip [0..] p, fst y == etiq ] ++ [-1])

gotoSinEtiqueta :: ProgramaGOTO -> ProgramaGOTO
gotoSinEtiqueta programa = [if b<3 then (0,(b,c))
          else (posicionsaltoGOTO programa (b-2),(3,c)) |
          (a,(b,c)) <- programa]


-------------------------------------------------------------------------------------------
-- Esta funcion relaciona (empareja) cada tipo de instruccion GOTO con el numero de
-- instrucciones RAM que hacen falta para simularla.

longitudEnRAM :: InstruccionGOTO -> (Int,InstruccionGOTO)
longitudEnRAM (a,(0,c)) = (1,(a,(0,c)))
longitudEnRAM (a,(1,c)) = (3,(a,(1,c)))
longitudEnRAM (a,(2,c)) = (7,(a,(2,c)))
longitudEnRAM (a,(b,c)) = (2,(a,(b,c)))


-------------------------------------------------------------------------------------------
-- En esta funcion asociamos a cada instruccion GOTO el lugar que ocuparia la primera
-- instruccion del bloque que la codifica en el programa RAM.
-- De esta forma ya podemos traducir las instrucciones de tipo salto, ya que conocemos
-- las instrucciones a las que se realiza dicho salto en el nuevo programa RAM.

-- auxiliar devuelve el programa GOTO con la numeracion de RAM, en formato
-- lista formada por (posicion que ocupa en RAM, instruccion).

posicionEnRAM xs = [(1,snd $ head xs)] ++
   [ ( (1+) $ sum $ take x (map fst xs),y) | (x,y) <- zip [1..length xs-1] (map snd (tail xs)) ] 


auxiliar :: ProgramaGOTO -> [(Int, InstruccionGOTO)]
auxiliar programa =  posicionEnRAM $ map longitudEnRAM $ gotoSinEtiqueta programa





------------------------------------------------------------------------------------------
---------------------------------- E J E M P L O S ---------------------------------------
------------------------------------------------------------------------------------------


pMultiplicacionRAM = [(1,1),(3,1),(3,5),(1,2),(3,2),(14,0),(3,3),(8,3),(11,2),(17,14),(5,4),(8,5),(3,4),(5,5),(8,5),(3,5),(5,3),(17,20),(15,5),(5,4),(19,0)] :: ProgramaRAM
-- Multiplica dos numeros.


pGOTO = [(0,(3,1)),(0,(1,0)),(1,(1,0))] :: ProgramaGOTO
--     IF X1 /= 0 GOTO A
--     Y <- Y + 1
-- [A] Y <- Y + 1


pRAM = [(5,2),(16,6),(5,1),(10,1),(3,1),(5,1),(10,1),(3,1),(19,0)] :: ProgramaRAM
-- pGOTO a RAM.


pGOTO2 = [(0,(1,0)),(0,(2,0)),(0,(2,0))] :: ProgramaGOTO
-- [A] Y <- Y + 1
--     X1 <- X1 - 1
--     IF X1 /= 0 GOTO A


pGOTO3 = [(0,(3,1)),(0,(1,0)),(0,(1,0)),(1,(2,0))] :: ProgramaGOTO
--     IF X1 /= 0 GOTO A
--     Y <- Y + 1
--     Y <- Y + 1
-- [A] Y <- Y - 1


pGOTO4 = [(0,(1,0)),(0,(1,0)),(0,(3,1)),(0,(1,0)),(1,(4,3)),(1,(2,0)),(2,(2,0))] :: ProgramaGOTO
--     Y <- Y + 1
--     Y <- Y + 1
--     IF X1 /= 0 GOTO A
--     Y <- Y + 1
-- [A] IF X2 /= 0 GOTO B
-- [A] Y <- Y - 1
-- [B] Y <- Y - 1


pGOTO5 = [(1,(1,0)),(0,(2,1)),(0,(3,1))] :: ProgramaGOTO
-- [A] Y <- Y + 1
--     X1 <- X1 - 1
--     IF X1 /= 0 GOTO A


pGOTO6 = [(1,(1,0)),(0,(7,0)),(0,(2,1)),(0,(3,1))] :: ProgramaGOTO
-- [A] Y <- Y + 1
--     IF Y /= 0 GOTO E
--     X1 <- X1 - 1
--     IF X1 /= 0 GOTO A







