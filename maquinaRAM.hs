module RAM where

type RAM = Registro
type Registro = [Int]
registroInic = [0, 0..]

type ProgramaRAM = [InstruccionRAM]

type InstruccionRAM = (Tipo,Variable)
type Tipo = Int
type Variable = Int

tipo = fst
var = snd


replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replace (n-1) newVal xs

-- -- Codificamos las Instrucciones
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

codPrograma :: [(String,Int)] -> ProgramaRAM
codPrograma = map codIns


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


computacionRAM :: ProgramaRAM -> [Int] -> Registro -> Int -> Int
computacionRAM p input r k = let (r',k') =(instruccionRAM (p!!(k-1)) input r k)
               in if k == 0 then (head r) else computacionRAM p input r' k'


ram p input = computacionRAM p input [0,0..] 1


pMultiplicacionRAM = [(1,1),(3,1),(3,5),(1,2),(3,2),(14,0),(3,3),(8,3),(11,2),(17,14),(5,4),(8,5),(3,4),(5,5),(8,5),(3,5),(5,3),(17,20),(15,5),(5,4),(19,0)] ::ProgramaRAM




-- INSTRUCCIONES GOTO --

type InstruccionGOTO = (Int,(Int,Int))

type ProgramaGOTO = [InstruccionGOTO]

instrucciondeGOTOaRAM :: InstruccionGOTO -> [InstruccionRAM]
instrucciondeGOTOaRAM (a,(0,c)) = [codIns ("ADD",0)]
instrucciondeGOTOaRAM (a,(1,c)) = [codIns ("LOAD",c+1),codIns ("ADD2",1),codIns ("STORE",c+1)]
instrucciondeGOTOaRAM (a,(2,c)) = [codIns ("LOAD",c+1),codIns ("SUB2",1),codIns ("STORE",c+1)]
instrucciondeGOTOaRAM (a,(b,c)) = [codIns ("LOAD",c+1),codIns ("JPOS",b-2)]


registroGOTO :: [Int] -> Registro
registroGOTO input = [0] ++ (concat [ [0,x] | x<-input ]) ++ [0,0..]

inicializarRegistro :: [Int] -> ProgramaRAM
inicializarRegistro input = concat
                    [[codIns ("READ",x),codIns ("STORE",2*y)] | (x,y) <- zip input [1..]]


programadeGOTOaRAM :: ProgramaGOTO -> [Int] -> ProgramaRAM
programadeGOTOaRAM pGOTO input = (inicializarRegistro input) ++ (concat $ map instrucciondeGOTOaRAM pGOTO) ++ [codIns ("LOAD",1)] ++ [codIns ("HALT",0)]

computacionGOTOporRAM :: ProgramaGOTO -> [Int] -> Int
computacionGOTOporRAM p input =
                      computacionRAM (programadeGOTOaRAM p input) input [0,0..] 1


pGOTO = [(0,(3,1)),(0,(1,0)),(1,(1,0))] :: ProgramaGOTO