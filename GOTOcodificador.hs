
module CodDecodGOTO (codGOTO) where

import Data.Char(digitToInt)


data TipoIns = Asignacion | Incremento | Decremento | GOTO
     deriving Show

data InstruccionGOTO = Ins {nombre::TipoIns, var::Int, etiq::Int, goto::Int}
     deriving Show

representacion :: InstruccionGOTO -> ([Int],String,Int,Int)
representacion (Ins Asignacion v e g) = ([e],"V<-V",v,-1)
representacion (Ins Incremento v e g) = ([e],"V<-V+1",v,-1)
representacion (Ins Decremento v e g) = ([e],"V<-V-1",v,-1)
representacion (Ins GOTO v e g) = ([e],"IF V/= 0 GOTO L",v,g)

codificar :: InstruccionGOTO -> (Int,(Int,Int))
codificar (Ins Asignacion v e g) = (e,(0,v))
codificar (Ins Incremento v e g) = (e,(1,v))
codificar (Ins Decremento v e g) = (e,(2,v))
codificar (Ins GOTO v e g) = (e,(2+g,v))

skip = (Ins Asignacion 0 0 (-1)) ::InstruccionGOTO
gotoE = Ins GOTO 0 0 3

type ProgramaGOTO = [InstruccionGOTO]

codificarPrograma = map codificar

-- DECODIFICADOR --

representacionString :: ([Int],String,Int,Int) -> String
representacionString ([e],"V<-V",v,g) =

                     if v == 0 then if e/=0 then "[" ++ (show e) ++ "] " ++ "Y <- Y"
                                    else "Y<-Y"
                     else if odd v then if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "X" ++ (show ((div v 2)+1)) ++ "<-X" ++
                          (show ((div v 2)+1))
                                    else "X" ++ (show ((div v 2)+1))
                                    ++ "<-X" ++ (show ((div v 2)+1))
                     else if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "Z" ++ (show (div v 2)) ++ "<-Z" ++
                          (show (div v 2))
                                    else "Z" ++ (show (div v 2))
                                    ++ "<-Z" ++ (show (div v 2))
                          
representacionString ([e],"V<-V+1",v,g) =
                     if v == 0 then if e/=0 then "[" ++ (show e) ++ "] " ++ "Y <- Y+1"
                                    else "Y<-Y+1"
                     else if odd v then if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "X" ++ (show ((div v 2)+1)) ++ "<-X" ++
                          (show ((div v 2)+1)) ++ "+1"
                                    else "X" ++ (show ((div v 2)+1))
                                    ++ "<-X" ++ (show ((div v 2)+1)) ++ "+1"
                     else if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "Z" ++ (show (div v 2)) ++ "<-Z" ++
                          (show (div v 2)) ++ "+1"
                                    else "Z" ++ (show (div v 2))
                                    ++ "<-Z" ++ (show (div v 2)) ++ "+1"
representacionString ([e],"V<-V-1",v,g) =
                     if v == 0 then if e/=0 then "[" ++ (show e) ++ "] " ++ "Y <- Y-1"
                                    else "Y<-Y-1"
                     else if odd v then if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "X" ++ (show ((div v 2)+1)) ++ "<-X" ++
                          (show ((div v 2)+1)) ++ "-1"
                                    else "X" ++ (show ((div v 2)+1))
                                    ++ "<-X" ++ (show ((div v 2)+1)) ++ "-1"
                     else if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "Z" ++ (show (div v 2)) ++ "<-Z" ++
                          (show (div v 2)) ++ "-1"
                                    else "Z" ++ (show (div v 2))
                                    ++ "<-Z" ++ (show (div v 2)) ++ "-1"
representacionString ([e],"IF V/= 0 GOTO L",v,g) =
                     if v == 0 then if e/=0 then "[" ++ (show e) ++ "] "
                                                 ++ "IF Y /= GOTO " ++ (show g)
                                    else "IF Y /= GOTO " ++ (show g)
                     else if odd v then if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "IF X" ++ (show ((div v 2)+1)) ++ "/=0 GOTO "
                          ++ (show g)
                          else "IF X" ++ (show ((div v 2)+1)) ++ "/=0 GOTO "
                          ++ (show g)
                     else if e/=0 then
                          "[" ++ (show e) ++ "] " ++ "IF Z" ++ (show (div v 2)) ++ "/=0 GOTO " 
                          ++ (show g)
                                    else "IF Z" ++ (show (div v 2))
                                    ++ "/=0 GOTO" ++ (show g)

escribe :: ProgramaGOTO -> [String]
escribe p = (map representacionString) $ map representacion p

escribeLn x = putStr ( unlines x )

escribeGOTO p = escribeLn $ escribe p


-- CODIFICADOR
a1 = "Y<-Y"
a2 = "X2<-X2"
a3 = "Z3<-Z3"
i1 = "Y<-Y+1"
i2 = "X2<-X2+1"
i3 = "Z3<-Z3+1"
d1 = "Y<-Y-1"
d2 = "X2<-X2-1"
d3 = "Z3<-Z3-1"
g1 = "IF Y/=0 GOTO 2"
g2 = "IF X2/=0 GOTO 2"
g3 = "IF Z3/=0 GOTO 2"

codsinet :: String -> (Int,Int)
codsinet "Y<-Y" = (0,0)
codsinet "Y<-Y+1" = (1,0)
codsinet "Y<-Y-1" = (2,0)
codsinet expresion = 
      if length expresion == 6 then
         if expresion!!0 == 'X' then
            let var = digitToInt $ expresion!!1 in (0,2*var-1)
         else
           let var = digitToInt $ expresion!!1 in (0,2*var)
      else if  expresion!!6=='+' then
           if expresion!!0 == 'X' then
           let var = digitToInt $ expresion!!1 in (1,2*var-1)
           else
           let var = digitToInt $ expresion!!1 in (1,2*var)
      else if expresion!!6=='-' then
           if expresion!!0 == 'X' then
           let var = digitToInt $ expresion!!1 in (2,2*var-1)
           else
           let var = digitToInt $ expresion!!1 in (2,2*var)
      else if expresion!!3=='Y' then
           let l = digitToInt $ expresion!!13 in (l+2,0)
           else if expresion!!3=='X' then
                   let
                   l = digitToInt $ expresion!!14
                   var =digitToInt $ expresion!!4
                   in (l+2,2*var-1)
                else
                   let
                   l = digitToInt $ expresion!!14
                   var =digitToInt $ expresion!!4
                   in (l+2,2*var)

cod :: String -> (Int,(Int,Int))
cod expresion =
      if expresion!!0 == '[' then
      let e = digitToInt $ expresion!!1
      in (e,codsinet (drop 4 expresion))
      else (0,codsinet expresion)

codGOTO :: [String] -> [(Int,(Int,Int))]
codGOTO p = map cod p

programaEscrito = ["IF X1/=0 GOTO 1","Y<-Y+1","[1] Y<-Y+1"]