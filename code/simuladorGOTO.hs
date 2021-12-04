-- GOTO SIMULADOR --
{-# LANGUAGE FlexibleContexts #-}
module SimuladorGOTO (gotosimulator) where

import Data.Numbers.Primes (primes)

-- TIPOS --

type InstruccionCodificada = (Int,(Int,Int))

type NumGod = [InstruccionCodificada]

type Programa = NumGod


-- FUNCIONES AUXILIARES --

left = fst
right = snd

deGodelaInt :: [Int] -> Int
deGodelaInt l = product [y^x | (x,y) <- zip l primes]

deCodificadoaInt :: InstruccionCodificada -> Int
deCodificadoaInt (a,(b,c)) = (2^a) * (2*d + 1) - 1
        where d = (2^b) * (2*c + 1) - 1

prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

count :: Eq a => [a] -> a -> Int 
count [] find = 0
count ys find = length xs
    where xs = [xs | xs <- ys, xs == find]


deIntaGodel :: Int ->[Int]
deIntaGodel n = reverse $ sinCerosInic $ reverse [count list x | x <- take (sqrtInt $ toInteger n) primes]
        where list = prime_factors n

sinCerosInic :: [Int] -> [Int]
sinCerosInic [] = []
sinCerosInic (x:xs) = if x==0 then sinCerosInic xs else x: xs

sqrtInt :: Integer -> Int
sqrtInt = round . sqrt. fromInteger

prodprimes2i :: [Int] -> Int
prodprimes2i x = product [y^x | (x,y) <- zip x primes2i]

primes2i = [ x | (x,y) <- zip primes [0..], odd y]


mu_i :: NumGod -> (Int,Int) -> Int
mu_i z u = head [ y | (x,y) <- zip z [0..], left x + 2 == left u]


-- SIMULADOR GOTO --

gotosimulator :: [Int] -> NumGod -> Int
gotosimulator x b =
               let 
               z = b
               s = prodprimes2i x
               k = 0::Int
               in c z s k
              

c :: NumGod -> Int -> Int -> Int
c z s k = if (k == length z) then (deIntaGodel s)!!0
    else let
    u = right $ z!!k
    p = primes!!(( right $ right $ z!!k)) in
      if left u == 0 then 
      c z s (k+1)
      else if left u == 1 then 
      c z (s*p) (k+1)
      else if mod s p /= 0 then 
      c z s (k+1)
      else if left u == 2 then 
      c z (div s p) (k+1)
      else c z s (mu_i z u) 









p = [(0,(3,1)),(0,(1,0)),(1,(1,0))] :: NumGod