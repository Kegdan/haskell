import Data.List
--обычный
constA = 4 * 7 + 1
constC = 37
constN = 256
--для простого
constPA = 5
constPN = 263 

rand a c m = iterate (flip mod m.(+)c.(*)a) 1 
-- обычный
getRand      = rand constA constC constN
-- m - простое
getPrimeRand = rand constPA 0    constPN
--Макларен
constMac = 100

getMacRand = gmr getRand getPrimeRand (take constMac getRand)

gmr (x:seq1) (y:seq2) map = (map !! j):gmr seq1 seq2 (replaceN map j x)
	where j = round ( (fromIntegral(constMac-1))*((fromIntegral y) / (fromIntegral (constPN-1))))
	
replaceN l n x = take n l ++ x: drop (n+1) l	
--хи квадрат
spanBy x k l  
	| s == []  = [f]
	| otherwise = (:)f $ spanBy x (k+1) s
		where (f,s) = span (\a-> x*k >=(fromIntegral a)) l

x2 seqE seqN = sum $ zipWith f seqE seqN
	where f a b = (a-b) * (a-b) / b
		
getMl cC sC max = map (fromIntegral.length).spanBy k 1.sort.take sC
	where k = (fromIntegral max) / cC

getX2 cC sC max seq= x2 s1 s2
	where s1 = getMl cC sC max seq;
		  s2 = repeat ((fromIntegral sC) / cC)		  
--тест на корреляцию

correl n l = upCorrel n list / downCorrel n list
	where list = take n l
		  
upCorrel   n list = (fromIntegral((n * (sumM list)) - sumSquare list))
downCorrel n list = (fromIntegral( n * sum (map (^2) list) - sumSquare list) )

sumSquare list = sum (list) ^ 2
		  
sumM l        = sum1 (last(l):l)
sum1 (h:[])   = 0
sum1 (h:h2:t) = h*h2 + sum1 (h2:t)

--тесты
t1 = getX2 10 200 constN getRand

t2 = getX2 10 200 (constPN-1) getPrimeRand

t3 = getX2 10 200 constN getMacRand

c1 = correl 200 getRand

c2 = correl 200 getPrimeRand

c3 = correl 200 getMacRand





