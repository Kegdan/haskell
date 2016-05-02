--Поиск минимальной выпуклой оболочки
import Data.List; import Data.Ord; import System.Random
--общие функции и типы
data Point = P{x::Double,y::Double}
	deriving (Show,Eq) 
	
getRotate a b c = baX * cbY - baY * cbX
	where baX = x b - x a; baY = y b - y a;
		  cbX = x c - x b; cbY = y c - y b;
		 
sortFunc a b c 
	|k < 0  = LT
	|k == 0 = compare (long a c) (long a b) 
	|k > 0  = GT
		where k = getRotate a b c


long a b = (x b - x a)*(x b - x a) + (y b - y a)*(y b - y a)
		
getLeftPoint = minimumBy (comparing x)
--Джарвис
getMBOJarvis l = mboJ fp l fp
	where fp = getLeftPoint l		
		
mboJ current list fp 
	|getRotate current next fp > 0   = []
	|True                            = current : mboJ next listWOC fp
		where listWOC = filter ((/=)current) list;
			  next    = minimumBy (sortFunc current) listWOC;
--Грехем			
getMBOGragam = tail.throwGraham.sortGraham 

sortGraham list = fp : sortBy (sortFunc fp) list
	where  fp = getLeftPoint list

throwGraham (f:s:t) = mboG [s,f] t
		   
mboG fs@(f:s:st) sn@(h:t)
	|sortFunc s f h /= LT  = mboG (s:st) sn
	|True                  = mboG(h:fs) t

mboG l [] = l
--тесты		  
randomList lower upper seed = map(\x-> x - snd (properFraction x)).randomRs (lower, upper) $ mkStdGen seed

gPL xl xu xSeed yl yu ySeed count = zipWith P (take count $ randomList xl xu xSeed) .take count $ randomList yl yu ySeed

testList1 = [P 0 (-1), P (-1) 0, P 0 1,P 1 0,P (-0.5) (-0.5),P 0.5 (-0.5),P (-0.5) 0.5,P 0.5 0.5,P 0 0]
		  
testList2 = [P 0 0, P 1 0, P 0 1,P 2 0,P 1 1,P 0 2,P 2 1,P 1 2,P 2 2]
	
bp:: [Point] -> IO ()
bp = mapM_ print 

testJ1  = getMBOJarvis testList1		
		
testG1  = getMBOGragam testList1

testJ2  = getMBOJarvis testList2		
		
testG2  = getMBOGragam testList2

testJ  = getMBOJarvis $ gPL (-10) 10 1 0 100 1 60
		
testG  = getMBOGragam $ gPL (-10) 10 1 0 100 1 60
