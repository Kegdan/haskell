import Data.Char

data Lexem = O Char | N Double
	deriving (Show)

getNumber x = _getNumber x 0.0 

_getNumber [] n  = (n, [])
_getNumber l@(h:t) n  
	| h == '.'   = _getNumber2 1 t n 
	| isDigit h  = _getNumber t. (+) (digit h) $ 10 * n 
	| otherwise  = (n, l) 

_getNumber2 d [] n  = (n/d, [])
_getNumber2 d l@(h:t) n  
	| isDigit h = _getNumber2 (d*10) t. (+) (digit h) $ 10 * n 
	| otherwise =(n, l) 

digit =  fromIntegral.digitToInt

recognize acc [] = acc 

recognize acc l@(h:t)  	
	| elem h "+-/*()"  = recognize (O h: acc) t
	| isDigit h        = recognize (N (fst next) : acc) $ snd next
	| otherwise        = recognize acc t 
		where next = getNumber l

calc (x:acc) [] = (x,[])
calc acc (O ')':t) = calc (fst next:acc) (snd next)
	where next = calc [] t
calc (x:acc) (O '(':t) = (x,t)
calc acc (N x:t) = calc (x:acc) t
calc (x:y:acc) (O '+':t) = calc (x+y:acc) t
calc (x:y:acc) (O '-':t) = calc (x-y:acc) t
calc (x:y:acc) (O '/':t) = calc (x/y:acc) t
calc (x:y:acc) (O '*':t) = calc (x*y:acc) t
	
		
calculate = fst.calc [].recognize []

