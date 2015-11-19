import Data.List; import Control.Arrow; import Data.Ord

data HTree a = Leaf a | Branch (HTree a) (HTree a)
	deriving (Show, Eq, Ord)
	
getStatistic = map(length &&& head). group. sort

treeBypass (Branch l r) = map (second('0': )) (treeBypass l) ++ map (second('1': )) (treeBypass r)
treeBypass (Leaf x) = [(x, "")]

createTree [(_, t)] = t
createTree ((w1,t1) : (w2,t2):wts) = createTree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
 
huffman :: (Ord w, Num w) => [(w, a)] -> [(a, String)]
huffman = treeBypass. createTree. sortBy (comparing fst). map (second Leaf)

code message = codeByKey message message

codeByKey key message = mapM(flip lookup huffmanSet) message >>= return.foldl1 (++)
	where huffmanSet = huffman $ getStatistic key

decodeByKey:: String ->String ->Maybe String 
decodeByKey key message = sequence ( dBK gCBC message [] )
	where gCBC = getCharByCode $ huffman $ getStatistic key

dBK gCBC [] acc = acc

dBK gCBC message acc =  case newLetter of
	Nothing          -> [Nothing]
	Just (char,code) -> dBK gCBC (drop (length code) message) (acc++(Just char):[])
	where newLetter = gCBC message

getCharByCode huffmanSet code = find (\x->isPrefixOf (snd x) code) huffmanSet 
