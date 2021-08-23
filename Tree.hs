data Tree = ETree | Node Int (Tree) (Tree) deriving (Show, Read, Eq)
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

minimalTree = Node 3 (Node 2 ETree ETree) (Node 5 ETree (Node 6 ETree (Node 7 ETree (Node 8 ETree ETree))))

makeTree:: [Int] -> Tree -> Tree
makeTree [] tree = tree
makeTree (x:xs) ETree = makeTree xs (Node x (ETree) (ETree))
makeTree (x:xs) (Node a ETree ETree) = if x<a then (makeTree xs (Node a (Node x ETree ETree) ETree)) else (makeTree xs (Node a ETree (Node x ETree ETree)))
makeTree (x:xs) (Node a l r) = if x<a then (makeTree xs ((Node a) (makeTree [x] l) r)) else  (makeTree xs ((Node a) l (makeTree [x] r)))

abstmakeTree:: [Int] -> Tree
abstmakeTree liste = (makeTree liste ETree)

isinTree:: Int -> Tree -> Bool
isinTree _ ETree = False
isinTree x (Node a l r)
    | (x < a) = (isinTree x l)
    | (x > a) = (isinTree x r)
    | otherwise = True
    
addnodeTree:: Int -> Tree -> Tree
addnodeTree x tree = if (isinTree x tree) then (tree) else (makeTree [x] tree)
--adding an existing node has no effect.

deletenodeTree:: Int -> Tree -> Tree
deletenodeTree _ ETree = ETree
deletenodeTree x tree@(Node a ETree ETree) = if x==a then ETree else tree
deletenodeTree x (Node a l r)
    | x<a = (Node a (deletenodeTree x l) r)
    | x>a = (Node a l (deletenodeTree x r))
    | otherwise = case (Node a l r) of (Node a ETree r) -> r
                                       (Node a l ETree) -> l
                                       (Node a l r) -> (Node (findmaxNode l) (func l) r) 

func :: Tree -> Tree
func (Node a l ETree) = l
func (Node a l r) = (Node a l (func r))


findmaxNode:: Tree -> Int
findmaxNode (Node a _ ETree) = a
findmaxNode (Node a _ r) = findmaxNode r


findminNode:: Tree -> Int
findminNode (Node a ETree _) = a
findminNode (Node a l _) = findminNode l

lengthTree:: Tree -> Int
lengthTree ETree = 0
lengthTree (Node a ETree ETree) = 0
lengthTree (Node a l ETree) = 1 + (lengthTree l)
lengthTree (Node a ETree r) = 1 + (lengthTree r)
lengthTree (Node a l r) =  1 + (if (lengthl < lengthr) then lengthr else lengthl) 
                                where lengthl = (lengthTree l)
                                      lengthr = (lengthTree r)
findrange:: Tree -> (Int,Int) -> [Int] -> [Int]
findrange ETree _ lst = lst
findrange (Node a ETree ETree) (n1,n2) lst = if ((n1<=a) && (a<=n2)) then (lst++[a]) else lst
findrange (Node a ETree r) (n1,n2) lst
    | a > n2 = lst
    | a < n1 = (findrange r (n1,n2) lst)
    | otherwise = (findrange r (n1,n2) (lst++[a]))
findrange (Node a l ETree) (n1,n2) lst
    | a < n1 = lst
    | a > n2 = (findrange l (n1,n2) lst)
    | otherwise = (findrange l (n1,n2) (lst++[a]))
findrange (Node a l r) (n1,n2) lst
    | a > n2 = (findrange l (n1,n2) lst)
    | a < n1 = (findrange r (n1,n2) lst)
    | otherwise = [a]++((findrange l (n1,n2) lst)++(findrange r (n1,n2) lst))

findrangeabst:: Tree -> (Int,Int) -> [Int]
findrangeabst tree@(Node a tree1 tree2) tpl = findrange tree tpl []
--Node 3 (Node 2 ETree ETree) (Node 5 ETree (Node 6 ETree (Node 7 ETree (Node 8 ETree ETree))))

printTreePreorder:: Tree ->[Int]
printTreePreorder ETree = []
printTreePreorder tree@(Node a ETree ETree) = [a]
printTreePreorder tree@(Node a l ETree) = ([a] ++ (printTreePreorder l))
printTreePreorder tree@(Node a ETree r) = ([a] ++ (printTreePreorder r))
printTreePreorder tree@(Node a l r) = ([a] ++ (printTreePreorder l) ++ (printTreePreorder r))

printTreePostorder:: Tree ->[Int]
printTreePostorder ETree = []
printTreePostorder tree@(Node a ETree ETree) = [a]
printTreePostorder tree@(Node a l ETree) = ((printTreePostorder l) ++ [a])
printTreePostorder tree@(Node a ETree r) = ((printTreePostorder r) ++ [a])
printTreePostorder tree@(Node a l r) = ((printTreePostorder l) ++ (printTreePostorder r) ++ [a])

printTreeInorder:: Tree ->[Int]
printTreeInorder ETree = []
printTreeInorder tree@(Node a ETree ETree) = [a]
printTreeInorder tree@(Node a l ETree) = ([a] ++ (printTreeInorder l))
printTreeInorder tree@(Node a ETree r) = ([a] ++ (printTreeInorder r))
printTreeInorder tree@(Node a l r) = ((printTreeInorder l) ++ [a] ++(printTreeInorder r))

printhelper:: Tree -> [Char]
printhelper tree = (printReal (printTreePreorder tree) "" 0 1)

printReal:: [Int] -> [Char] -> Int -> Int -> [Char]
printReal [] str _ _ = str
printReal (x:xs) str c counter = if c==counter then (printReal xs (str++"\n"++ (show x)) 0 (counter*2)) else (printReal xs (str++"|"++(show x)) (c+1) counter)
