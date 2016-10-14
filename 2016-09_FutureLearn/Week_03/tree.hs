data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node n leftSubtree rightSubtree) =
  n + (treeSum leftSubtree) + (treeSum rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node n leftSubtree rightSubtree) minVal maxVal =
  let sortedLeft  = isSortedTree leftSubtree minVal n
      sortedRight = isSortedTree rightSubtree n maxVal
  in n >= minVal && n < maxVal && sortedLeft && sortedRight

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node n lt Leaf) = Node n lt (Node (n + 1) Leaf Leaf)
addNewMax (Node n lt rt) = Node n lt (addNewMax rt)

flatten :: Tree -> [Int]
flatten Leaf = []
flatten (Node n lt rt) = (flatten lt) ++ [n] ++ (flatten rt)


addValue :: Tree -> Int -> Tree
addValue Leaf newVal = Node newVal Leaf Leaf
addValue (Node oldVal lt rt) newVal
  | oldVal < newVal = Node oldVal lt (addValue rt newVal)
  | newVal < oldVal = Node oldVal (addValue lt newVal) rt
  | otherwise = Node oldVal lt rt
