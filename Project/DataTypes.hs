-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines


module DataTypes where

import Data.Tree
import System.Random
import Data.List
import Data.Bool

-- data type for representing the trees in our forest(list of trees from phase 0 on github)
data Bin_Tree = Bin_Tree {
  tree_type::String, 
  label::Int
}
  deriving (Show, Eq)

-- tree types
all_tree_types :: [String]
all_tree_types = ["orange tree" , "apple tree", "cherry tree", "banana tree", "plum tree", "oak tree",
 "pine tree", "peach tree", "apricot tree", "lemon tree", "pear tree", "nectarine tree", "walnut tree",
 "hazelnut tree", "almond tree", "cacao tree", "willow tree", "lime tree", "grapefruit tree", "fig tree"]


-- data type for representing a user
data User = User {
  tree_index::Int, 
  serenity::Int, 
  wrong_moves::Int
}

data Bin = L Bool | B Bool Bin Bin
  deriving (Show, Eq)

createBinaryTree :: Int -> Bin
createBinaryTree 0 = L False  -- a leaf node for height 0
createBinaryTree h = B False (createBinaryTree (h-1)) (createBinaryTree (h-1))

-- set the root to True
setRoot :: Bin -> Bin
setRoot (L _) = L True
setRoot (B b left right) = B True left right

-- generate a random path from the root to a leaf
generateRandomPath :: Int -> Int -> IO [Bool]
generateRandomPath 0 _ = return []
generateRandomPath h current_height = do
    choose_left <- randomIO
    rest <- generateRandomPath (h-1) (current_height-1)
    return (choose_left : rest)

-- update the tree to True on the given path
updatePath :: Bin -> [Bool] -> Bin
updatePath (L _) _ = L True
updatePath (B b left right) (True : path) = B True (updatePath left path) right
updatePath (B b left right) (False : path) = B True left (updatePath right path)

-- generate the binary tree
placeBird :: Int -> IO Bin
placeBird tree_height = do
    let initial_tree = createBinaryTree tree_height
    let tree_1 = setRoot initial_tree
    random_path <- generateRandomPath tree_height tree_height
    let final_tree = updatePath tree_1 random_path
    return final_tree




-- a "one-hole context" for a binary tree may be thought of as a
-- binary tree with a hole for another binary tree
data BinCxt = Hole
            | B0 Bool BinCxt Bin
            | B1 Bin Bool BinCxt
  deriving (Show,Eq)

-- Plugging a one-hole context with a binary tree to produce a binary
-- tree is accomplished by the following function
plug :: BinCxt -> Bin -> Bin
plug Hole      t = t
plug (B0 b c (B _ _ t2)) t = B b (plug c t) t2
plug (B1 (B _ t1 _) b c) t = B b t1 (plug c t)

-- Under this definition of plugging, values of type BinCxt are
-- interpreted "inside-out" as describing paths *from* the hole *to*
-- the root.  Alternatively, we can interpret contexts "outside-in" as
-- describing paths from the root to the hole, in which case we would
-- use the following plugging function.


-- But the inside-out representation is more useful for navigating
-- within a tree using "zippers".

-- A zipper is a pair of a one-hole context c and a tree t, which we
-- think of as defining a pointer to t as a subtree of u = plug c t.
type BinZip = (BinCxt,Bin)
-- (The terminology comes from Gérard Huet's paper, "The Zipper".)

-- The following functions implement moving the pointer up to the
-- left child, up to the right child, or down to the parent of a
-- subtree.  (Note that these operations are only partial, i.e., return a
-- Maybe type, since the subtree may not have a child or a parent.)

go_left :: BinZip -> Maybe BinZip
go_left (c,B b t1 t2) = Just (B0 b c t2,t1)  -- focus on the left child
go_left (c,L _)       = Nothing            -- (leaf => no left child)

go_right :: BinZip -> Maybe BinZip
go_right (c,B b t1 t2) = Just (B1 t1 b c,t2) -- focus on the right child
go_right (c,L _)       = Nothing           -- (leaf => no right child)

go_down :: BinZip -> Maybe BinZip
go_down (B0 b c (B _ t2_1 t2_2), t) = Just (c, B b t t2_2)  -- focus on parent *from* left child
go_down (B1 (B _ t1_1 t1_2) b c, t) = Just (c, B b t1_1 t)  -- focus on parent *from* right child
go_down (Hole, _) = Nothing  -- (root => no parent)


-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.


treeFromBin :: Bin -> Tree String
treeFromBin (L b)         = Node ("L " ++ show b) []
treeFromBin (B b t1 t2) = Node ("B " ++ show b) [treeFromBin t1,treeFromBin t2]

treeCxtFromBinCxt :: BinCxt -> Tree String -> Tree String
treeCxtFromBinCxt Hole      t = t
treeCxtFromBinCxt (B0 b c t1) t = treeCxtFromBinCxt c (Node ("B "++ show b) [t, treeFromBin t1])
treeCxtFromBinCxt (B1 t1 b c) t = treeCxtFromBinCxt c (Node ("B "++ show b) [treeFromBin t1, t])

treeFromBinZip :: BinZip -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=marker})
  where
    t' = treeFromBin t
    marker = "@ <--you"

drawBin :: Bin -> String
drawBin = drawTree . treeFromBin

drawBinZip :: BinZip -> String
drawBinZip = drawTree . treeFromBinZip



