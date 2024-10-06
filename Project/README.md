# DESCRIPTION OF THE GAME:

BIRDING ADVENTURE GAME: "Try to catch the bird without disturbing its serenity!"

The goal for the user is to catch the bird in a forest of binary trees. 
The game begins by asking the user to select the forest parameters (for ex. number of trees,
their height, number of tree species).
Then, we place the bird on a random branch of a random tree which the player is supposed to reach. 
After the user makes the correct choice of the tree (they can ask for hints), they are positioned at the root
and their goal is to climb the branch where the bird is placed. However, we include a parameter which quantifies
the "serenity of the bird". When the user moves towards the bird, the parameter is unchanged, but if the user 
selects the wrong subtrees two times in a row the parameter decreases. When serenity reaches zero, the bird changes
its position in the forest and the game resets in the same forest. The initial value of the parameter when the user
reaches the correct tree depends on the number of moves the user makes to find that tree at the beginning of the game.
We will also reward the user for making three consecutive moves within the tree by increasing the "serenity parameter".

______________________________________________________________________________________________


# CODE STRUCTURE (BirdingAdventureGame.hs)

Based on the BinTreeWorld.hs template file.

The game consists of three phases (0, 1 and 2).

# Phase 0:
We display the welcome message - function `welcome`. <br>
Then, the user is asked to select the input :
1) Number of trees - function `input_num_trees`
2) Height of the trees (needed for phase 2) - function `input_tree_height`
3) Number of types of trees - function `input_num_tree_types`
Then, automatically select the correct number of types from a predetermined list of all_tree_types and go to **phase 1**

## User input:
In phase 0, the functions `input_num_trees`, `input_tree_height` and `input_num_tree_types` accept either a number (between 1-20 / "one"-"twenty") or a "quit" command to exit the game. Each function passes the obtained information to the next as input. 

# Phase 1:
1) We create a forest with given parameters - function `create_forest`
2) We select a random tree where the bird will be placed - by using `randomRIO`
3) We ask the user at which position they would like to start the game - function `select_starting_position`, until they select a valid position in the forest, and set 
the *serenity parameter* to 20.
4) If the user selected the correct tree, **phase 2** begins immediately, otherwise, we make a call to the `loop` function to allow the user to move through the list.

## User moves through the list:
1) If `serenity` is zero, let the user know the bird changed the tree and restart **phase 1**, otherwise, go to step 2
2) We ask the user for the label of the tree to move to:
  If the selected tree is correct, we let them know and start **phase 2**
  Otherwise, we give them hints if they moved closer to the bird, decrease the `serenity` and go back to step 1.

In both `loop` and `select_starting_position`, the accepted user input are commands "Hint", "Quit" and a number that represents a label of some tree.

# Phase 2:
The user is placed at the root of the tree and asked for their move:
### At a leaf :
- If the bird is there, we end the game
- If the bird is not there, tell that to the user

### At a node:
- If the move is on the right path, reset the `wrong_moves` parameter to 0
- If the move is on the wrong path, decrement `serenity` by 1 and:
  - if `serenity<=0` go to **phase 0**
  - else increase the number of consecutive wrong moves then check if it's equal to 3:
      - case `wrong_moves==3` -> print a message and reset `wrong_moves` to 0
      - otherwise do nothing


_______________________________________________________________________________________________


# DataTypes.hs

Based on the Bin.hs template file.

# Representing the forest

Forest is a list of trees (`Bin_Tree`), defined as:

    data Bin_Tree = Bin_Tree {
      tree_type::String, 
      label::Int, 
    }

Possible types of trees (20 different types) are given in the `all_tree_types` list.

Forest is used in phase 0 and 1.

# Representing the User

A user is defined with the following parameters:

    data User = User {
      tree_index::Int, -- position in the forest
      serenity::Int, -- intuitively, this is the number of remaining moves before the bird changes position
      wrong_moves::Int -- track the number of consecutive wrong moves in phase 2
    }

The `User` type is needed for phase 1 and 2.

# Representing the Binary Tree

In phase 2, the binary tree is represented as follows: 

      data Bin = L Bool | B Bool Bin Bin
        deriving (Show, Eq)

In the tree, the unique path from the root to the leaf with the bird is identified such that all the nodes on this path (including the root and the leaf) have their boolean set at True.

In order for the user to move in the tree, we are using "one-hole context" structures, which are also containg a bool value. They are paired with trees using zippers. 

      data BinCxt = Hole
              | B0 Bool BinCxt Bin
              | B1 Bin Bool BinCxt
        deriving (Show,Eq)

      type BinZip = (BinCxt,Bin)

These types are needed only for phase 2. 

______________________________________________________________________________________________

# Parser.hs

Based on the Parser.hs template file.

Two additional functions, to accommodate parsing "Hint requests" and "Numerical inputs" (numbers from 1-20, digits or English words) :

    -- Parse a hint request
    parseHint :: Parser String Cmd
    parseHint = do
      match "hint" >> return Hint

    -- Parse a number parameter
    parseNumber :: Parser String Cmd
    parseNumber = do
      n <- number
      return (Num n)

_______________________________________________________________________________________________

# Cmd.hs

Based on the Cmd.hs template file.

Additional commands (`Hint` and `Num Int`) for parsing "Hint requests" and "Numerical inputs" that complement the Parser.hs file.