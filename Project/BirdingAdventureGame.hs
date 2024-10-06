import DataTypes
import Cmd
import Parser

import System.IO
import System.Random
import Control.Concurrent (threadDelay)

-- initialize the forest
create_tree :: String -> Int -> Bin_Tree
create_tree t_type index = Bin_Tree{
  tree_type = t_type,
  label = index + 1
}

create_forest :: Int -> [String] -> Int -> [Bin_Tree]
-- num_trees, tree_types, tree_type_index
create_forest 0 _ _ = []
create_forest n tree_types m = (create_tree (tree_types!!m) (n-1)) : (create_forest (n-1) tree_types ((m+1) `mod` (length tree_types)))

-- initialize the user
create_user :: Int -> Int -> User
create_user position s = User {
  tree_index = position, 
  serenity = s, 
  wrong_moves = 0
}

create_user_p2 :: Int -> Int -> User
create_user_p2 s wm = User {
  tree_index = 0, 
  serenity = s, 
  wrong_moves = wm
}


-- phase 0
welcome :: IO ()
welcome = do
  putStrLn "Welcome to BIRDING ADVENTURE GAME:\n\"Try to catch the bird without disturbing its serenity!\" "
  putStrLn "Before you start the game, let's customize our forest:"
  input_num_trees
  
input_num_trees :: IO ()
input_num_trees = do
  putStrLn "How many trees do you want in the forest?"
  line <- getLine
  case parseInput parseCmd line of                 -- parse the input
          Just Quit -> do
            putStrLn "Okay."
            putStrLn "Goodbye."
            return ()
          Just (Num n) -> do
            putStrLn "Added the number of trees in the forest"
            input_tree_height n
          _ -> do
            putStrLn "I'm sorry, I do not understand."
            input_num_trees

input_tree_height :: Int -> IO()
input_tree_height num_trees = do
  putStrLn "What do you wish to be the height of your trees?"
  line <- getLine
  case parseInput parseCmd line of                 -- parse the input
          Just Quit -> do
            putStrLn "Okay."
            putStrLn "Goodbye."
            return ()
          Just (Num n) -> do
            putStrLn "Added the height of the trees"
            input_num_tree_types num_trees n
          _ -> do
            putStrLn "I'm sorry, I do not understand."
            input_tree_height num_trees

input_num_tree_types :: Int -> Int -> IO ()
input_num_tree_types num_trees tree_height = do
  putStrLn "How many types of trees would you like to be present in the forest?"
  line <- getLine
  case parseInput parseCmd line of                 -- parse the input
          Just Quit -> do
            putStrLn "Okay."
            putStrLn "Goodbye."
            return ()
          Just (Num n) -> do
            putStrLn "Added the number of tree types"
            phase_1 num_trees tree_height n (take n all_tree_types)
          _ -> do
            putStrLn "I'm sorry, I do not understand."
            input_num_tree_types num_trees tree_height

-- phase 1
phase_1 :: Int -> Int -> Int -> [String] -> IO ()
phase_1 num_trees tree_height num_tree_types tree_types = do
    i <- randomRIO(0,19)
    let bird_index = i `mod` num_trees
    -- putStrLn $ show $ bird_index
    let forest = create_forest num_trees tree_types 0
    -- print for debugging:
    -- https://www.reddit.com/r/haskellquestions/comments/jyz1mz/how_do_you_print_in_haskell/
    --putStrLn $ show $ forest
    select_starting_position forest bird_index num_trees tree_height num_tree_types tree_types


select_starting_position :: [Bin_Tree] -> Int -> Int -> Int -> Int -> [String] -> IO ()
select_starting_position forest bird_index num_trees tree_height num_tree_types tree_types = do
  putStr "There are " 
  putStr $ show $ num_trees 
  putStrLn " trees in the forest,"
  putStrLn $ show $ forest
  putStrLn "At which position do you want to start the game?"
  line <- getLine
  case parseInput parseCmd line of                 -- parse the input
          Just Quit -> do
            putStrLn "Okay."
            putStrLn "Goodbye."
            return ()
          Just Hint -> do
              putStrLn "Ok, I can tell you the type of the tree where the bird is: "
              putStr "It is a(n) "
              putStrLn $ show $ tree_type (forest!!(num_trees - bird_index - 1))
              select_starting_position forest bird_index num_trees tree_height num_tree_types tree_types
          Just (Num n)
            |n > num_trees -> do
              putStr "There is no tree with index "
              putStr $ show $ n
              putStrLn " in your forest"
              select_starting_position forest bird_index num_trees tree_height num_tree_types tree_types
            |otherwise -> do
              putStrLn "selected the starting position"
              let user = create_user n 10
              if (tree_index user) - 1 == bird_index
              then do
                putStrLn "Congratulations, you start at the tree with the bird!"
                phase_2 user tree_height
              else do
                putStrLn "This is not the tree with the bird, try to move:"
                loop forest user bird_index num_trees tree_height num_tree_types tree_types
          _ -> do
            putStrLn "I'm sorry, I do not understand."
            select_starting_position forest bird_index num_trees tree_height num_tree_types tree_types
          

loop :: [Bin_Tree] -> User -> Int -> Int -> Int -> Int -> [String] -> IO ()
loop forest user bird_index num_trees tree_height num_tree_types tree_types = do
  case serenity user of
    0 -> do
      putStrLn "Serenity is 0, the bird changed its position, starting over..."
      phase_1 num_trees tree_height num_tree_types tree_types
    x -> do
      let n = length forest
      -- https://stackoverflow.com/questions/27487505/multiline-if-statement-haskell
      if (tree_index user) - 1 == bird_index
      then do
        putStrLn "Congratulations, you found the tree with the bird!"
        phase_2 user tree_height
      else do
        line <- getLine
        case parseInput parseCmd line of                 -- parse the input
                Just Quit -> do
                  putStrLn "Okay."
                  putStrLn "Goodbye."
                  return ()
                Just Hint -> do
                  putStrLn "Ok, I can tell you the type of the tree where the bird is: "
                  putStr "It is a(n) "
                  putStrLn $ show $ tree_type (forest!!(n - bird_index - 1))
                  putStrLn $ show $ forest
                  putStrLn "Move to another tree:"
                  loop forest user bird_index num_trees tree_height num_tree_types tree_types

                Just (Num m)
                  |m > num_trees -> do
                    putStr "There is no tree with index "
                    putStr $ show $ m
                    putStrLn " in your forest"
                    putStrLn $ show $ forest
                    loop forest user bird_index num_trees tree_height num_tree_types tree_types
                  |otherwise  ->
                    let u = (create_user m (x-1)) in
                    if (abs (bird_index - (tree_index user) + 1)) < (abs (bird_index - m + 1))
                    then do 
                      putStrLn $ show $ forest
                      putStrLn "You moved away from the bird, try again:"
                      loop forest u bird_index num_trees tree_height num_tree_types tree_types
                    else
                      if (abs (bird_index - m + 1)) < (abs (bird_index - (tree_index user) +1))
                      then do
                        putStrLn $ show $ forest
                        putStrLn "Wrong tree, but you moved closer to the bird"
                        putStrLn "try again:"
                        -- putStrLn $ show $ serenity u
                        loop forest u bird_index num_trees tree_height num_tree_types tree_types
                      else do
                        putStrLn $ show $ forest
                        putStrLn "Try again:"
                        loop forest u bird_index num_trees tree_height num_tree_types tree_types
                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  loop forest user bird_index num_trees tree_height num_tree_types tree_types
              


phase_2 :: User -> Int -> IO ()
phase_2 user tree_height = do
  my_tree <- placeBird tree_height
  putStrLn "You are at the root of an ancient binary tree."
  putStrLn "Choose an action: climb/go left, climb/go right, go down, rest for _ second(s)"
  make_move user (Hole, my_tree)

make_move :: User -> BinZip -> IO()
make_move user z = do
    --debug : putStrLn "entering make_move with z:"                                                 -- give the player some information
    --debug : print z
    case z of                                        -- about the current position in the tree
      (_,L _)     -> putStrLn "You see a leaf."
      (_,B _ _ _) -> putStrLn "You see a binary node."

    putStr "> "                                      -- print the prompt
    hFlush stdout                                    -- flush standard output
    line <- getLine                                  -- get a line of input
    case parseInput parseCmd line of                 -- parse the input
        Nothing -> do
          putStrLn "I'm sorry, I do not understand."
          make_move user z

        Just Go_Left ->
          case z of
            (c,B b t1 t2) -> do
              check_move user (B0 b c t2,t1)    -- climb up to the left
                      
            (c,L _) -> do
              putStrLn "You cannot climb any further."
              make_move user z

        Just Go_Right ->
          case z of
            (c,B b t1 t2) -> do
              check_move user (B1 t1 b c,t2) -- climb up to the right
                        
            (c,L _) -> do
              putStrLn "You cannot climb any further."
              make_move user z

        Just Go_Down ->
          case z of
            (B0 b c t2,t) -> do
              check_move user (c,B b t t2) -- climb down from the left, or
            
            (B1 t1 b c,t) -> do
              check_move user (c,B b t1 t)  -- climb down from the right, or
                          
            (Hole,_) -> do                           -- already at the root
              putStrLn "You are already at the root."
              putStrLn "You cannot climb down any further."
              make_move user z

        Just (Meditate n) -> do
          putStrLn "You take some rest."
          threadDelay (n * 1000000)
          putStrLn "You have finished re-energizing."
          make_move user z

        Just Quit -> do
          putStrLn "Okay."
          putStrLn "You ended the game over here:\n"
          putStrLn (drawBinZip z)
          putStrLn "Goodbye."
          return ()

        _ -> do
          putStrLn "I'm sorry, I do not understand."
          make_move user z


check_move :: User -> BinZip -> IO()
check_move user z = do
  case z of 
    (_, (L b)) -> do                                 -- moving at a leaf

      case b of
        True -> do
          putStrLn "Congratulations, you caught the bird !"
          putStrLn "You ended the game over here:\n"
          putStrLn (drawBinZip z)
          putStrLn "Goodbye."
          return ()
        False -> do
          putStrLn "The bird is not on this leaf!"
          make_move user z
          

    (_, (B b left right)) -> do                     --moving at a node

      case b of
        True -> do
          let new_user = create_user_p2 (serenity user) 0 
          make_move new_user z
          
        False -> do
          --putStrLn "debug: wrong move"
          --putStrLn $ show $ (serenity user)
          --putStrLn $ show $ (wrong_moves user)
          let new_user = create_user_p2 ((serenity user)-1) (wrong_moves user)
          --putStrLn $ show $ (serenity new_user)
          if (serenity new_user) <= 0
          then do
            putStrLn "You lost, the bird got bored! Let's start again in a new forest."
            input_num_trees
            
          else do
            let new_new_user = create_user_p2 (serenity new_user) ((wrong_moves new_user)+1)
            if (wrong_moves new_new_user) == 3
            then do
              putStrLn "Beware, you have done three wrong consecutive moves!"
              let new_new_new_user = create_user_p2 (serenity new_new_user) 0 
              make_move new_new_new_user z
              
            else do
              make_move new_new_user z
              



main = welcome