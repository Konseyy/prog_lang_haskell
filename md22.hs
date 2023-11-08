-- Custom function to map a function over a list
customMap :: (aa -> bb) -> [aa] -> [bb]
customMap _ [] = []
customMap f (x : xs) = f x : customMap f xs

-- Data type to hold a tree where each node has two children
-- Every node can either be None to mark the end of a branch or Some to hold a list of values and two children
data TTT aa = None | Some [aa] (TTT aa) (TTT aa) deriving (Show)

-- Custom function to map a function over a tree
mm :: (aa -> aa) -> TTT aa -> TTT aa
-- If the end of a branch is reached, return None
mm _ None = None
-- If a node is reached, map the function over the list of values and recursively map the function over the children
mm f (Some xs left right) = Some (customMap f xs) (mm f left) (mm f right)

-- This functions squares the input int
a :: Integer -> Integer
a num = num * num

-- This function returns the input int mod 3
b :: Integer -> Integer
b num
  | abs num < 3 = num
  | num < 0 = b (num + 3)
  | otherwise = b (num - 3)

-- This function returns the opposite of the given int
c :: Integer -> Integer
c num = -1 * num

ff_a :: TTT Integer -> TTT Integer
ff_a = mm a

ff_b :: TTT Integer -> TTT Integer
ff_b = mm b

ff_c :: TTT Integer -> TTT Integer
ff_c = mm c

main :: IO ()
main = do
  let tree = Some [0, 1, 2, 3, 4, 5, 6] (Some [2, 3] (Some [4, 5] (Some [6, 7] None None) (Some [8, 9] None None)) (Some [-1, -2] (Some [-3, -4] None None) (Some [-5, -6] None None))) (Some [1, 5] (Some [3, 2] (Some [6, -2] None None) (Some [18, 12339] None None)) (Some [-1, -22] (Some [-3, -44] None None) (Some [7, -6] None None)))
  print tree
  print (ff_a tree)
  print (ff_b tree)
  print (ff_c tree)