import Keyboard
import Random
import Either (..)

startgrid : [[Int]]
startgrid = addnew 5 <| addnew 10 <| split 4 <| repeat 16 0
-- split 4 <| repeat 16 0

main = flow' . show' <~ foldp folder startgrid (input arrows)

-- layout
flow' : [[Element]] -> Element   
flow' = flow down . map (flow right)

-- "print"
show' : [[Int]] -> [[Element]]
show' = map (map box)

box : Int -> Element
box n = colour n <| container 50 50 middle <| plainText <| if n == 0 then " " else show n

-- act on keypress and maybe add a new tile
folder : ((Int, Int), Float) -> [[Int]] -> [[Int]]
folder (keypress, randseed) grid =
 action keypress grid |> either id (addnew' randseed)


merge : [Int] -> Either [Int] [Int]
merge l =
 if all (\e -> e == 0) <| tail l
  then Left l
  else
   case l of
    0 :: t      -> right' <| merge <| t ++ [0]
    a :: 0 :: t -> right' <| merge <| a :: t ++ [0]
    a :: b :: t -> if a == b then right' . on merge ((::) (a + a)) <| t ++ [0]
                             else          on merge ((::) a)       <| b :: t

-- grid changed
right' = either Right Right

-- merge left/right
rows : ([Int] -> Either [Int] [Int]) -> [[Int]] -> Either [[Int]] [[Int]]
rows mergef = reduce . map mergef 

-- merge up/down
cols : ([Int] -> Either [Int] [Int]) -> [[Int]] -> Either [[Int]] [[Int]]
cols mergef = on (rows mergef) transpose . transpose

-- basically lift . into either
on : (a -> Either b b) -> (b -> c) -> a -> Either c c
on f g = (either (Left . g) (Right . g)) . f

-- combine?
reduce : [Either a a] -> Either [a] [a]
reduce l = if any isRight l then Right <| map strip l
                            else Left  <| map strip l

-- escape from eitherland
strip = either id id

transpose : [[Int]] -> [[Int]]
transpose g =
 case g of
  []      -> []
  [] :: t -> transpose t
  _       -> map head g :: (transpose <| map tail g)

-- select merge direction based on key input
action : (Int, Int) -> [[Int]] -> Either [[Int]] [[Int]]
action key =
 case key of
  ( -1,  0) -> rows merge                         -- left
  (  0,  1) -> cols merge                         -- up
  (  1,  0) -> rows <| on merge reverse . reverse -- right
  (  0, -1) -> cols <| on merge reverse . reverse -- down
  _         -> Left . id

count0s : [[Int]] -> Int
count0s = length . (filter (\x -> x == 0)) . concat

select0 : Float -> [[Int]] -> Int
select0 fl g = round <| fl * (toFloat <| (count0s g) - 1)

addnew' : Float -> [[Int]] -> [[Int]]
addnew' randseed grid =
 let index = select0 randseed grid
     x     = if randseed > 0.92 then 4 else 2
 in split 4 <| setnth index 0 x <| concat grid

addnew : Int -> [[Int]] -> [[Int]]
addnew n g = split 4 <| setnth n 0 2 <| concat g

split : Int -> [Int] -> [[Int]]
split n l = if length l <= n then [l] else take n l :: (split n <| drop n l)

setnth n a b (x::xs) =
 if n == 0
 then if x == a then b :: xs else x :: (setnth n a b xs)
 else x :: (setnth (n - 1) a b xs)

input : Signal {x:Int, y:Int} -> Signal ((Int, Int), Float)
input keys = (,) <~ lift (\{x, y} -> (x, y)) keys ~ Random.float keys

-- drop key up signals 
arrows : Signal {x:Int, y:Int}
arrows = dropIf (\{x,y} -> (x,y) == (0,0)) {x=0,y=1} Keyboard.arrows

colour : Int -> Element -> Element
colour n = color <| head <| drop (logBase 2 n) colours

colours =
  [ grey
  , lightBlue
  , lightGreen
  , lightYellow
  , lightOrange
  , lightPurple
  , darkBlue
  , darkGreen
  , darkYellow
  , darkOrange
  , darkBrown
  , darkRed
  , darkPurple
  ] ++ repeat 10 grey
