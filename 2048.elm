import Keyboard
import Random
import Either (..)

startgrid : [[Int]]
startgrid = split 4 <| repeat 16 0

main = flow' . show' <~ foldp step startgrid (input <| merges [init, arrows])

-- layout elements
flow' : [[Element]] -> Element
flow' = flow down . map (flow right)

-- translate grid to graphical elements
show' : [[Int]] -> [[Element]]
show' = map (map box)

box : Int -> Element
box n = colour n <| container 50 50 middle <| plainText <| if n == 0 then " " else show n

-- act on keypress and maybe add a new tile
step : ((Int, Int), (Float, Float)) -> [[Int]] -> [[Int]]
step (keypress, randseed) grid =
 action keypress grid |> either id (addnew randseed)

-- select merge direction based on key input
action : (Int, Int) -> [[Int]] -> Either [[Int]] [[Int]]
action key =
 case key of
  ( -1,  0) -> rows merge                         -- left
  (  0,  1) -> cols merge                         -- up   
  (  1,  0) -> rows <| on merge reverse . reverse -- right
  (  0, -1) -> cols <| on merge reverse . reverse -- down
  (  2,  2) -> Right . id
  _         -> Left . id

merge : [Int] -> Either [Int] [Int]
merge l =
 if all ((==) 0) <| tail l
  then Left l
  else
   case l of
    0 :: t      ->                right' . merge                   <|      t ++ [0]
    a :: 0 :: t ->                right' . merge                   <| a :: t ++ [0]
    a :: b :: t -> if a == b then right' . on merge ((::) (a + a)) <|      t ++ [0]
                             else          on merge ((::) a)       <| b :: t

-- signal grid change so a new tile will spawn
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

-- spawn a new tile in a random place
addnew : (Float, Float) -> [[Int]] -> [[Int]]
addnew (rnd1, rnd2) grid =
 let list = concat grid
     n    = pickrand0 rnd1 list
     tile = if rnd2 > 0.92 then 2 else 1
 in split 4 <| setnth0 n tile list

pickrand0 : Float -> [Int] -> Int
pickrand0 rnd = floor . ((*) rnd) . toFloat . count0s'

count0s' : [Int] -> Int
count0s' = length . (filter ((==) 0))

setnth0 : Int -> Int -> [Int] -> [Int]
setnth0 n y (x::xs) =
 if | n == 0 && x == 0 -> y :: xs
    | n == 0 -> x :: (setnth0 n y xs)
    | True   -> x :: (setnth0 (n - 1) y xs)

split : Int -> [Int] -> [[Int]]
split n l =
 if length l <= n
 then [l]
 else take n l :: (split n <| drop n l)

-- transform input
input : Signal {x:Int, y:Int} -> Signal ((Int, Int), (Float, Float))
input keys = (,) <~ lift (\{x, y} -> (x, y)) keys ~ randgen keys

-- simulate two fake key presses to initialise the grid
init : Signal {x:Int, y:Int}
init = always {x=2,y=2} <~ (dropIf (\x -> x > 2) 0 <| count <| fps 10)

-- drop key up signals 
arrows : Signal {x:Int, y:Int}
arrows = dropIf (\{x,y} -> (x,y) == (0,0)) {x=0,y=1} Keyboard.arrows    

randgen : Signal a -> Signal (Float, Float)
randgen sig = (\[f1,f2] -> (f1,f2)) <~ (Random.floatList <| always 2 <~ sig)

-- colour element with nth colour
colour : Int -> Element -> Element
colour n = color <| head <| drop (logBase 2 <| n + 1) colours

colours =
  [ grey
  , darkGrey
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

transpose : [[Int]] -> [[Int]]
transpose g =
 case g of
  []      -> []
  [] :: t -> transpose t
  _       -> map head g :: (transpose <| map tail g)
