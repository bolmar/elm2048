import Keyboard
import Random
import Either (..)

startgrid : [[Int]]
startgrid = addnew 5 <| addnew 10 <| split 4 <| repeat 16 0
-- split 4 <| repeat 16 0

main = flow' . show' <~ foldp folder startgrid (input arrows)

flow' : [[Element]] -> Element
flow' = flow down . map (flow right)

show' : [[Int]] -> [[Element]]
show' = map (map box)

box : Int -> Element
box n = colour n <| container 50 50 middle <| plainText <| if n == 0 then " " else show n

folder : ((Int, Int), Float) -> [[Int]] -> [[Int]]
folder (keypress, randseed) grid =
 either id (addnew' randseed) <| action keypress grid

merge : [Int] -> Either [Int] [Int]
merge l =
 if all (\e -> e == 0) <| tail l
  then Left l
  else
   case l of
    0 :: t       -> either Right Right <| merge <| t ++ [0]
    a :: 0 :: t  -> either Right Right <| merge <| a :: t ++ [0]
    a :: b :: t  -> if a == b then either (Right . (::) (a + a)) (Right . (::) (a + a)) <| merge <| t ++ [0]
                              else either (Left . ((::) a)) (Right . ((::) a)) <| merge <| b :: t

egrem : [Int] -> Either [Int] [Int]
egrem = (either (Left . reverse) (Right . reverse)) . merge . reverse

cols : ([Int] -> Either [Int] [Int]) -> [[Int]] -> Either [[Int]] [[Int]]
cols mergef g =
 case g of
  [] :: _ -> Left g
  g -> let heads = map head g
           tails = map tail g
           meads = mergef heads
       in ezip meads <| cols mergef tails

ezip : Either [Int] [Int] -> Either [[Int]] [[Int]] -> Either [[Int]] [[Int]]
ezip meads mails =
 case either id id meads of
  [] -> Left []
  _  ->
   if isRight meads || isRight mails
    then Right <| zipWith (::) (either id id meads) (either id id mails)
    else Left  <| zipWith (::) (either id id meads) (either id id mails)

rows : ([Int] -> Either [Int] [Int]) -> [[Int]] -> Either [[Int]] [[Int]]
rows mergef = reduce . map mergef

reduce : [Either a a] -> Either [a] [a]
reduce l = if any isRight l then Right <| strip l
                            else Left  <| strip l

strip = map <| either id id


action : (Int, Int) -> [[Int]] -> Either [[Int]] [[Int]]
action key =
 case key of
  ( -1,  0) -> rows merge -- left
  (  0,  1) -> cols merge -- up
  (  1,  0) -> rows egrem -- right
  (  0, -1) -> cols egrem -- down
  _ -> Left . id

count0s : [[Int]] -> Int
count0s = foldl (\a b -> foldl (+) b (map (\n -> if n == 0 then 1 else 0) a)) 0

select0 : Float -> [[Int]] -> Int
select0 fl g = round <| fl * (toFloat <| (count0s g) - 1)

addnew' : Float -> [[Int]] -> [[Int]]
addnew' randseed grid =
 let index = select0 randseed grid
     n     = if randseed > 0.9 then 4 else 2
 in addnew'' index n grid

addnew'' index n g = split 4 <| setnth index 0 n <| concat g

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
  , darkOrange
  ] ++ repeat 10 grey
