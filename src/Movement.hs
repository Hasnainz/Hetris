module Movement where

import Grid
import qualified Data.Vector as Vector      --How we store the grid for O(1) lookups

--Does the rotation for any (x,y) using matrix multiplication
--We first translate to (x-a, y-b) where (a,b) is the centre square of the piece
--Then we multiply by matrix 
-- | 0 1 |
-- |-1 0 |
-- because this is the rotation matrix 
-- when we plug in theta = pi/2
-- giving us (y-b, a-x)
-- Then we add back on the original translation
-- (y-b+a,a-x+b) which is our formula for rotation
tetrisrotate90 ::(Int,Int) -> (Int,Int) -> (Int,Int)
tetrisrotate90 (a,b) (x,y) = (y-b+a,a-x+b)

--We rotate by 270 instead using the same method as above
tetrisrotate270 ::(Int,Int) -> (Int,Int) -> (Int,Int)
tetrisrotate270 (a,b) (x,y) = (b-y+a,x-a+b)

--Returns the 5 different offsets available when rotating the I piece
--https://harddrop.com/wiki/SRS#Wall_Kicks
offset :: Tetrimino -> Int -> [(Int,Int)] 
offset I 0 = [(0,0),(-1,0),(2,0),(-1,0),(2,0)]
offset I 1 = [(-1,0),(0,0),(0,0),(0,1),(0,-2)]
offset I 2 = [(-1,1),(1,1),(-2,1),(1,0),(-2,0)]
offset I 3 = [(0,1),(0,1),(0,1),(0,-1),(0,2)]

--The offsets for all other pieces, the O piece we skip the rotate earlier
offset _ 0 = [(0,0),(0,0),(0,0),(0,0),(0,0)]
offset _ 1 = [(0,0),(1,0),(1,-1),(0,2),(1,2)]
offset _ 2 = [(0,0),(0,0),(0,0),(0,0),(0,0)]
offset _ 3 = [(0,0),(-1,0),(-1,-1),(0,2),(-1,2)]

--Helper functions to add and subtract pairs
pairsum :: Num n => (n,n) -> (n,n) -> (n,n)
pairsum (a,b) (c,d) = (a+c,b+d)

pairsub :: Num n => (n,n) -> (n,n) -> (n,n)
pairsub (a,b) (c,d) = (a-c,b-d)

--This calculates the offset required when rotating pieces that 
wallkick :: Grid -> [(Int,Int)] -> [(Int,Int)] -> Tetrimino -> Int -> Int -> [(Int,Int)]
wallkick grid oldloc rotatedloc piece oldrotidx newrotidx
  | validmove grid oldloc test1 = test1
  | validmove grid oldloc test2 = test2
  | validmove grid oldloc test3 = test3
  | validmove grid oldloc test4 = test4
  | validmove grid oldloc test5 = test5
  | otherwise                   = oldloc
  where
    (a:b:c:d:e:_) = zipWith pairsub (offset piece oldrotidx) (offset piece newrotidx)
    test1 = map (pairsum a) rotatedloc
    test2 = map (pairsum b) rotatedloc
    test3 = map (pairsum c) rotatedloc
    test4 = map (pairsum d) rotatedloc
    test5 = map (pairsum e) rotatedloc

--The grid, the current piece position, the rotation amount, the piece type, the rotation index of the piece
rotatepiece :: Grid -> [(Int,Int)] -> Int -> Tetrimino -> Int -> [(Int, Int)]
--We don't want to rotate the O piece so we don't bother with any calculations.
rotatepiece _ currentpos _ O _ = currentpos
--Based on the Tetris SRS system we need to allow for offsets after rotation 
--https://tetris.fandom.com/wiki/SRS
rotatepiece grid (x:xs) 90 piece rindex = offset
  where 
    rotated = map (tetrisrotate90 x) (x:xs) --Matrix rotation
    offset  = wallkick grid (x:xs) rotated  piece (rindex) ((rindex+1) `mod` 4 )

--Same logic as above but with different location
rotatepiece grid (x:xs) 270 piece rindex = offset
  where 
    rotated = map (tetrisrotate270 x) (x:xs) --Matrix rotation
    offset  = wallkick grid (x:xs) rotated  piece (rindex) ((rindex-1) `mod` 4 )

movepieceleft grid xs = movepiece grid xs (map (\(x,y) -> (x-1,y)) xs)
movepieceright grid xs = movepiece grid xs (map (\(x,y) -> (x+1,y)) xs)
movepiecedown grid xs = movepiece grid xs (map (\(x,y) -> (x,y-1)) xs)

--Up refers to how the keypress up will move the piece all the way to the bottom
movepieceup :: Grid -> [(Int,Int)] -> Int -> [(Int,Int)]
movepieceup grid xs 0 = xs 
movepieceup grid xs n = movepieceup grid (movepiecedown grid xs) (n-1)

--Takes the grid and the before after positions and then returns the 
--valid position (trys the after one and if it is invalid then the before one)
movepiece :: Grid -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
movepiece grid xs xs' = if (validmove grid xs xs') 
                        then xs' 
                        else xs

--The spot is valid in the grid if it is a Nothing tile and in bounds
valid :: Grid -> (Int,Int) -> Bool
valid grid (x,y) | x < 0     = False
                 | x > 9     = False
                 | y < 0     = False
                 | y > 20    = False
                 | otherwise = case (blockType $ (grid Vector.! y) Vector.! x) of
                               Nothing -> True
                               _       -> False

--Checks if all tiles are valid
validmove :: Grid -> [(Int,Int)] -> [(Int,Int)] -> Bool
validmove grid prev next = and (map (valid grid') next)
  where grid' = removeolds grid prev 


