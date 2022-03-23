module Grid where

import System.Random
import qualified Data.Vector as Vector      --How we store the grid for O(1) lookups

--Type alias for Vector of vectors
type Grid = Vector.Vector (Vector.Vector Block)

--We use coordinates [(0,0),(0,1)...] for our renderer to map into a picture
--The blocktype allows us to calculate behaviour like full lines and colours
data Block = Block { coords    :: (Int, Int), -- Fixed coordinates that are transformed into a location by our render function
                     blockType :: Maybe Tetrimino }
             deriving (Show)

-- Enum and bounded are for easy access to Random
data Tetrimino = I | O | T | S | Z | J | L 
  deriving (Show, Enum, Bounded, Eq) 



--Returns true if all blocks are not nothing (they are tetris pieces)
islinefull :: Vector.Vector Block -> Bool
islinefull line = not $ Vector.all (\x -> (blockType x /= Nothing)) line

realign :: Grid -> Grid
realign grid = Vector.fromList [Vector.fromList [Block {coords = (x, y), blockType = (blockType $ (grid Vector.! y) Vector.! x)} | x <- [0..9]] | y <- [0..21] ]
--Filter out all full lines
--let c = the number of lines cleared, 
--Add c lines to the top of the grid
--Realign the grid
clear :: Grid -> (Grid,Int)
clear grid = (alignedgrid, linescleared)
  where
    filteredgrid = Vector.filter islinefull grid
    linescleared = (Vector.length grid) - (Vector.length filteredgrid)
    appendedgrid = filteredgrid Vector.++ (Vector.fromList [Vector.fromList [Block {coords = (x, y), blockType = Nothing} | x <- [0..9]] | y <- [(22-linescleared)..(21)]])
    alignedgrid  = realign appendedgrid

    
--We fold over the 20th row (0 indexed) and if it contains only nothing blocks then the game isn't over
gameover :: Grid -> Bool
gameover grid = not $ Vector.and (Vector.map (\x -> (blockType x) == Nothing ) (grid Vector.! 20))



--Changes a block on the grid
modifyGrid :: (Int,Int) -> Maybe Tetrimino -> Grid -> Grid
modifyGrid (x,y) tetrimino grid = grid Vector.// [(y, (grid Vector.! y) Vector.// [(x, Block {coords = (x, y), blockType = tetrimino})])] 

--Bulk changes blocks on the grid
updateGrid :: Grid -> [(Int,Int)] -> [(Int,Int)] -> Tetrimino -> Grid
updateGrid grid old new piece = addNews grid' new piece
  where
  grid' = removeolds grid old 

--Adds pieces to a grid
addNews :: Grid -> [(Int,Int)] -> Tetrimino -> Grid
addNews grid new piece = foldr (\x -> modifyGrid x (Just piece)) grid new

--Removes pieces from a grid
removeolds :: Grid -> [(Int,Int)] -> Grid
removeolds grid old = foldr (\x -> modifyGrid x Nothing) grid old

--Makes a vector of vectors (10 x 22) where the top 2 tiles are for spawning in blocks
baseGrid :: Grid
baseGrid = Vector.fromList [Vector.fromList [Block {coords = (x, y), blockType = Nothing} | x <- [0..9]] | y <- [0..21] ]
