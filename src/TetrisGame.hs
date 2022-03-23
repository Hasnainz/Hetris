module TetrisGame where

import System.Random                        --Randomly generating tetriminoes
import System.Random.Shuffle                --For shuffling the list (reasoning is below)
import qualified Data.Vector as Vector      --How we store the grid for O(1) lookups
import Prelude hiding (lines)
import Movement (movepieceup)
import Grid

data TetrisGame = TetrisGame { 
    currentPiece     :: Tetrimino,       --The type of the current piece
    nextPieces       :: [Tetrimino],     --The infinite list of types of the next pieces      
    currentPiecePos  :: [(Int, Int)],    --Where the piece is located on the grid
    rotationindex    :: Int,             --How many turns is the piece rotated (0->3)
    currentGrid      :: Grid,            --The current state of the grid (doesn't include the piece until landed)
    score            :: Int,             --The score from the number of lines cleared.
    falling          :: Bool,            --Is the current block falling (if not we move to the next piece)
    ended            :: Bool,            --Is the game over? (if so we stop spawning in new pieces)
    level            :: Int,             --How many points is a line clear worth and how fast is the game
    lines            :: Int,             --How many lines have been cleared 
    ghostpiecepos    :: [(Int,Int)],     --Where is the ghostpiece located
    gametimer        :: Integer,         --Game timer -> allows us to speed up levels
    heldpiece        :: Maybe Tetrimino, --Which piece is being held
    canswap          :: Bool             --Can only swap once per "turn"
  }
  deriving(Show)


--Returns an array of (Int,Int) which map to all the blocks that
--the tetrimino takes up based on where it's centre is.
getPieceCoords :: Tetrimino -> (Int, Int) -> [(Int, Int)]
getPieceCoords piece (x,y)
  | piece == I = [(x,y),(x+2,y),(x+1,y),(x-1,y)]
  | piece == O = [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]
  | piece == S = [(x,y),(x,y+1),(x-1,y),(x+1,y+1)]
  | piece == T = [(x,y),(x-1,y),(x+1,y),(x,y+1)]
  | piece == Z = [(x,y),(x,y+1),(x-1,y+1),(x+1,y)]
  | piece == L = [(x,y),(x+1,y),(x-1,y),(x+1,y+1)]
  | piece == J = [(x,y),(x+1,y),(x-1,y),(x-1,y+1)]
  | otherwise  = []

maybepiececoords :: Maybe Tetrimino -> (Int, Int) -> [(Int, Int)]
maybepiececoords (Just piece) a = getPieceCoords piece a
maybepiececoords Nothing _ = []


--This is the inital state of the game
--The only interesting thing here is the random 
--list of pieces, everything else is fairly
--self-explainatory
--
--According to https://tetris.fandom.com/wiki/Random_Generator
--The way we generate random pieces is to:
--"generates a sequence of all seven one-sided tetrominoes 
--(I, J, L, O, S, T, Z) permuted randomly, as if they were drawn from a bag. 
--Then it deals all seven tetrominoes to the piece sequence before generating another bag"
--
--So we use an infinite list of lists [[I,O,T,S,Z,J,L], [I,O,T,S,Z,J,L], ...] to 
--achieve this making use of haskell's laziness and the below library.
--https://hackage.haskell.org/package/random-shuffle
initialState :: StdGen -> TetrisGame
initialState gen = TetrisGame { 
                 currentPiece    = tetrimino, 
                 nextPieces      = tail piecelist,
                 currentPiecePos = pos,
                 rotationindex   = 0,
                 currentGrid     = baseGrid,
                 score           = 0,
                 falling         = True,
                 ended           = False,
                 level           = 0,
                 ghostpiecepos   = ghostpos,
                 lines           = 0,
                 gametimer       = 0,
                 heldpiece       = Nothing,
                 canswap         = True     }
    where 
      piecelist = concat $ map (\x -> shuffle' x 7 gen) (repeat ([I,O,T,S,Z,J,L]))
      tetrimino = head piecelist
      pos       = getPieceCoords tetrimino (4, 19) 
      ghostpos  = movepieceup baseGrid pos 20


