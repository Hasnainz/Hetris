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


--According to https://tetris.fandom.com/wiki/Random_Generator
--The way we generate random pieces is to:
--"generates a sequence of all seven one-sided tetrominoes 
--(I, J, L, O, S, T, Z) permuted randomly, as if they were drawn from a bag. 
--Then it deals all seven tetrominoes to the piece sequence before generating another bag"
--
--So we use an infinite list of lists [[I,O,T,S,Z,J,L], [I,O,T,S,Z,J,L], ...] to 
--achieve this making use of haskell's laziness and the below library.
--https://hackage.haskell.org/package/random-shuffle

--An inifinite list of generators using split
--split :: gen0 -> (gen1,gen2)
gens :: StdGen -> [StdGen]
gens gen = gen : gens (snd $ split gen)

--An infinite list of pieces that first zips an infinite list of 
--pieces and infinite list of generators together to get:
--[([I,O,T,S,Z,J,L], gen0),([I,O,T,S,Z,J,L], gen1)...]
--Then it maps it using the shuffle' function to get
--[[Z,J,S,L,I,O,T], ... ]
--Then concats them together so it is one long list
generatepiecelist :: StdGen -> [Tetrimino]
generatepiecelist gen = concat $ map (\(list,rand) -> shuffle' list 7 rand) (zip (repeat [I,O,T,S,Z,J,L]) (gens gen))

--This is the inital state of the game
initialState :: StdGen -> TetrisGame
initialState gen = TetrisGame { 
                 currentPiece    = tetrimino,       --The head of our infinite random list
                 nextPieces      = tail piecelist,  --The tail of our infinite random list
                 currentPiecePos = pos,             --Where the piece will be on the board
                 rotationindex   = 0,               --How far the piece will be rotated
                 currentGrid     = baseGrid,        --The empty grid
                 score           = 0,               --Inital score
                 falling         = True,            --The piece should start by falling
                 ended           = False,           --The game isn't over yet
                 level           = 0,               --We start on level 0
                 ghostpiecepos   = ghostpos,        --The location of the ghost piece
                 lines           = 0,               -- We haven't cleared any lines yet
                 gametimer       = 0,               --The game has had 0 ticks passed
                 heldpiece       = Nothing,         --There is no piece to be swapped in
                 canswap         = True}            --We are allowed to swap pieces
    where 
      piecelist = generatepiecelist gen
      tetrimino = head piecelist
      pos       = getPieceCoords tetrimino (4, 19) 
      ghostpos  = movepieceup baseGrid pos 20


