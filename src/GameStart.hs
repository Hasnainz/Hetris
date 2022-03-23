module GameStart (entry) where

import Debug.Trace

import Prelude hiding (lines)               --So we can use lines as a function name
import System.Random                        --Randomly generating tetriminoes
import qualified Data.Vector as Vector      --How we store the grid for O(1) lookups
import Graphics.Gloss                       --For the game
import Graphics.Gloss.Interface.IO.Interact --For keypress events

import Render
import TetrisGame
import Movement
import Grid

window :: Display
window = InWindow "Tetris" (500, 520) (2500, 200)

fps :: Int
fps = 120

entry :: IO ()
entry = do 
  gen <- getStdGen
  play window white fps (initialState gen) render handleKeys updateGame

updateGame :: Float -> TetrisGame -> TetrisGame
updateGame _ game
                | ended game          = game
                | not $ falling game' = updateNotFalling game'
                | not $ doupdate t l  = game'
                | otherwise           = updateFalling game'
                where
                  t     = (gametimer game) + 1
                  l     = level game
                  game' = game { gametimer = t }


updateNotFalling :: TetrisGame -> TetrisGame
updateNotFalling game = game { currentPiece    = piece,
                               nextPieces      = nextpieces,
                               currentPiecePos = pos, 
                               rotationindex   = 0,
                               falling         = True,
                               ended           = finished,
                               currentGrid     = grid',
                               level           = getlevel totallines,
                               lines           = totallines,
                               score           = (score game) + calcscore (level game) cleared,
                               ghostpiecepos   = ghostpos, 
                               canswap         = True }
                 where
                  finished        = gameover $ currentGrid game
                  piece           = head (nextPieces game)
                  nextpieces      = tail (nextPieces game)
                  (grid',cleared) = clear (currentGrid game)
                  totallines      = (lines game) + cleared
                  pos             = getPieceCoords piece (4, 20)
                  ghostpos        = movepieceup grid' pos 20

updateFalling :: TetrisGame -> TetrisGame
updateFalling game = game {  currentPiecePos    = newpos', 
                             currentGrid        = newgrid, 
                             falling            = fall,
                             ghostpiecepos      = ghostpos }
          where
             newpos   = movepiecedown (currentGrid game) (currentPiecePos game)
             fall     = newpos /= (currentPiecePos game)
             newpos'  = if fall then newpos else (currentPiecePos game)
             newgrid  = updateGrid (currentGrid game) (currentPiecePos game) newpos' (currentPiece game)
             ghostpos = movepieceup newgrid newpos' 20 


--Handles movement
handleKeys :: Event -> TetrisGame -> TetrisGame

--Left Arrow -> Move Left
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game  = handlemove game (movepieceleft (currentGrid game) 
                                                            (currentPiecePos game)) (rotationindex game) 0
--Right Arrow -> Move Right
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = handlemove game (movepieceright (currentGrid game) 
                                                            (currentPiecePos game)) (rotationindex game) 0
--Down Arrow -> Move Down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game  = handlemove game (movepiecedown (currentGrid game) 
                                                            (currentPiecePos game)) (rotationindex game) 2
--Up Arrow -> Instant Down
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game    = handlemove game (movepieceup (currentGrid game) 
                                                            (currentPiecePos game) 20) (rotationindex game) 1


-- z -> Rotate 90 degrees clockwise
handleKeys (EventKey (Char 'x') Down _ _) game = handlemove game rotated rindex 0
  where 
    rotated = rotatepiece (currentGrid game) (currentPiecePos game) 90 (currentPiece game) (rotationindex game)
    --If the rotated position is different than the current position that means that the rotation has been successful and so we should update the rotation index
    rindex  = if   (rotated /= (currentPiecePos game)) 
              then (rotationindex game) + 1  
              else (rotationindex game)



-- x -> Rotate by 270 degrees clockwise
handleKeys (EventKey (Char 'z') Down _ _) game = handlemove game rotated rindex 0
  where 
    rotated = rotatepiece (currentGrid game) (currentPiecePos game) 270 (currentPiece game) (rotationindex game)
    rindex  = if   (rotated /= (currentPiecePos game)) 
              then (rotationindex game) - 1  
              else (rotationindex game)

-- c -> Swap piece between old piece and new piece
handleKeys (EventKey (Char 'c') Down _ _) game = handleswap game

--Any other key doesn't do anything
handleKeys _ game = game 

--Gives the user a hold piece, doesn't work if the swap
--spot is occupied and the swap spot is always is in the
--same location
handleswap :: TetrisGame -> TetrisGame
handleswap game = if (canswap game && isvalid) 
                  then game { 
                    currentPiece    = swappedpiece, 
                    nextPieces      = nextpieces,
                    currentPiecePos = nextpos,
                    currentGrid     = newgrid,
                    rotationindex   = 0,
                    ghostpiecepos   = movepieceup newgrid nextpos 20,
                    canswap         = False,
                    falling         = True,
                    heldpiece       = Just $ currentPiece game }
                  else game
        where
          swappedpiece = case (heldpiece game) of
                          Nothing -> head (nextPieces game)
                          Just x  -> x

          nextpos      = getPieceCoords swappedpiece (4,19)
          isvalid      = validmove (currentGrid game) (currentPiecePos game) nextpos

          nextpieces   = case (heldpiece game) of
                          Nothing -> tail (nextPieces game)
                          Just x  -> nextPieces game
          newgrid      = updateGrid (currentGrid game) (currentPiecePos game) (nextpos) swappedpiece


--Checks if falling before we make a move so that we dont move the piece while it is being locked
--The lock tells us if we should stop falling after this move (on an instant down move)
handlemove :: TetrisGame -> [(Int,Int)] -> Int -> Int -> TetrisGame 
handlemove game nextpiecepos rindex n = if (falling game) 
                                        then game { 
                                               currentPiecePos = nextpiecepos, 
                                               currentGrid     = newgrid, 
                                               rotationindex   = rindex `mod` 4,
                                               ghostpiecepos   = movepieceup newgrid nextpiecepos 20,
                                               falling         = if n == 1 then False else True,
                                               score           = case n of 
                                                                 2 -> (score game) + 1
                                                                 1 -> (score game) + 2 * (linesfallen)
                                                                 _ -> (score game) }
                                        else game
  where 
    newgrid     = updateGrid (currentGrid game) (currentPiecePos game) (nextpiecepos) (currentPiece game)
    (a,b)       = head (currentPiecePos game)
    (c,d)       = head nextpiecepos
    linesfallen = b-d


--Is the standard score calculator based on how many lines are cleared
calcscore :: Int -> Int -> Int
calcscore level 0 = 0
calcscore level 1 = 40 * (level+1)
calcscore level 2 = 100 * (level+1)
calcscore level 3 = 300 * (level+1)
calcscore level 4 = 1200 * (level+1)

--Level is the first digit of how many lines
--you have cleared. e.g 
--04 -> level 0
--10 -> level 1
--29 -> level 2
getlevel :: Int -> Int
getlevel n = (n `div` 10)

--Should we update the game based on the ingame timer and 
--the level. For example we want to update the game 
--4 times a second for level 1. So 120/4 = 30
leveltoframes :: Int -> Integer
leveltoframes 0 = 40
leveltoframes 1 = 30
leveltoframes 2 = 25
leveltoframes 3 = 20
leveltoframes 4 = 15
leveltoframes 5 = 10
leveltoframes 6 = 5
leveltoframes 7 = 4
leveltoframes 8 = 3
leveltoframes _ = 2

--Check if we need to update the frame
doupdate :: Integer -> Int -> Bool 
doupdate t l = (t `mod` (leveltoframes l)) == 0

