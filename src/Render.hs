module Render where

import Prelude hiding (lines)               --So we can use access lines from TetrisGame
import qualified Data.Vector as Vector      --To access the grid
import Graphics.Gloss                       --For drawing

import TetrisGame
import Grid

--Takes the state of the game and translates into the game
render :: TetrisGame -> Picture
render state = 
        let 
          grid         = pictures [uncurry translate (pairtofloat $ transform $ coords x) $ cell (blockType x) | y <- Vector.toList $ currentGrid state, x <- Vector.toList y]
          textscore    = translate (-242) (160) $ pictures [translate a b $ scale 0.15 0.15 $ text ("Score: " ++ (show $ score state)) | a <- [(-0.5),(0)..0.5], b <- [(-0.5),(0)..0.5]] 
          nexttext     = translate (150) (130) $ pictures [translate a b $ scale 0.15 0.15 $ text ("Next") | a <- [(-0.5),(0)..0.5], b <- [(-0.5),(0)..0.5]] 
          (p1:p2:p3:_) = nextPieces state
          nextpieces   = pictures [uncurry translate (offsetfloat x) (cell (Just piece)) | (piece,offset) <- zip [p3,p2,p1] [-2,1,4], x <- getPieceCoords piece (8,offset)]
          ghostpiece   = pictures [uncurry translate (pairtofloat $ transform $ x) $ ghostcell (currentPiece state) | x <- ghostpiecepos state ]
          leveltext    = translate (-242) (190) $ pictures [translate a b $ scale 0.15 0.15 $ text ("Level: " ++ (show $ level state)) | a <- [(-0.5),(0)..0.5], b <- [(-0.5),(0)..0.5]] 
          linescleared = translate (-242) (220) $ pictures [translate a b $ scale 0.15 0.15 $ text ("Lines Cleared: " ++ (show $ lines state)) | a <- [(-0.5),(0)..0.5], b <- [(-0.5),(0)..0.5]] 
          swaptext     = translate (-202) (130) $ pictures [translate a b $ scale 0.15 0.15 $ text ("Swap") | a <- [(-0.5),(0)..0.5], b <- [(-0.5),(0)..0.5]] 
          swappiece    = pictures [uncurry translate (offsetfloat x) (cell $ heldpiece state) | x <- maybepiececoords (heldpiece state) (-9,4) ]
        in
          pictures [grid,textscore,nexttext,nextpieces,ghostpiece,leveltext,linescleared,swappiece,swaptext]


--Converts our tetrimino into a picture, nothing is an empty cell
cell :: Maybe Tetrimino -> Picture
cell Nothing = pictures [ color grey $ rectangleSolid 20 20, color black $ rectangleSolid 18 18 ]
cell (Just t) = color (getColour t) $ rectangleSolid 18 18

--Gets a ghost coloured rectangle
ghostcell t = color (getghostcol t) $ rectangleSolid 18 18

--Helper functions
pairtofloat :: (Int, Int) -> (Float, Float)
pairtofloat (x,y) = (fromIntegral x, fromIntegral y)

offsetfloat :: (Int, Int) -> (Float, Float)
offsetfloat (x,y) = ((fromIntegral x) * 20, (fromIntegral y)*20)


--The top 2 row of cells are rendered off the screen (just used for loading in blocks)
transform :: (Int, Int) -> (Int, Int)
transform (x,y) 
            | y >= 20   = (-1000, -1000)
            | otherwise = ((x*20) - 100,(y*20) - 240)

--Gets colours
grey :: Color
grey = makeColorI 230 230 230 255

getColour :: Tetrimino -> Color 
getColour t = case t of 
  I -> makeColorI 0 255 255 255
  O -> makeColorI 255 255 0 255
  T -> makeColorI 128 0 128 255
  J -> makeColorI 0 0 255 255
  L -> makeColorI 255 127 0 255
  S -> makeColorI 0 255 0 255
  Z -> makeColorI 255 0 0 255

getghostcol t = case t of 
  I -> makeColorI 0 255 255 150
  O -> makeColorI 255 255 0 150
  T -> makeColorI 128 0 128 150
  J -> makeColorI 0 0 255 150
  L -> makeColorI 255 127 0 150
  S -> makeColorI 0 255 0 150
  Z -> makeColorI 255 0 0 150


