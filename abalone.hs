{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import Data.Array
import Data.List hiding (union)
import qualified Data.Vector as V
import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import System.Directory
import System.FilePath

data Cell = Empty | Black | White deriving Show

type Board = V.Vector (V.Vector Cell)

height, width :: Int
height   = 600
width    = 600

spriteEmpty, spriteWhite, spriteBlack :: Bitmap ()
spriteEmpty = bitmap "empty.png"
spriteWhite = bitmap "white.png"
spriteBlack = bitmap "black.png"

bitmapWidth, bitmapHeight :: Int
bitmapWidth  = 50
bitmapHeight = 50

startBoard :: Board
startBoard = V.fromList $
             map V.fromList [replicate 5 White,
                             replicate 6 White,
                             [Empty,Empty,White,White,White,Empty,Empty],
                             replicate 8 Empty,
                             replicate 9 Empty,
                             replicate 8 Empty,
                             [Empty,Empty,Black,Black,Black,Empty,Empty],
                             replicate 6 Black,
                             replicate 5 Black
                            ]

posX s col = (9-s)*(div bitmapWidth 2)+(col*bitmapWidth)

genRow row s = zipWith point (map (posX s) [1..s]) (replicate s (bitmapHeight+row*bitmapHeight)) 

positions = V.fromList $
             map V.fromList [genRow 0 5,
                             genRow 1 6,
                             genRow 2 7,
                             genRow 3 8,
                             genRow 4 9,
                             genRow 5 8,
                             genRow 6 7,
                             genRow 7 6,
                             genRow 8 5
                            ]


main :: IO ()
main = start abalone

abalone :: IO ()
abalone = do
  window <- frame [ text       := "Abalone"
                  , bgcolor    := white ]
  status <- statusField [text := "Welcome to abalone"] 
  set window [statusBar := [status]] 

  t  <- timer window [ interval   := 50 ]

  createMenu window

  pp <- panel window []
  set window [ layout  := minsize (sz width height) $ widget pp ]

  let networkDescription :: forall t. Frameworks t => Moment t ()
      networkDescription = do
        etick  <- event0 t command  -- frame timer
        emouse <- event1 pp mouse   -- mouse events

        let onPaint dc _viewArea = drawBoard startBoard dc _viewArea
  
        sink pp [on paint :== pure onPaint  ]
        reactimate $ repaint pp <$ etick

  network <- compile networkDescription    
  actuate network


createMenu :: Frame () -> IO ()
createMenu window = do
  game  <- menuPane      [ text := "Game" ] 
  new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
  menuLine game
  quit  <- menuQuit game [help := "Quit the game"] 
	
  set new   [on command := close window >> abalone] 
  set quit  [on command := close window]

  set window [menuBar := [game]]

drawBoard :: Board -> DC a -> b -> IO ()
drawBoard board dc _ = do
  let drawCell row (col,cell) = drawBitmap dc sprite p True []
        where sprite = case cell of
                Empty -> spriteEmpty
                Black -> spriteBlack
                White -> spriteWhite
              p = (positions V.! row) V.! col
  let drawRow (row,l) = V.mapM_ (drawCell row) l 
  let idxBoard = V.indexed $ V.map V.indexed board
  V.mapM_ drawRow idxBoard 



