{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE ParallelListComp #-}

import Control.Monad
import Data.List hiding (union)
import Data.Vector ( (!) )
import qualified Data.Vector as V
import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import System.Directory
import System.FilePath

data Cell = Empty | Black | White deriving (Show,Eq)

type Board = V.Vector (V.Vector Cell)

data Player = B | W deriving (Show,Eq)

type Pos = (Int,Int)

data Move = Move { moveFrom :: [Pos], moveTo :: Pos }

data Game = Game { turn :: Player, board :: Board, selected :: [Pos]  }

height, width :: Int
height   = 600
width    = 600

spriteEmpty, spriteWhite, spriteBlack :: Bitmap ()
spriteEmpty = bitmap "empty.png"
spriteWhite = bitmap "white.png"
spriteWhiteSel = bitmap "white_sel.png"
spriteBlack = bitmap "black.png"
spriteBlackSel = bitmap "black_sel.png"

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


newGame :: Game
newGame = Game B startBoard []

xMargin s = bitmapWidth + (9-s)*(div bitmapWidth 2)
yMargin = bitmapHeight

rowSizes = [5,6,7,8,9,8,7,6,5]

posX s col = xMargin s + (col*bitmapWidth)

genRow row s = zipWith point (map (posX s) [0..(s-1)]) (replicate s (yMargin + row*bitmapHeight))

positions = V.fromList $
             map V.fromList [genRow row size| row<-[0..8] | size <-rowSizes ]

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
        let
          eclick = filterE isClick emouse

          getClickPoint (MouseLeftDown pt _) = pt

          moves :: Event t (Game -> Game)
          moves =  selectMoveCell <$> getClickPoint <$> eclick

          state :: Behavior t Game
          state = accumB newGame moves

          onPaint :: Game -> DC a -> b -> IO ()
          onPaint game dc _viewArea = drawBoard game dc _viewArea
  
        sink pp [on paint :== onPaint <$> state ]
        reactimate $ repaint pp <$ etick

  network <- compile networkDescription
  actuate network



selectMoveCell :: Point -> Game -> Game
selectMoveCell p game | getCell p == Nothing = game
                      | otherwise =
                        let pos = fromJust $ getCell p
                            sel = selected game
                            b = board game
                        in if b !(fst pos)!(snd pos) /= Empty then
                             if elem pos sel then
                               game {selected = delete pos sel}
                             else
                               game {selected = pos:sel}
                           else
                             game {- TODO: move -}


getCell :: Point -> Maybe Pos
getCell (Point x y) | y < yMargin || y > yMargin + 9*bitmapHeight = Nothing
                    | otherwise = let row = div (y - yMargin) bitmapHeight
                                      xs = rowSizes!!row
                                      xm = xMargin xs
                                  in if x < xm || x > xm + xs*bitmapWidth then
                                       Nothing
                                     else
                                       Just (row,div (x - xm) bitmapWidth)


createMenu :: Frame () -> IO ()
createMenu window = do
  game  <- menuPane      [ text := "Game" ]
  new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
  menuLine game
  quit  <- menuQuit game [help := "Quit the game"]

  set new   [on command := close window >> abalone]
  set quit  [on command := close window]

  set window [menuBar := [game]]

drawBoard :: Game -> DC a -> b -> IO ()
drawBoard game dc _ = do
  let b = board game
  let sel = selected game
  let drawCell row (col,cell) = drawBitmap dc sprite p True []
        where sprite = case cell of
                Empty -> spriteEmpty
                Black -> if elem (row,col) sel then spriteBlackSel else spriteBlack
                White -> if elem (row,col) sel then spriteWhiteSel else spriteWhite
              p = (positions ! row) ! col
  let drawRow (row,l) = V.mapM_ (drawCell row) l
  let idxBoard = V.indexed $ V.map V.indexed b
  V.mapM_ drawRow idxBoard

isClick :: EventMouse -> Bool
isClick (MouseLeftDown pt _) = True
isClick _                    = False
