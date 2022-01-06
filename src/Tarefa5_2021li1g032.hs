{- |
Module      : Tarefa5_2021li1g032
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game

import LI12122
import Tarefa5_draw
import Tarefa5_events
import Tarefa5_io
import Tarefa5_types

fr :: Int
fr = 25

time :: Float -> BlockDude -> IO BlockDude
time _ w = return w

background :: BlockDude -> Color
background (janela, niveis, (texturas, tID)) = color
    where (_, color) = texturas !! tID
-- makeColorI 109 174 255 255

main :: IO ()
main = do
    dimensoes <- getScreenSize
    blockdude <- loadBlockDude
    if (blockdude == Nothing)
        then return undefined
        else playIO (window dimensoes) (background (fromJust blockdude)) fr (fromJust blockdude) draw event time
