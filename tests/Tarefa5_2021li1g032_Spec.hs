module Tarefa5_2021li1g032_Spec where

import Test.HUnit
import LI12122
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3
import Tarefa5_draw

testsT5 =
  test
    [ "Tarefa 5 - Teste  numEstrelas 0" ~: 0 ~=? numEstrelas Nothing 10
    , "Tarefa 5 - Teste  numEstrelas 1" ~: 1 ~=? numEstrelas (Just 16) 10
    , "Tarefa 5 - Teste  numEstrelas 2" ~: 2 ~=? numEstrelas (Just 15) 10
    , "Tarefa 5 - Teste  numEstrelas 3" ~: 3 ~=? numEstrelas (Just 10) 10
    , "Tarefa 5 - Teste  meioCamera 1" ~: (6,5) ~=? meioCamera ((1,1),(10,10))
    , "Tarefa 5 - Teste  meioCamera 2" ~: (7,6) ~=? meioCamera ((5,3),(8,9))
    , "Tarefa 5 - Teste  subtraiCoordenadas" ~: (3,2) ~=? subtraiCoordenadas (15,13) (12,11)
    , "Tarefa 5 - Teste  estaNaJanela fora cima" ~: False ~=? estaNaJanela (1,0) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela fora cimaEsquerda" ~: False ~=? estaNaJanela (0,0) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela fora esquerda" ~: False ~=? estaNaJanela (0,1) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela fora direita" ~: False ~=? estaNaJanela (6,5) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela fora direitaBaixo" ~: False ~=? estaNaJanela (6,6) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela fora baixo" ~: False ~=? estaNaJanela (5,6) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro cimaEsquerda" ~: True ~=? estaNaJanela (1,1) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro cima" ~: True ~=? estaNaJanela (2,1) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro esquerda" ~: True ~=? estaNaJanela (1,2) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro baixoDireita" ~: True ~=? estaNaJanela (5,5) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro baixo" ~: True ~=? estaNaJanela (4,5) ((1,1),(5,5))
    , "Tarefa 5 - Teste  estaNaJanela dentro diretia" ~: True ~=? estaNaJanela (5,4) ((1,1),(5,5))
    , "Tarefa 5 - Teste  calculaCamera 1" ~: ((3,0),(5,2)) ~=? calculaCamera (5,5) (3,3) (4,1)
    , "Tarefa 5 - Teste  calculaCamera 2" ~: ((0,0),(2,2)) ~=? calculaCamera (5,5) (3,3) (1,1)
    , "Tarefa 5 - Teste  calculaCamera 3" ~: ((0,3),(2,5)) ~=? calculaCamera (5,5) (3,3) (1,4)
    , "Tarefa 5 - Teste  calculaCamera 4" ~: ((3,3),(5,5)) ~=? calculaCamera (5,5) (3,3) (4,4)
    , "Tarefa 5 - Teste  calculaCamera 5" ~: ((0,0),(2,2)) ~=? calculaCamera (5,5) (3,3) (0,0)
    , "Tarefa 5 - Teste  calculaCamera 6" ~: ((3,0),(5,2)) ~=? calculaCamera (5,5) (3,3) (5,0)
    , "Tarefa 5 - Teste  calculaCamera 7" ~: ((3,3),(5,5)) ~=? calculaCamera (5,5) (3,3) (5,5)
    , "Tarefa 5 - Teste  calculaCamera 8" ~: ((0,3),(2,5)) ~=? calculaCamera (5,5) (3,3) (0,5)
    , "Tarefa 5 - Teste  calculaCamera 9" ~: ((1,1),(4,4)) ~=? calculaCamera (5,5) (4,4) (3,2)
    , "Tarefa 5 - Teste  calculaAlgarismos 1" ~: [1,0] ~=? calculaAlgarismos 10
    , "Tarefa 5 - Teste  calculaAlgarismos 2" ~: [1] ~=? calculaAlgarismos 01
    , "Tarefa 5 - Teste  calculaAlgarismos 3" ~: [1] ~=? calculaAlgarismos 1
    , "Tarefa 5 - Teste  calculaAlgarismos 4" ~: [0] ~=? calculaAlgarismos 0
    ]
