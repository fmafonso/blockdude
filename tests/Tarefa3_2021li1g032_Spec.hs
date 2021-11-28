module Tarefa3_2021li1g032_Spec where

import Test.HUnit
import Tarefa3_2021li1g032
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3
import LI12122

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m2e1" ~: "       \n       \n    XC \nPX  C> \nXXX XXX" ~=?  show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2" ~: "       \n    <  \n    X  \nPX  C  \nXXX XXX" ~=?  show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m3e1" ~: "     PX\n    XXX\n>CCC  X\nXXXXXXX" ~=?  show m3e1
    , "Tarefa 3 - Teste Imprime Jogo m3e2" ~: "     PX\n    XXX\n CCC< X\nXXXXXXX" ~=?  show m3e2
    , "Tarefa 3 - Teste Imprime Jogo m5e1" ~: m5s ~=? show m5e1
    , "Tarefa 3 - Teste Imprime Jogo m6e1" ~: m6s ~=?  show m6e1
    , "Tarefa 3 - Teste Imprime Jogo m7e1" ~: m7s ~=?  show m7e1
    , "Tarefa 3 - Teste Imprime Jogo m8e1" ~: m8s ~=?  show m8e1
    , "Tarefa 3 - Teste Imprime Jogo m9e1" ~: m9e1s ~=?  show m9e1
    , "Tarefa 3 - Teste Imprime Jogo m9e2" ~: m9e2s ~=?  show m9e2
    , "Tarefa 3 - Teste Imprime Jogo m10e1" ~: m10s ~=?  show m10e1
    , "Tarefa 3 - Teste Imprime Jogo m11e1" ~: m11s ~=?  show m11e1
    , "Tarefa 3 - Teste Imprime Jogo vazio" ~: "" ~=?  show (Jogo [] (Jogador (0,0) Este False))
    , "Tarefa 3 - Teste Imprime Jogo com Vazio" ~: " " ~=?  show (Jogo [[Vazio]] (Jogador (10,0) Este False))
    , "Tarefa 3 - Teste Imprime Jogo com Jogador" ~: ">" ~=?  show (Jogo [[Vazio]] (Jogador (0,0) Este False))
    , "Tarefa 3 - Teste Imprime Jogo com um elemento" ~: "P" ~=? show (Jogo [[Porta]] (Jogador (10,0) Este False))
    , "Indica a representação de cada peça 1" ~: 'C' ~=? showPeca Caixa
    , "Indica a representação de cada peça 2" ~: 'X' ~=? showPeca Bloco
    , "Indica a representação de cada peça 3" ~: 'P' ~=? showPeca Porta
    , "Indica a representação de cada peça 4" ~: ' ' ~=? showPeca Vazio
    , "Indica a representação do jogador" ~: '<' ~=? showJogador (Jogador (0,0) Oeste False)
    , "Indica a representação do jogador" ~: '>' ~=? showJogador (Jogador (0,0) Este False)
    ]
