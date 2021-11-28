module Tarefa4_2021li1g032_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g032
import Tarefa4_2021li1g032
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    , "Tarefa 4 - Teste Mapa FAQ1" ~: jogoFAQ1AposMov ~=? correrMovimentos jogoFAQ1 movimentosM1
    , "Tarefa 4 - Teste Mapa FAQ2" ~: jogoFAQ2AposMov ~=? correrMovimentos jogoFAQ2 movimentosM2
    , "Tarefa 4 - Teste Mapa FAQ3" ~: jogoFAQ3AposMov ~=? correrMovimentos jogoFAQ3 movimentosM3
    , "Tarefa 4 - Teste m5 não pode Trepar" ~: m5e1 ~=? correrMovimentos m5e1 [Trepar]
    , "Tarefa 4 - Teste m7e1 não pode AndarEsquerda" ~: m7e1 ~=? correrMovimentos m7e1 [AndarEsquerda]
    , "Tarefa 4 - Teste m9e1 não pode Trepar" ~: m9e1 ~=? correrMovimentos m9e1 [Trepar]
    , "Tarefa 4 - Teste m9e2 não pode Trepar" ~: m9e2 ~=? correrMovimentos m9e2 [Trepar]
    , "Tarefa 4 - Teste m10e1 não pode Trepar" ~: m10e1 ~=? correrMovimentos m10e1 [Trepar]
    , "Tarefa 4 - Teste m11e1 pode Trepar" ~: False ~=? m11e1 == correrMovimentos m11e1 [Trepar]
    , "Tarefa 4 - Teste m11e1 não pode AndarEsquerda" ~: m11e1 ~=? correrMovimentos m11e1 [AndarEsquerda]
    , "Tarefa 4 - Teste m12e1 pode Trepar" ~: m12e2 ~=? correrMovimentos m12e1 [Trepar]
    , "Tarefa 4 - Teste m12e1 não pode AndarEsquerda" ~: m12e1 ~=? correrMovimentos m12e1 [AndarEsquerda]
    , "Tarefa 4 - Teste m12e2 não pode AndarEsquerda" ~: m12e2 ~=? correrMovimentos m12e2 [AndarEsquerda]
    , "Tarefa 4 - Teste m13e1 pode InterageCaixa" ~: m14e1 ~=? correrMovimentos m13e1 [InterageCaixa]
    , "Tarefa 4 - Teste m14e1 pode InterageCaixa" ~: m13e1 ~=? correrMovimentos m14e1 [InterageCaixa]
    , "Tarefa 4 - Teste m13e1 não pode AndarDireita" ~: m13e1 ~=? correrMovimentos m13e1 [AndarDireita]
    , "Tarefa 4 - Teste m15e1 não pode InterageCaixa" ~: m15e1 ~=? correrMovimentos m15e1 [InterageCaixa]
    , "Tarefa 4 - Teste m15e1 não pode InterageCaixa" ~: m15e1 ~=? correrMovimentos m15e1 [InterageCaixa]
    , "Tarefa 4 - Teste m16e1 não pode InterageCaixa" ~: m16e1 ~=? correrMovimentos m16e1 [InterageCaixa]
    , "Tarefa 4 - Teste m17e1 não pode InterageCaixa" ~: m17e1 ~=? correrMovimentos m17e1 [InterageCaixa]
    ]