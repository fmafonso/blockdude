module Tarefa2_2021li1g032_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g032
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Construir Mapa mapaFAQ1" ~: mapaFAQ1r ~=? constroiMapa mapaFAQ1
    , "Tarefa 2 - Teste Desconstruir Mapa mapaFAQ1r" ~: mapaFAQ1 ~=? desconstroiMapa mapaFAQ1r
    , "Tarefa 2 - Teste Construir Mapa mapaFAQ3" ~: mapaFAQ3r ~=? constroiMapa mapaFAQ3
    , "Tarefa 2 - Teste Desconstruir Mapa mapaFAQ3r" ~: mapaFAQ3 ~=? desconstroiMapa mapaFAQ3r
    ]