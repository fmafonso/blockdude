module Tarefa2_2021li1g032_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g032
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3


testsT2 =
  test
    [ "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Mapa m2" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Desconstruir Mapa m2" ~: m2 ~=? desconstroiMapa m2r
    , "Tarefa 2 - Teste Construir Mapa m3" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Desconstruir Mapa m3" ~: m3 ~=? desconstroiMapa m3r
    , "Tarefa 2 - Teste Construir Mapa m5" ~: m5r ~=? constroiMapa m5
    , "Tarefa 2 - Teste Desconstruir Mapa m5" ~: m5 ~=? desconstroiMapa m5r
    , "Tarefa 2 - Teste Construir Mapa m6" ~: m6r ~=? constroiMapa m6
    , "Tarefa 2 - Teste Desconstruir Mapa m6" ~: m6 ~=? desconstroiMapa m6r
    , "Tarefa 2 - Teste Construir Mapa m7" ~: m7r ~=? constroiMapa m7
    , "Tarefa 2 - Teste Desconstruir Mapa m7" ~: m7 ~=? desconstroiMapa m7r
    , "Tarefa 2 - Teste Construir Mapa m8" ~: m8r ~=? constroiMapa m8
    , "Tarefa 2 - Teste Desconstruir Mapa m8" ~: m8 ~=? desconstroiMapa m8r
    , "Tarefa 2 - Teste Construir Mapa m9" ~: m9r ~=? constroiMapa m9
    , "Tarefa 2 - Teste Desconstruir Mapa m9" ~: m9 ~=? desconstroiMapa m9r
    , "Tarefa 2 - Teste Construir Mapa m10" ~: m10r ~=? constroiMapa m10
    , "Tarefa 2 - Teste Desconstruir Mapa m10" ~: m10 ~=? desconstroiMapa m10r
    , "Tarefa 2 - Teste Construir Mapa m11" ~: m11r ~=? constroiMapa m11
    , "Tarefa 2 - Teste Desconstruir Mapa m11" ~: m11 ~=? desconstroiMapa m11r
    , "Tarefa 2 - Teste Construir Mapa m12" ~: m12r ~=? constroiMapa m12
    , "Tarefa 2 - Teste Desconstruir Mapa m12" ~: m12 ~=? desconstroiMapa m12r
    , "Tarefa 2 - Teste Construir Mapa m13" ~: m13r ~=? constroiMapa m13
    , "Tarefa 2 - Teste Desconstruir Mapa m13" ~: m13 ~=? desconstroiMapa m13r
    , "Tarefa 2 - Teste Construir Mapa m14" ~: m14r ~=? constroiMapa m14
    , "Tarefa 2 - Teste Desconstruir Mapa m14" ~: m14 ~=? desconstroiMapa m14r
    , "Tarefa 2 - Teste Construir Mapa m15" ~: m15r ~=? constroiMapa m15
    , "Tarefa 2 - Teste Desconstruir Mapa m15" ~: m15 ~=? desconstroiMapa m15r
    , "Tarefa 2 - Teste Construir Mapa m16" ~: m16r ~=? constroiMapa m16
    , "Tarefa 2 - Teste Desconstruir Mapa m16" ~: m16 ~=? desconstroiMapa m16r
    , "Tarefa 2 - Teste Construir Mapa m17" ~: m17r ~=? constroiMapa m17
    , "Tarefa 2 - Teste Desconstruir Mapa m17" ~: m17 ~=? desconstroiMapa m17r
    , "Tarefa 2 - Teste Construir Mapa mapaFAQ1" ~: mapaFAQ1r ~=? constroiMapa mapaFAQ1
    , "Tarefa 2 - Teste Desconstruir Mapa mapaFAQ1r" ~: mapaFAQ1 ~=? desconstroiMapa mapaFAQ1r
    , "Tarefa 2 - Teste Construir Mapa mapaFAQ2" ~: mapaFAQ2r ~=? constroiMapa mapaFAQ2
    , "Tarefa 2 - Teste Desconstruir Mapa mapaFAQ2r" ~: mapaFAQ2 ~=? desconstroiMapa mapaFAQ2r
    , "Tarefa 2 - Teste Construir Mapa mapaFAQ3" ~: mapaFAQ3r ~=? constroiMapa mapaFAQ3
    , "Tarefa 2 - Teste Desconstruir Mapa mapaFAQ3r" ~: mapaFAQ3 ~=? desconstroiMapa mapaFAQ3r
    ]