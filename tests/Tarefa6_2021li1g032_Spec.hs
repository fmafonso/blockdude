module Tarefa6_2021li1g032_Spec where

import Test.HUnit
import LI12122
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3
import Tarefa6_2021li1g032

testsT6 =
  test
    [ "Tarefa 6 - Teste MapaFAQ2" ~: Just movimentosM2 ~=? resolveJogo 19 jogoFAQ2
    ]