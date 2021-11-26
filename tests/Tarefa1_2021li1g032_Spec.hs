module Tarefa1_2021li1g032_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g032
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    ]

-- Testa temEspacoAcimaDoChao
-- [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (1,0)), (Bloco, (2,0))]
-- False

-- [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))]
-- True

-- [(Bloco, (0,0))]
-- False

-- [(Bloco, (0,1))]
-- True

-- []
-- False


-- Testa temChao
-- [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (1,2)), (Bloco, (2,1))]
-- True

-- [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (1,2)), (Bloco, (2,0))]
-- False

-- [(Vazio, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Vazio, (1,1)), (Vazio, (1,2))]
-- True

-- [(Bloco, (0,2)), (Bloco, (1,0))]
-- False

-- []
-- False


-- Testa selecionaChao
-- [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (0,5)), (Bloco, (1,3)), (Bloco, (1,4))]
-- [(Bloco, (0,5)), (Bloco, (1,3)), (Bloco, (1,4))]

-- [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))]
-- [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))]

-- [(Bloco, (0,2))]
-- [(Bloco, (0,2))]

-- []
-- []
