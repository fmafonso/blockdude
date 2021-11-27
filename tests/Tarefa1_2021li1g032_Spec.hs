module Tarefa1_2021li1g032_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g032
import Fixtures
import Tarefa1_2021li1g032 (temEspacoAcimaDoChao, selecionaChao, coordenadasRepetidas, contaPortas)
import Test.HUnit (assertEqual, Test (TestCase))

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: True ~=? validaPotencialMapa m1
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa []
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: False ~=? validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))]
    , "Tarefa 1 - Teste Valida Mapa mapaFAQ1" ~: True ~=? validaPotencialMapa mapaFAQ1
    , "Tarefa 1 - Teste Valida Mapa mapaFAQ3" ~: True ~=? validaPotencialMapa mapaFAQ3
    , "Verifica se tem espaço acima do chão 1" ~: False ~=? (temEspacoAcimaDoChao [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (1,0)), (Bloco, (2,0))])
    , "Verifica se tem espaço acima do chão 2" ~: True ~=? (temEspacoAcimaDoChao [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))])
    , "Verifica se tem espaço acima do chão 3" ~: False ~=? (temEspacoAcimaDoChao [(Bloco, (0,0))])
    , "Verifica se tem espaço acima do chão 4" ~: True ~=? (temEspacoAcimaDoChao [(Bloco, (0,1))])
    , "Verifica se tem espaço acima do chão 5" ~: False ~=? (temEspacoAcimaDoChao [])
    , "Verifica se tem chão 1" ~: True ~=? (temChao [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (1,2)), (Bloco, (2,1))])
    , "Verifica se tem chão 2" ~: False ~=? (temChao [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (1,2)), (Bloco, (2,0))])
    , "Verifica se tem chão 3" ~: True ~=? (temChao [(Vazio, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Vazio, (1,1)), (Vazio, (1,2))])
    , "Verifica se tem chão 4" ~: False ~=? (temChao [(Bloco, (0,2)), (Bloco, (1,0))])
    , "Verifica se tem chão 5" ~: False ~=? (temChao [])
    , "Seleciona os blocos que são considerados chão 1" ~: [(Bloco, (0,5)), (Bloco, (1,3)), (Bloco, (1,4))] ~=? (selecionaChao [(Bloco, (0,5)), (Bloco, (1,3)), (Bloco, (1,4))])
    , "Seleciona os blocos que são considerados chão 2" ~: [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))] ~=? (selecionaChao [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (2,1)), (Bloco, (2,2))])
    , "Seleciona os blocos que são considerados chão 3" ~: [(Bloco, (0,2))] ~=? (selecionaChao [(Bloco, (0,2))])
    , "Seleciona os blocos que são considerados chão 4" ~: [] ~=? (selecionaChao [])
    , "Verifica se há coordenadas repetidas 1" ~: True ~=? (coordenadasRepetidas [(Porta, (0,2)), (Bloco, (0,3)), (Caixa, (0,3)),(Bloco, (1,3)), (Bloco, (2,3)), (Bloco, (3,3)), (Bloco, (4,0)), (Bloco, (4,1)), (Bloco, (4,2)), (Bloco, (4,3))])
    , "Verifica se há coordenadas repetidas 2" ~: False ~=? (coordenadasRepetidas [(Porta, (0,2)), (Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Bloco, (4,0)), (Bloco, (4,1)), (Bloco, (4,2)), (Bloco, (4,3))])
    , "Verifica se há coordenadas repetidas 3" ~: False ~=? (coordenadasRepetidas [(Bloco, (0,1))])
    , "Verifica se há coordenadas repetidas 3" ~: False ~=? (coordenadasRepetidas [])
    , "Conta quantas portas tem 1" ~: 1 ~=? (contaPortas [(Porta, (0,0)), (Bloco, (4,0)), (Bloco, (0,1)), (Bloco, (1,1)), (Bloco, (4,1)), (Bloco, (0,2)), (Bloco, (1,2)), (Bloco, (2,2)), (Bloco, (4,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (2,3)), (Bloco, (3,3)), (Bloco, (4,3))])
    , "Conta quantas portas tem 2" ~: 2 ~=? (contaPortas [(Porta, (0,0)), (Bloco, (4,0)), (Bloco, (0,1)), (Bloco, (1,1)), (Bloco, (4,1)), (Bloco, (0,2)), (Bloco, (1,2)), (Bloco, (2,2)), (Bloco, (4,2)), (Porta, (0,3)), (Bloco, (1,3)), (Bloco, (2,3)), (Bloco, (3,3)), (Bloco, (4,3))])
    , "Conta quantas portas tem 3" ~: 0 ~=? (contaPortas [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,2))])
    , "Conta quantas portas tem 4" ~: 0 ~=? (contaPortas [])
    , "Seleciona os blocos de uma lista de peças 1" ~: [] ~=? (selecionaBlocos [(Caixa, (0,1)), (Caixa, (0,2)), (Caixa, (0,3)), (Caixa, (1,3))])
    , "Seleciona os blocos de uma lista de peças 2" ~: [] ~=? (selecionaBlocos [])
    , "Seleciona os blocos de uma lista de peças 3" ~: [(Bloco, (0,1)), (Bloco, (0,3))] ~=? (selecionaBlocos [(Bloco, (0,1)), (Caixa, (0,2)), (Bloco, (0,3))])
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