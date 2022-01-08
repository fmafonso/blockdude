module Tarefa6_2021li1g032_Spec where

import Test.HUnit
import Test.HUnit.Base
import Data.Map
import LI12122
import Fixtures
import Fixture_mapaFAQ1
import Fixture_mapaFAQ2
import Fixture_mapaFAQ3
import Tarefa6_2021li1g032

testsT6 =
   test
     [ "Tarefa 6 - Teste MapaFAQ2" ~: Just movimentosM2 ~=? resolveJogo 19 jogoFAQ2
     , "Tarefa 6 - Teste juntaMaybeLista 1" ~: (Nothing::Maybe [Int]) ~=? juntaMaybeLista [Nothing, Nothing, Nothing, Nothing]
     , "Tarefa 6 - Teste juntaMaybeLista 2" ~: Just [1] ~=? juntaMaybeLista [Nothing, Nothing, Just [1], Nothing]
     , "Tarefa 6 - Teste juntaMaybeLista 3" ~: Just [9] ~=? juntaMaybeLista [Nothing, Just [1,9], Just [9], Just [1,2,3]]
     , "Tarefa 6 - Teste juntaMaybeLista 4" ~: Just [] ~=? juntaMaybeLista [Just [2,2], Just [], Just [1], Just [2,2,2,2,2,2]]
     , "Tarefa 6 - Teste juntaMaybeLista 5" ~: Just [2,2] ~=? juntaMaybeLista [Just [2,2], Just [1,1], Just [2,2,2,2,2,2]]
     , "Tarefa 6 - Teste juntaMaybeLista 6" ~: Just [0] ~=? juntaMaybeLista [Just [2,2], Just [0], Just [1,3,4,5,6], Just [2,2,2,2]]
     , "Tarefa 6 - Teste insereMaybeLista 1" ~: Just [1,2] ~=? insereMaybeLista 1 (Just [2])
     , "Tarefa 6 - Teste insereMaybeLista 2" ~: Just [1] ~=? insereMaybeLista 1 (Just [])
     , "Tarefa 6 - Teste insereMaybeLista 2" ~: Nothing ~=? insereMaybeLista [1,2,3,4,5] Nothing
     , "Tarefa 6 - Teste insereMaybeLista 3" ~: Just [5,1,2,3,4] ~=? insereMaybeLista 5 (Just [1,2,3,4])
     , "Tarefa 6 - Teste estaResolvido 1" ~: True ~=? estaResolvido (Jogo mapaFAQ1AposMov (Jogador (1,9) Oeste False))
     , "Tarefa 6 - Teste estaResolvido 2" ~: True ~=? estaResolvido (Jogo mapaFAQ1AposMov (Jogador (1,9) Este True))
     , "Tarefa 6 - Teste estaResolvido 3" ~: False ~=? estaResolvido (Jogo mapaFAQ1AposMov (Jogador (3,9) Oeste True))
     , "Tarefa 6 - Teste estaResolvido 4" ~: True ~=? estaResolvido (Jogo m1r (Jogador (0,3) Este False))
     , "Tarefa 6 - Teste estaResolvido 5" ~: False ~=? estaResolvido (Jogo m1r (Jogador (1,3) Oeste True))
     , "Tarefa 6 - Teste estaResolvido 6" ~: False ~=? estaResolvido (Jogo m3r (Jogador (0,2) Este True))
     , "Tarefa 6 - Teste estaResolvido 7" ~: True ~=? estaResolvido (Jogo m3r (Jogador (5,0) Oeste False))
     , "Tarefa 6 - Teste procura 1" ~: Just (2,Just [AndarEsquerda, AndarEsquerda]) ~=? procura (Jogo m1r j1e3) (DMap (fromList [ ( j1e3, fromList [(m1r,(2, Just [AndarEsquerda, AndarEsquerda]))])]))
     , "Tarefa 6 - Teste procura 2" ~: Just (1,Just [AndarEsquerda]) ~=? procura (Jogo m1r j1e3) (DMap (fromList [ (j1e3, fromList [(m1r, (1, Just [AndarEsquerda]))])]))
     , "Tarefa 6 - Teste procura 3" ~: Just (2,Just [AndarEsquerda, AndarDireita]) ~=? procura (Jogo m1r j1e3) (DMap (fromList [ (j1e3, fromList [(m1r,(2, Just [AndarEsquerda, AndarDireita]))])]))
     , "Tarefa 6 - Teste insere 1" ~: DMap (fromList [ (j2e3, fromList [(m2r,(2, Just [AndarEsquerda, Trepar]))])]) ~=? insere (Jogo m2r j2e3) (2, Just [AndarEsquerda,Trepar]) (DMap (fromList [ (j2e3, fromList [(m2r,(2, Just [AndarEsquerda, Trepar]))])]))
     , "Tarefa 6 - Teste insere 2" ~: DMap (fromList [ (j2e3, fromList [(m3r,(4, Just [AndarEsquerda])),(m2r,(1, Just [AndarEsquerda, Trepar]))])]) ~=? insere (Jogo m3r j2e3) (4, Just [AndarEsquerda]) (DMap (fromList [ (j2e3, fromList [(m2r,(1, Just [AndarEsquerda, Trepar]))])]))
     , "Tarefa 6 - Teste insere 3" ~: DMap (fromList [ (j2e3, fromList [(m3r,(4, Just [AndarEsquerda]))])]) ~=? insere (Jogo m3r j2e3) (4, Just [AndarEsquerda]) (DMap Data.Map.empty)
     ]