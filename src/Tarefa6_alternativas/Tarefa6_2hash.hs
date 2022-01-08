module Tarefa6_2hash where

import LI12122
import HashableTypes
import Tarefa4_2021li1g032
import Tarefa3_2021li1g032
import Data.Maybe
import qualified Data.HashMap.Strict as M (HashMap, empty, insert, lookup, toList, unionWith)



data DoubleMap outerKey innerKey value = DMap (M.HashMap outerKey (M.HashMap innerKey value))


resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x jogo
    | caminho == Nothing = caminho
    | length (fromJust caminho) <= x = caminho
    | otherwise = Nothing
    where caminho = fst (aux jogo x (DMap M.empty))

aux :: Jogo -> Int -> DoubleMap Mapa Jogador (Int, Maybe [Movimento]) -> (Maybe [Movimento], DoubleMap Mapa Jogador (Int, Maybe [Movimento]))
aux jogo max visitados
    | value /= Nothing && max <= fst (fromJust value) = (snd (fromJust value), visitados)
    | max < 0 = (Nothing, visitados)
    | estaResolvido jogo = (Just [], insere jogo (max, Just []) (snd auxInterage))
    | otherwise = (caminhoMaisCurto, insere jogo (max, caminhoMaisCurto) (snd auxInterage))
    where
        value = procura jogo visitados
        visitadosFrente
            | value == Nothing = insere jogo (max, Nothing) visitados
            | otherwise = visitados
        auxDireita = aux (moveJogador jogo AndarDireita) (max-1) visitadosFrente
        auxEsquerda = aux (moveJogador jogo AndarEsquerda) (max-1) (snd auxDireita)
        auxTrepar = aux (moveJogador jogo Trepar) (max-1) (snd auxEsquerda)
        auxInterage = aux (moveJogador jogo InterageCaixa) (max-1) (snd auxTrepar)
        moveDireita = insereMaybeLista AndarDireita (fst auxDireita)
        moveEsquerda = insereMaybeLista AndarEsquerda (fst auxEsquerda)
        moveTrepar = insereMaybeLista Trepar (fst auxTrepar)
        moveInterage = insereMaybeLista InterageCaixa (fst auxInterage)
        caminhoMaisCurto = juntaMaybeLista [moveDireita, moveEsquerda, moveTrepar, moveInterage]



insere :: Eq v => Jogo -> v -> DoubleMap Mapa Jogador v -> DoubleMap Mapa Jogador v
insere (Jogo mapa jogador) v (DMap dMap) = DMap (M.insert mapa (M.insert jogador v innerMap) dMap)
    where
        procuraMapa = M.lookup mapa dMap
        innerMap = if procuraMapa == Nothing then M.empty else fromJust procuraMapa

procura :: Eq v => Jogo -> DoubleMap Mapa Jogador v -> Maybe v
procura (Jogo mapa jogador) (DMap dMap)
    | procuraMapa == Nothing = Nothing
    | otherwise = M.lookup jogador (fromJust procuraMapa)
    where procuraMapa = M.lookup mapa dMap


insereMaybeLista :: Eq a => a -> Maybe [a] -> Maybe [a]
insereMaybeLista x lista
    | lista == Nothing = Nothing
    | otherwise = Just (x : fromJust lista)


estaResolvido :: Jogo -> Bool
estaResolvido (Jogo mapa (Jogador coord direcao caixa)) = acederPeca mapa coord == Porta

juntaMaybeLista :: Eq a => [Maybe [a]] -> Maybe [a]
juntaMaybeLista [] = Nothing
juntaMaybeLista [l] = l
juntaMaybeLista (Nothing:t) = juntaMaybeLista t
juntaMaybeLista (h1:h2:t)
    | h2 == Nothing = juntaMaybeLista (h1:t)
    | length (fromJust h1) <= length (fromJust h2) = juntaMaybeLista (h1:t)
    | otherwise = juntaMaybeLista (h2:t)

-- unionMaybeLista :: Eq a => Maybe [a] -> Maybe [a] -> Maybe [a]
-- unionMaybeLista l1 Nothing = l1
-- unionMaybeLista Nothing l2 = l2
-- unionMaybeLista (Just l1) (Just l2)
--     | length l1 <= length l2 = Just l1
--     | otherwise = Just l2

-- juntaMap :: [M.HashMap Jogo (Maybe [Movimento])] -> M.HashMap Jogo (Maybe [Movimento])
-- juntaMap [] = M.empty
-- juntaMap [e] = e
-- juntaMap (h1:h2:t) = juntaMap ((M.unionWith unionMaybeLista h1 h2) : t)

-- juntaResultados :: [(Maybe [Movimento], M.HashMap Jogo (Maybe [Movimento]))] -> (Maybe [Movimento], M.HashMap Jogo (Maybe [Movimento]))
-- juntaResultados l = (juntaMaybeLista (map fst l), juntaMap (map snd l))


mapa1 :: Mapa
mapa1 = [[Porta],[Bloco]]

jogador1 :: Jogador
jogador1 = Jogador (0,0) Oeste False

jogo1 :: Jogo
jogo1 = Jogo mapa1 jogador1

mapa2 :: Mapa
mapa2 = [[Porta,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]]

aaa = [(Porta, (0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))]

jogador2 :: Jogador
jogador2 = Jogador (3,0) Oeste False

jogo2 :: Jogo
jogo2 = Jogo mapa2 jogador2



mapaFAQ1r :: Mapa
mapaFAQ1r =
  [ [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

jogoFAQ1 :: Jogo
jogoFAQ1 = Jogo mapaFAQ1r (Jogador (9,6) Oeste False)

tapaBuracoM1 = [AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,InterageCaixa]
fazDegrauM1 = [AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,InterageCaixa]
moveCaixaDireitaM1 = [Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarDireita,InterageCaixa]
fazPassagemMeioM1 = [Trepar,Trepar,InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa]
movRepEsquerdaM1 = [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
movRepDireitaM1 = [AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita]
pegaEscada1M1 = [AndarDireita,Trepar,Trepar,AndarDireita,Trepar,InterageCaixa]
poeEscada1M1 = movRepEsquerdaM1 ++ [AndarEsquerda,InterageCaixa]
pegaEscada2M1 = movRepDireitaM1 ++ [Trepar,AndarDireita,InterageCaixa]
poeEscada2M1 = [AndarEsquerda] ++ movRepEsquerdaM1 ++ [InterageCaixa]
pegaEscada3M1 = movRepDireitaM1 ++ [InterageCaixa]
poeEscada3M1 = movRepEsquerdaM1 ++ [Trepar,InterageCaixa]
fimMapa1 = [Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda]
movimentosM1 = tapaBuracoM1++fazDegrauM1++moveCaixaDireitaM1++fazPassagemMeioM1++pegaEscada1M1++poeEscada1M1++pegaEscada2M1++poeEscada2M1++pegaEscada3M1++poeEscada3M1++fimMapa1


-- movimentosMelhoresM1 = [AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,InterageCaixa,AndarEsquerda,AndarEsquerda,
-- AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,InterageCaixa,Trepar,Trepar,Trepar,AndarDireita,AndarDireita,
-- AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarDireita,InterageCaixa,Trepar,Trepar,InterageCaixa,AndarEsquerda,
-- AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,Trepar,InterageCaixa,
-- AndarEsquerda,Trepar,InterageCaixa,AndarDireita,Trepar,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,
-- AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,
-- AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,Trepar,
-- InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,
-- InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,Trepar,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda]




mapaFAQ2r :: Mapa
mapaFAQ2r =
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ]

jogoFAQ2 :: Jogo
jogoFAQ2 = Jogo mapaFAQ2r (Jogador (16,5) Este False)

movimentosM2 = [
  AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,InterageCaixa,
  AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,
  InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda
  ]

mapaFAQ2AposMov :: Mapa
mapaFAQ2AposMov =
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ]

jogoFAQ2AposMov :: Jogo
jogoFAQ2AposMov = Jogo mapaFAQ2AposMov (Jogador (1,5) Oeste False)

mapaFAQ3 :: [(Peca, Coordenadas)]
mapaFAQ3 =
  [ (Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),
    (Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)), (Bloco,(1,1)),(Porta,(1,8)),
    (Bloco,(1,9)),(Bloco,(1,10)),(Bloco,(1,11)),(Bloco,(1,12)),(Bloco,(2,1)),
    (Bloco,(2,12)),(Bloco,(3,1)),(Bloco,(3,9)),(Bloco,(3,10)),(Bloco,(3,11)),
    (Bloco,(3,12)),(Bloco,(4,1)),(Bloco,(4,9)),(Bloco,(5,0)),(Bloco,(5,8)),
    (Bloco,(5,9)),(Bloco,(6,0)),(Bloco,(6,5)),(Bloco,(6,6)),(Bloco,(6,7)),
    (Bloco,(6,8)),(Bloco,(7,0)),(Caixa,(7,7)),(Bloco,(7,8)),(Bloco,(8,1)),
    (Caixa,(8,7)),(Bloco,(8,8)),(Bloco,(9,1)),(Caixa,(9,7)),(Bloco,(9,8)),
    (Bloco,(10,1)),(Caixa,(10,7)),(Bloco,(10,8)),(Bloco,(11,1)),(Bloco,(11,8)),
    (Bloco,(11,9)),(Bloco,(12,0)),(Bloco,(12,9)),(Bloco,(12,10)),(Bloco,(12,11)),
    (Bloco,(12,12)),(Bloco,(12,13)),(Bloco,(13,0)),(Bloco,(13,13)),(Bloco,(14,0)),
    (Bloco,(14,9)),(Bloco,(14,10)),(Bloco,(14,11)),(Bloco,(14,12)),(Bloco,(14,13)),
    (Bloco,(15,0)),(Bloco,(15,10)),(Bloco,(15,11)),(Bloco,(15,12)),(Bloco,(16,0)),
    (Bloco,(16,12)),(Bloco,(17,0)),(Bloco,(17,12)),(Bloco,(18,0)),(Caixa,(18,11)),
    (Bloco,(18,12)),(Bloco,(19,0)),(Caixa,(19,10)),(Caixa,(19,11)),(Bloco,(19,12)),
    (Bloco,(20,0)),(Caixa,(20,9)),(Caixa,(20,10)),(Caixa,(20,11)),(Bloco,(20,12)),
    (Bloco,(21,1)),(Bloco,(21,2)),(Bloco,(21,3)),(Bloco,(21,4)),(Bloco,(21,5)),
    (Bloco,(21,6)),(Bloco,(21,7)),(Bloco,(21,8)),(Bloco,(21,9)),(Bloco,(21,10)),
    (Bloco,(21,11)),(Bloco,(21,12))
  ]

mapaFAQ3r :: Mapa
mapaFAQ3r =
  [ [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]


jogoFAQ3 :: Jogo
jogoFAQ3 = Jogo mapaFAQ3r (Jogador (12,8) Oeste False)


meteCaixa1 = [Trepar, InterageCaixa, AndarDireita, InterageCaixa]
meteCaixa2 = [AndarEsquerda, Trepar, AndarEsquerda, InterageCaixa, AndarDireita, AndarDireita, InterageCaixa]
meteCaixa3 = [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda, InterageCaixa, AndarDireita, AndarDireita, AndarDireita, InterageCaixa]
poeCaixaVoltar = [AndarDireita, Trepar, AndarDireita, AndarDireita, AndarDireita, Trepar, Trepar, InterageCaixa, AndarEsquerda, AndarEsquerda, InterageCaixa]
poeCimaEsquerda = [AndarDireita, Trepar, InterageCaixa, AndarEsquerda, Trepar,Trepar,Trepar,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa]
pegaCaixaSegundaFila = [AndarDireita, AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita,Trepar,AndarDireita,InterageCaixa]
terminaEscadaMeio = [AndarEsquerda,AndarEsquerda,Trepar,Trepar,Trepar,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa]
pegaBaixoEsquerda = [AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita,InterageCaixa]
porCaixaEsquerda = [AndarEsquerda,Trepar,Trepar,Trepar,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,Trepar,Trepar,Trepar]
largaCaixaEsquerda = porCaixaEsquerda ++ [InterageCaixa]
pegarPenultimaCaixa = [AndarDireita, AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa]
tapaBuraco = [AndarEsquerda,InterageCaixa,AndarEsquerda,AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,AndarDireita,AndarEsquerda,InterageCaixa]
fimMapa3 = [AndarEsquerda, Trepar]

movimentosM3 :: [Movimento]
movimentosM3 = meteCaixa1 ++ meteCaixa2 ++ meteCaixa3 ++ poeCaixaVoltar ++ poeCimaEsquerda ++ pegaCaixaSegundaFila ++ terminaEscadaMeio ++ pegaBaixoEsquerda ++ largaCaixaEsquerda ++ pegarPenultimaCaixa ++ porCaixaEsquerda ++ tapaBuraco ++ fimMapa3


-- main = do
--   let a = resolveJogo 94 jogoFAQ1
--   print a
--   print (length (fromJust a))
