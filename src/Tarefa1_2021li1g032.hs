{- |
Module      : Tarefa1_2021li1g032
Description : Validação de um potencial mapa
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g032 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = c1 && c2
    where
        pecasOrdColuna = insertionSort pecas comparaPorCoordenadasXY
        maiores = maioresCoordenadas pecas
        c1 = not (coordenadasRepetidas pecasOrdColuna)
        c2 = contaPortas pecas == 1
        c3 = caixasTemBase pecasOrdColuna
        c4 = temEspaco pecasOrdColuna maiores (0, 0)
        c5 = temChao pecasOrdColuna maiores 0


-- | Verifica se há mais do que uma declaração de peça para a mesma posição
-- | NOTA: a lista deve estar oredenada por coordenadas
coordenadasRepetidas :: [(Peca, Coordenadas)] -> Bool
coordenadasRepetidas [] = False
coordenadasRepetidas [x] = False
coordenadasRepetidas ((_, x):(p, y):t) = x == y || coordenadasRepetidas ((p, y):t)


-- | Comparator para peças no mapa com base nas coordenadas
comparaPorCoordenadasXY :: (Peca, Coordenadas) -> (Peca, Coordenadas) -> Bool
comparaPorCoordenadasXY (_, c1) (_, c2) = c1 > c2


-- | Conta quantas portas existem no mapa
contaPortas :: [(Peca, Coordenadas)] -> Int
contaPortas ((Porta, _):t) = 1 + contaPortas t
contaPortas (_ : t) = contaPortas t


-- | Verifica se existe um bloco ou caixa por baixo de cada caixa
-- | NOTA: a lista deve estar ordenada por coordenadas
caixasTemBase :: [(Peca, Coordenadas)] -> Bool
caixasTemBase [] = True
caixasTemBase [(Caixa, _)] = False
caixasTemBase [_] = True
caixasTemBase ((Caixa, _):(Porta, _):t) = False
caixasTemBase ((Caixa, (x1, y1)):(p, (x2, y2)):t) = caixaTemBase && caixasTemBase ((p, (x2, y2)):t)
    where caixaTemBase = x1 == x2 && y1 == y2 - 1


-- | Verifica se existe pelo menos um espaço livre no mapa
-- | NOTA: a lista deve estar oredenada por coordenadas
temEspaco :: [(Peca, Coordenadas)] -> Coordenadas -> Coordenadas -> Bool
temEspaco [] _ _ = False
temEspaco ((p,c):t) maiores atuais
    | c /= atuais = True
    | snd c == snd maiores = temEspaco t maiores (fst atuais + 1, 0)
    | otherwise = temEspaco t maiores (fst atuais, snd atuais + 1)


-- | Verifica se toda a base do mapa é constituida por blocos
-- | NOTA: a lista deve estar oredenada por coordenadas
temChao :: [(Peca, Coordenadas)] -> Coordenadas -> Int -> Bool
temChao [] _ _ = True
temChao [(Bloco, c)] maiores colAtual = c == maiores && fst c == colAtual
temChao [_] _ _ = False
temChao ((Bloco,c):t) maiores colAtual
    | snd c /= snd maiores = temChao t maiores colAtual
    | fst c /= colAtual = False
    | otherwise = temChao t maiores (colAtual + 1)
temChao (_:t) maiores colAtual = temChao t maiores colAtual
