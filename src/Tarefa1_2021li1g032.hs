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
validaPotencialMapa pecas = c1 && c2 && c3 && c4 && c5
    where
        semVazios = removeVazios pecas
        pecasOrdColuna = insertionSort semVazios comparaPorCoordenadasXY
        c1 = not (coordenadasRepetidas pecasOrdColuna)
        c2 = contaPortas semVazios == 1
        c3 = caixasTemBase pecasOrdColuna
        c4 = temEspacoAcimaDoChao pecasOrdColuna
        c5 = temChao pecasOrdColuna


-- | Verifica se há mais do que uma declaração de peça para a mesma posição
--
--   __NOTA__: a lista deve estar oredenada por colunas
coordenadasRepetidas :: [(Peca, Coordenadas)] -> Bool
coordenadasRepetidas [] = False
coordenadasRepetidas [x] = False
coordenadasRepetidas ((_, x):(p, y):t) = x == y || coordenadasRepetidas ((p, y):t)


-- | Conta quantas portas existem no mapa
contaPortas :: [(Peca, Coordenadas)] -> Int
contaPortas [] = 0
contaPortas ((Porta, _):t) = 1 + contaPortas t
contaPortas (_ : t) = contaPortas t


-- | Verifica se existe um bloco ou caixa por baixo de cada caixa
--
--   __NOTA__: a lista deve estar ordenada por colunas
caixasTemBase :: [(Peca, Coordenadas)] -> Bool
caixasTemBase [] = True
caixasTemBase [(Caixa, _)] = False
caixasTemBase [_] = True
caixasTemBase ((Caixa, _):(Porta, _):t) = False
caixasTemBase ((Caixa, (x1, y1)):(p, (x2, y2)):t) = caixaTemBase && caixasTemBase ((p, (x2, y2)):t)
    where caixaTemBase = x1 == x2 && y1 == y2 - 1


-- | Verifica se existe pelo menos um espaço livre no mapa
--
--   __NOTA__: a lista deve estar oredenada por colunas
temEspacoAcimaDoChao :: [(Peca, Coordenadas)] -> Bool
temEspacoAcimaDoChao pecas = temEspacoAcimaDoChaoAux pecas fundo (0,0)
    where fundo = pegaNumBlocoPorColuna (selecionaChao (selecionaBlocos pecas))

temEspacoAcimaDoChaoAux :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Coordenadas -> Bool
temEspacoAcimaDoChaoAux [] _ _ = False
temEspacoAcimaDoChaoAux ((p,(x,y)):t) ((pm,(xm,ym)):tm) atuais
    | (x,y) /= atuais = True
    | y == ym = temEspacoAcimaDoChaoAux t tm (fst atuais + 1, 0)
    | otherwise = temEspacoAcimaDoChaoAux t ((pm,(xm,ym)):tm) (fst atuais, snd atuais + 1)

pegaNumBlocoPorColuna :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pegaNumBlocoPorColuna [pc] = [pc]
pegaNumBlocoPorColuna ((p1, (x1, y1)) : (p2, (x2, y2)):t)
    | x1 == x2 = pegaNumBlocoPorColuna ((p2, (x2, y2)):t)
    | otherwise = (p1, (x1, y1)) : pegaNumBlocoPorColuna ((p2, (x2, y2)):t)

-- | Verifica se toda a base do mapa é constituida por blocos
--
--   __NOTA__: a lista deve estar oredenada por colunas
temChao :: [(Peca, Coordenadas)] -> Bool
temChao x = temChaoAux (map snd x) (fst (maioresCoordenadas x))

temChaoAux :: [Coordenadas] -> Int -> Bool
temChaoAux [] _ = False
temChaoAux ((x,y):t) maiorX = x == maiorX || n || ne || e || se || s || so || o || no
    where
        n = temChaoAux (subLista t (x,y-1)) maiorX
        ne = temChaoAux (subLista t (x+1, y-1)) maiorX
        e = temChaoAux (subLista t (x+1, y)) maiorX
        se = temChaoAux (subLista t (x+1, y+1)) maiorX
        s = temChaoAux (subLista t (x, y+1)) maiorX
        so = temChaoAux (subLista t (x-1, y+1)) maiorX
        o = temChaoAux (subLista t (x-1, y)) maiorX
        no = temChaoAux (subLista t (x-1, y-1)) maiorX



selecionaChao :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
selecionaChao [] = []
selecionaChao [pc] = [pc]
selecionaChao ((p1, (x1, y1)):t)
    | x1 == x2 && y1 + 1 == y2 = (p1, (x1,y1)) : ultimasPecasTail
    | x1 == x2 = ultimasPecasTail
    | otherwise = (p1, (x1,y1)) : ultimasPecasTail
    where
        ultimasPecasTail = selecionaChao t
        x2 = fst (snd (head ultimasPecasTail))
        y2 = snd (snd (head ultimasPecasTail))

selecionaBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
selecionaBlocos [] = []
selecionaBlocos ((p, c):t)
    | p == Bloco = (p, c) : selecionaBlocos t
    | otherwise = selecionaBlocos t


-- .x..
-- xxxx
-- .x..
-- .x..

subLista :: Eq a => [a] -> a -> [a]
subLista [] _ = []
subLista (c:cs) pivo
    | c == pivo = c:cs
    | otherwise = subLista cs pivo



-- Tem chao? Tem espaco? E valido?
-- Pxx
-- x..

-- Tem chao? E valido?
-- x.
-- x.
-- .x
-- x.

-- Pode pegar na caixa?
-- P..C
-- xxxx

-- Tem chao? E valido?
-- ...P
-- ..xx
-- .x..  ? = Bloco || Vazio
-- ..x.
-- xx..

-- Tem chao? E valido?
-- Px.
-- x.x
-- x.x

-- x.x
-- xxx