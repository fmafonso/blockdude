{- |
Module      : Tarefa2_2021li1g032
Description : Construção/Desconstrução do mapa
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g032 where

import LI12122

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pecas = constroiMapaAux pecasOrdLinha maiores (0,0)
    where
        semVazios = removeVazios pecas
        pecasOrdLinha = insertionSort pecas comparaPorCoordenadasYX
        maiores = maioresCoordenadas pecas


-- | Constroi um mapa com base na lista de peças e as suas dimensões
--
--   __NOTAS__:
--
--   * a lista deve estar ordenada por linhas
--
--   * a função serve-se do par de coordenadas atuais para a chamada recursiva
constroiMapaAux :: [(Peca, Coordenadas)] -> Coordenadas -> Coordenadas -> Mapa
constroiMapaAux [] _ _ = []
constroiMapaAux ((p,c):t) maiores atuais
    | c == atuais && fst atuais == fst maiores = insereNoMapa (constroiMapaAux t maiores (0, snd atuais + 1)) p maiores atuais
    | fst atuais == fst maiores = insereNoMapa (constroiMapaAux t maiores (0, snd atuais + 1)) Vazio maiores atuais
    | c == atuais = insereNoMapa (constroiMapaAux t maiores (fst atuais + 1, snd atuais)) p maiores atuais
    | otherwise = insereNoMapa (constroiMapaAux ((p,c):t) maiores (fst atuais + 1, snd atuais)) Vazio maiores atuais

-- | Insere uma peça no mapa
insereNoMapa :: Mapa -> Peca -> Coordenadas -> Coordenadas -> Mapa
insereNoMapa [] p _ _ = [[p]]
insereNoMapa (l : ls) p maiores atuais
    | fst atuais == fst maiores = [p] : (l : ls)
    | otherwise = (p : l) : ls

-- | Torna um Mapa numa lista de peças e respetivas coordenadas
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapaAux mapa (0, 0)

-- | Converte um Mapa na lista de peças não vazias e respetivas coordenadas
desconstroiMapaAux :: Mapa -> Coordenadas -> [(Peca, Coordenadas)]
desconstroiMapaAux [[]] _ = []
desconstroiMapaAux ([]:t) (_, y) = desconstroiMapaAux t (0, y+1)
desconstroiMapaAux ((Vazio : hs) : t) (x, y) = desconstroiMapaAux (hs:t) (x+1, y)
desconstroiMapaAux ((h : hs) : t) (x, y) = (h, (x, y)) : desconstroiMapaAux (hs:t) (x+1, y)
