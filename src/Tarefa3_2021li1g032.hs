{- |
Module      : Tarefa3_2021li1g032
Description : Representação textual do jogo
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g032 where

import LI12122

instance Show Jogo where
    show = showJogo


showJogo :: Jogo -> String
showJogo jogo = showJogoAux jogo (0, 0)

-- | Converte um Mapa na lista de peças não vazias e respetivas coordenadas
showJogoAux :: Jogo -> Coordenadas -> String
showJogoAux (Jogo [[]] _) _ = ""
showJogoAux (Jogo ([]:t) j) (_, y) = '\n' : showJogoAux (Jogo t j) (0, y+1)
showJogoAux (Jogo ((h : hs) : t) (Jogador c d b)) (x, y)
    | c == (x,y+1) && b = (showPeca Caixa) : restoDoJogo
    | c == (x,y) = (showJogador (Jogador c d b)) : restoDoJogo
    | otherwise = (showPeca h) : restoDoJogo
    where restoDoJogo = showJogoAux (Jogo (hs:t) (Jogador c d b)) (x+1, y)

showPeca :: Peca -> Char
showPeca Vazio = ' '
showPeca Bloco = 'X'
showPeca Porta = 'P'
showPeca Caixa = 'C'

showJogador :: Jogador -> Char
showJogador (Jogador _ Este _) = '>'
showJogador _ = '<'
