{- |
Module      : Tarefa4_2021li1g032
Description : Movimentação do personagem
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g032 where

import LI12122

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador c _ b)) AndarDireita = undefined


calculaCoordenadas :: Jogo -> Movimento -> Coordenadas
calculaCoordenadas (Jogo _ (Jogador c _ _)) InterageCaixa = c
calculaCoordenadas (Jogo _ (Jogador (x, y) _ _)) AndarDireita = (x+1, y)
calculaCoordenadas (Jogo _ (Jogador (x, y) _ _)) AndarEsquerda = (x-1, y)
calculaCoordenadas (Jogo _ (Jogador (x, y) Este _)) Trepar = (x+1, y+1)
calculaCoordenadas (Jogo _ (Jogador (x, y) Oeste _)) Trepar = (x-1, y+1)

calculaDirecao :: Jogo -> Movimento -> Direcao
calculaDirecao _ AndarDireita = Este
calculaDirecao _ AndarEsquerda = Oeste
calculaDirecao (Jogo _ (Jogador _ d _)) _ = d

calculaTemCaixa :: Jogo -> Movimento -> Bool
calculaTemCaixa (Jogo _ (Jogador _ _ b)) InterageCaixa = not b
calculaTemCaixa (Jogo _ (Jogador _ _ b)) _ = b

-- | Verifica se é possivel andar uma posição
haEspacoAndar :: Jogo -> Movimento -> Bool
haEspacoAndar (Jogo [] _) _ = False
haEspacoAndar (Jogo [[]] _) _ = False
haEspacoAndar (Jogo [[x]] _) _ = False
haEspacoAndar (Jogo m (Jogador (x, y) _ b)) AndarDireita
    | b == False = acederPeca m (x+1, y) == Vazio
    | otherwise = (acederPeca m (x+1 , y) == Vazio && acederPeca m (x+1, y+1) == Vazio)
haEspacoAndar (Jogo m (Jogador (x, y) _ b)) AndarEsquerda
    | b == False = acederPeca m (x-1, y) == Vazio
    | otherwise = (acederPeca m (x-1 , y) == Vazio && acederPeca m (x-1, y+1) == Vazio)

-- | Verifica se é possivel trepar o bloco ou caixa
haEspacoTrepar :: Jogo -> Bool
haEspacoTrepar (Jogo [] _) = False
haEspacoTrepar (Jogo [[]] _) = False
haEspacoTrepar (Jogo m (Jogador (x, y) Este b))
    | b == False = (pecaDireita == Bloco || pecaDireita == Caixa) && pecaCimaDireita == Vazio && pecaCima == Vazio
    | otherwise = (pecaDireita == Bloco || pecaDireita == Caixa) && pecaCimaDireita2x == Vazio && pecaCima2x == Vazio
        where 
            pecaDireita = acederPeca m (x+1, y)
            pecaCimaDireita = acederPeca m (x+1, y-1)
            pecaCimaDireita2x = acederPeca m (x+1, y-2)
            pecaCima = acederPeca m (x, y-1)
            pecaCima2x = acederPeca m (x, y-2)

haEspacoTrepar (Jogo m (Jogador (x, y) Oeste b))
    | b == False = (pecaEsquerda == Bloco || pecaEsquerda == Caixa) && pecaCimaEsquerda == Vazio && pecaCima == Vazio
    | otherwise = (pecaEsquerda == Bloco || pecaEsquerda == Caixa) && pecaCimaEsquerda2x == Vazio && pecaCima2x == Vazio
        where 
            pecaEsquerda = acederPeca m (x-1, y)
            pecaCimaEsquerda = acederPeca m (x-1, y-1)
            pecaCimaEsquerda2x = acederPeca m (x-1, y-2)
            pecaCima = acederPeca m (x, y-1)
            pecaCima2x = acederPeca m (x, y-1)

-- | Executa uma lista de movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (m:ms) = correrMovimentos (moveJogador jogo m) ms
