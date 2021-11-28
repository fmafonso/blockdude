{- |
Module      : Tarefa4_2021li1g032
Description : Movimentação do personagem
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g032 where

import LI12122

-- | Executa um movimento para o jogador (se for possível)
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo AndarDireita = moveDireita jogo
moveJogador jogo AndarEsquerda = moveEsquerda jogo
moveJogador jogo Trepar = trepa jogo
moveJogador jogo InterageCaixa = interage jogo


-- | Move o jogador para a direita (se for possível)
moveDireita :: Jogo -> Jogo
moveDireita (Jogo mapa (Jogador (x, y) d b))
    | haEspacoAndarDireita (Jogo mapa (Jogador (x, y) d b)) = (Jogo mapa (Jogador (x+1, y + movimentoVertical) Este b))
    | otherwise = (Jogo mapa (Jogador (x, y) Este b))
    where movimentoVertical = caiQuanto (Jogo mapa (Jogador (x+1, y) d b))

-- | Move o jogador para a esquerda (se for possível)
moveEsquerda :: Jogo -> Jogo
moveEsquerda (Jogo mapa (Jogador (x, y) d b))
    | haEspacoAndarEsquerda (Jogo mapa (Jogador (x, y) d b)) = (Jogo mapa (Jogador (x-1, y + movimentoVertical) Oeste b))
    | otherwise = (Jogo mapa (Jogador (x, y) Oeste b))
    where movimentoVertical = caiQuanto (Jogo mapa (Jogador (x-1, y) d b))

-- | Executa o movimento `Trepar` (se for possível)
trepa :: Jogo -> Jogo
trepa (Jogo mapa (Jogador (x, y) d b))
    | podeTrepar && d == Este = (Jogo mapa (Jogador (x+1, y-1) d b))
    | podeTrepar = (Jogo mapa (Jogador (x-1, y-1) d b))
    | otherwise = (Jogo mapa (Jogador (x, y) d b))
    where podeTrepar = haEspacoTrepar (Jogo mapa (Jogador (x, y) d b))

-- | Pega ou larga a caixa (se for possível)
interage :: Jogo -> Jogo
interage (Jogo mapa (Jogador (x, y) d b))
    | not valido = (Jogo mapa (Jogador (x, y) d b))
    | d == Este && b = Jogo (inserePeca mapa Caixa (x+1, y + caixaCaiDireita -1)) (Jogador (x, y) d False)
    | d == Este && not b = Jogo (inserePeca mapa Vazio (x+1, y)) (Jogador (x, y) d True)
    | d == Oeste && b = Jogo (inserePeca mapa Caixa (x-1, y + caixaCaiEsquerda -1)) (Jogador (x, y) d False)
    | otherwise = Jogo (inserePeca mapa Vazio (x-1, y)) (Jogador (x, y) d True)
    where
        valido = podeInteragir (Jogo mapa (Jogador (x, y) d b))
        caixaCaiDireita = caiQuanto (Jogo mapa (Jogador (x+1, y-1) d b))
        caixaCaiEsquerda = caiQuanto (Jogo mapa (Jogador (x-1, y-1) d b))

-- | Verifica se é possivel andar para a direita
haEspacoAndarDireita :: Jogo -> Bool
haEspacoAndarDireita (Jogo [[x]] _) = False
haEspacoAndarDireita (Jogo mapa (Jogador (x, y) _ b))
    | b = dentroLimites && elem (acederPeca mapa (x+1 , y)) [Porta, Vazio] && acederPeca mapa (x+1, y-1) == Vazio
    | otherwise = dentroLimites && elem (acederPeca mapa (x+1 , y)) [Porta, Vazio]
    where dentroLimites = x < (length (head mapa) - 1)

-- | Verifica se é possivel andar para a esquerda
haEspacoAndarEsquerda :: Jogo -> Bool
haEspacoAndarEsquerda (Jogo [[x]] _) = False
haEspacoAndarEsquerda (Jogo mapa (Jogador (x, y) _ b))
    | b = dentroLimites && elem (acederPeca mapa (x-1 , y)) [Porta, Vazio] && acederPeca mapa (x-1, y-1) == Vazio
    | otherwise = dentroLimites && elem (acederPeca mapa (x-1 , y)) [Porta, Vazio]
    where dentroLimites = x > 0

-- | Verifica se é possivel trepar
haEspacoTrepar :: Jogo -> Bool
haEspacoTrepar (Jogo m (Jogador (x, y) d b))
    | b && d == Este = elem pecaDireita [Bloco, Caixa] && elem pecaCimaDireita [Porta, Vazio] && pecaCimaDireita2x == Vazio && pecaCima2x == Vazio
    | d == Este = elem pecaDireita [Bloco, Caixa] && elem pecaCimaDireita [Porta, Vazio] && pecaCima == Vazio
    | b && d == Oeste = elem pecaEsquerda [Bloco, Caixa] && elem pecaCimaEsquerda [Porta, Vazio] && pecaCimaEsquerda2x == Vazio && pecaCima2x == Vazio
    | otherwise = elem pecaEsquerda [Bloco, Caixa] && elem pecaCimaEsquerda [Porta, Vazio] && pecaCima == Vazio
    where
        pecaDireita = acederPeca m (x+1, y)
        pecaCimaDireita = acederPeca m (x+1, y-1)
        pecaCimaDireita2x = acederPeca m (x+1, y-2)
        pecaEsquerda = acederPeca m (x-1, y)
        pecaCimaEsquerda = acederPeca m (x-1, y-1)
        pecaCimaEsquerda2x = acederPeca m (x-1, y-2)
        pecaCima = acederPeca m (x, y-1)
        pecaCima2x = acederPeca m (x, y-2)

-- | Verifica se o jogador pode pegar ou pousar a caixa
podeInteragir :: Jogo -> Bool
podeInteragir (Jogo m (Jogador (x, y) d b))
    | d == Este && not b = pecaDireita == Caixa && pecaCima == Vazio && pecaCimaDireita == Vazio
    | d == Oeste && not b = pecaEsquerda == Caixa && pecaCima == Vazio && pecaCimaEsquerda == Vazio
    | d == Este && b = pecaCimaDireita == Vazio
    | otherwise = pecaCimaEsquerda == Vazio
    where
        pecaDireita = acederPeca m (x+1, y)
        pecaCimaDireita = acederPeca m (x+1, y-1)
        pecaEsquerda = acederPeca m (x-1, y)
        pecaCimaEsquerda = acederPeca m (x-1, y-1)
        pecaCima = acederPeca m (x, y-1)

-- | Calcula quantas posições o jogador vai cair
caiQuanto :: Jogo -> Int
caiQuanto (Jogo m (Jogador (x, y) d b))
    | vaiCair (Jogo m (Jogador (x, y) d b)) = 1 + caiQuanto (Jogo m (Jogador (x, y+1) d b))
    | otherwise = 0

-- | Verifica se o jogador vai cair
vaiCair :: Jogo -> Bool
vaiCair (Jogo m (Jogador (x, y) _ _)) = elem (acederPeca m (x, y+1)) [Porta, Vazio]

-- | Executa uma lista de movimentos devolvendo o estado final do `Jogo`
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (m:ms) = correrMovimentos (moveJogador jogo m) ms
