module Tarefa5_events where

import Data.Maybe

import Graphics.Gloss.Interface.IO.Game

import LI12122
import Tarefa4_2021li1g032
import Tarefa5_io
import Tarefa5_types


-- TODO tirar as texturas
event :: Event -> BlockDude -> IO BlockDude
event ev ((MainMenu op), niveis, texturas) = return (eventMenu ev ((MainMenu op), niveis, texturas))
event ev ((PlayMenu n), niveis, texturas) = return (eventPlayMenu ev ((PlayMenu n), niveis, texturas))
event ev ((ModoJogo estadoJogo n m), niveis, texturas) = return (eventModoJogo ev ((ModoJogo estadoJogo n m), niveis, texturas))
event ev (VenceuJogo, niveis, texturas) = return (eventVenceuJogo ev (VenceuJogo, niveis, texturas))
event ev (GuardaBlockDude, niveis, texturas) = eventGuardar ev (GuardaBlockDude, niveis, texturas)
event ev (MenuDefinicoes m, niveis, texturas) = return (eventDefinicoes ev ((MenuDefinicoes m), niveis, texturas))

eventMenu :: Event -> BlockDude -> BlockDude
-- eventMenu blockdude = do
--     (EventKey (SpecialKey k) Down _ _ ) (MainMenu o, n, t) <- blockdude
--     when (k == KeyUp && o == Jogar) return (MainMenu Sair, n, t)
--     when (k == )
eventMenu (EventKey (SpecialKey KeyUp) Down  _ _ ) (MainMenu Jogar, niveis, texturas) = (MainMenu Sair, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Guardar, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu OpcaoDefinicoes, niveis, texturas) = (MainMenu Guardar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Creditos, niveis, texturas) = (MainMenu OpcaoDefinicoes, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Sair , niveis, texturas) = (MainMenu Creditos, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Jogar, niveis, texturas) = (MainMenu Guardar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Guardar, niveis, texturas) = (MainMenu OpcaoDefinicoes, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu OpcaoDefinicoes, niveis, texturas) = (MainMenu Creditos, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Creditos, niveis, texturas) = (MainMenu Sair, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Sair, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Sair, niveis, texturas) = undefined
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu OpcaoDefinicoes, niveis, texturas) = (MenuDefinicoes PackTexturas, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Jogar, (niveis, atual), texturas) = (PlayMenu atual, (niveis, atual), texturas)
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Guardar, niveis, texturas) = (GuardaBlockDude, niveis, texturas)
    -- do
    -- saveBlockDude (MainMenu Guardar, niveis, texturas)
    -- return (MainMenu Guardar, niveis, texturas)
eventMenu _ bd = bd

eventGuardar :: Event -> BlockDude -> IO BlockDude
eventGuardar (EventKey (SpecialKey KeyEnter) Up _ _ ) blockdude = do
    saveBlockDude blockdude
    return blockdude
eventGuardar _ (janela, niveis, texturas) = do
    return (MainMenu Jogar, niveis, texturas)

eventVenceuJogo :: Event -> BlockDude -> BlockDude
eventVenceuJogo _ (VenceuJogo, (niveis, atual), texturas) = (PlayMenu atual, (niveis, atual), texturas)

eventPlayMenu :: Event -> BlockDude -> BlockDude
eventPlayMenu (EventKey (SpecialKey KeyLeft) Down _ _ ) (PlayMenu 0, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventPlayMenu (EventKey (SpecialKey k) Down  _ _ ) (PlayMenu n, (niveis, atual), texturas)
    | k == KeyLeft = (PlayMenu (if n-1 < 0 then n else n-1), (niveis, atual), texturas)
    | k == KeyRight = (PlayMenu (if n+1 > atual then n else n+1), (niveis, atual), texturas)
    | k == KeyEnter = (ModoJogo jogo n 0, (niveis, atual), texturas)
    | otherwise = (PlayMenu n, (niveis, atual), texturas)
    where
        jogo = fst3 (niveis !! n)
eventPlayMenu _ bd = bd

eventModoJogo :: Event -> BlockDude -> BlockDude
eventModoJogo (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoJogo estadoJogo n m, niveis, texturas) = (ModoJogo (moveJogador estadoJogo Trepar) n (m+1), niveis, texturas)
eventModoJogo (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoJogo estadoJogo n m, niveis, texturas) = (ModoJogo (moveJogador estadoJogo InterageCaixa) n (m+1), niveis, texturas)
eventModoJogo (EventKey (SpecialKey KeyLeft) Down _ _ ) (ModoJogo estadoJogo n m, niveis, texturas) = (ModoJogo (moveJogador estadoJogo AndarEsquerda) n (m+1), niveis, texturas)
eventModoJogo (EventKey (SpecialKey KeyRight) Down _ _ ) (ModoJogo estadoJogo n m, niveis, texturas) = (ModoJogo (moveJogador estadoJogo AndarDireita) n (m+1), niveis, texturas)
eventModoJogo (EventKey (Char 'r') Down _ _ ) (ModoJogo estadoJogo n m, (niveis, atual), texturas) = (ModoJogo (fst3 (niveis !! n)) n 0, (niveis, atual), texturas)
eventModoJogo (EventKey (Char 'q') Down _ _ ) (_, (niveis, atual), texturas) = (PlayMenu atual, (niveis, atual), texturas)
eventModoJogo _ (ModoJogo (Jogo mapa (Jogador c d b)) n m, (niveis, atual), texturas)
    | acederPeca mapa c == Porta && atual == n = (VenceuJogo, (atualizaMovs m niveis n, novoAtual), texturas)
    | acederPeca mapa c == Porta = (VenceuJogo, (atualizaMovs m niveis n, atual), texturas)
    | otherwise = (ModoJogo (Jogo mapa (Jogador c d b)) n m, (niveis, atual), texturas)
    where novoAtual = min ((length niveis)-1) (atual+1)

atualizaMovs :: Movimentos -> [Nivel] -> NivelID -> [Nivel]
atualizaMovs m niveis id
    | min == Nothing = inicio ++ [(jogo, Just m, opt)] ++ fim
    | otherwise = inicio ++ [nivelAtualizado] ++ fim
    where
        (jogo, min, opt) = niveis !! id
        inicio = take id niveis
        fim = drop (id+1) niveis
        movMin = fromJust min
        nivelAtualizado = (jogo, Just (if m < movMin then m else movMin), opt)

eventDefinicoes :: Event -> BlockDude -> BlockDude
eventDefinicoes (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuDefinicoes PackTexturas, niveis, texturas) = (MenuDefinicoes Voltar, niveis, texturas)
eventDefinicoes (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuDefinicoes Voltar, niveis, texturas) = (MenuDefinicoes PackTexturas, niveis, texturas)
eventDefinicoes (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuDefinicoes PackTexturas, niveis, texturas) = (MenuDefinicoes Voltar, niveis, texturas)
eventDefinicoes (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuDefinicoes Voltar, niveis, texturas) = (MenuDefinicoes PackTexturas, niveis, texturas)
eventDefinicoes (EventKey (SpecialKey KeyEnter) Down _ _ ) (MenuDefinicoes Voltar, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventDefinicoes (EventKey (SpecialKey KeyRight) Down _ _ ) (MenuDefinicoes PackTexturas, niveis, (texturas, atual))
    | atual+1 == length texturas = (MenuDefinicoes PackTexturas, niveis, (texturas, atual))
    | otherwise = (MenuDefinicoes PackTexturas, niveis, (texturas, atual+1))
eventDefinicoes (EventKey (SpecialKey KeyLeft) Down _ _ ) (MenuDefinicoes PackTexturas, niveis, (texturas, atual)) 
    | atual == 0 = (MenuDefinicoes PackTexturas, niveis, (texturas, atual))
    | otherwise = (MenuDefinicoes PackTexturas, niveis, (texturas, atual-1))
eventDefinicoes _ blockdude = blockdude











fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x