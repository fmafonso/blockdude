module Tarefa5_draw where

import Data.Map (Map, empty, insert, lookup)
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

import LI12122
import Tarefa2_2021li1g032
import Tarefa5_types

draw :: BlockDude -> IO Picture
draw (janela, niveis, (pacotes, pID)) = do
    picture <- drawAux (janela, niveis, (pacotes, pID))
    (l, a) <- getScreenSize
    let 
        dx = (fromIntegral l) / 2
        dy = (fromIntegral a) / 2
        (_, cor) = pacotes !! pID
        fundo = Color cor $ polygon [(dx, dy),(dx, -dy), (-dx, -dy), (-dx, dy)] 
    return (Pictures [fundo, picture])

drawAux :: BlockDude -> IO Picture
drawAux (MainMenu opcao, _, t) = return (drawMainMenu opcao t)
drawAux (PlayMenu nivel, niveis, t) = return (drawPlayMenu (PlayMenu nivel, niveis, t))
drawAux (ModoJogo jogo _ _, _, t) = return (drawModoJogo jogo t)
drawAux (VenceuJogo, _, t) = return (drawVenceuJogo t)
drawAux (GuardaBlockDude, _, t) = return (drawGuardaBlockDude t)
drawAux (MenuDefinicoes definicao, _, t) = return (drawMenuDefinicoes definicao t)

----------------------------------------------------------------

drawMainMenu :: Opcoes -> Texturas -> Picture
drawMainMenu op (pacotes, pID) = Pictures [
    Translate (0) (a*1/4) $ drawMenuOption play,
    Translate (0) (a*1/8) $ drawMenuOption guardar,
    drawMenuOption definicoes,
    Translate (0) (-(a*1/8)) $ drawMenuOption creditos,
    Translate (0) (-(a*1/4)) $ drawMenuOption sair]
    where
        a = fromIntegral alturaJanela
        (mapTexturas, _) = pacotes !! pID
        play = fromJust (Data.Map.lookup (if op == Jogar then TexturaPlayR else TexturaPlay) mapTexturas)
        guardar = fromJust (Data.Map.lookup (if op == Guardar then TexturaGuardarR else TexturaGuardar) mapTexturas)
        definicoes = fromJust (Data.Map.lookup (if op == OpcaoDefinicoes then TexturaDefinicoesR else TexturaDefinicoes) mapTexturas)
        creditos = fromJust (Data.Map.lookup (if op == Creditos then TexturaCreditosR else TexturaCreditos) mapTexturas)
        sair = fromJust (Data.Map.lookup (if op == Sair then TexturaSairR else TexturaSair) mapTexturas)

drawMenuOption :: (PictureInfo, Picture) -> Picture
drawMenuOption ((_, _, altura), picture) = Scale (d/a) (d/a) picture
    where
        a = fromIntegral altura
        d = fromIntegral dimensaoBloco

----------------------------------------------------------------

drawPlayMenu :: BlockDude -> Picture
drawPlayMenu (PlayMenu n, niveis, texturas) = Pictures (drawPlayMenuAux 0 (PlayMenu n, niveis, texturas))

drawPlayMenuAux :: NivelID -> BlockDude -> [Picture]
drawPlayMenuAux _ (_, ([], _), _) = []
drawPlayMenuAux i (PlayMenu n,((h:t), atual), texturas)
    | i == n = drawNivelRealcado h n texturas : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n-1 = drawNivelSecundario h i texturas Oeste atual : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n+1 = [drawNivelSecundario h i texturas Este atual]
    | otherwise = drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)

-- separar em drawJogo drawEstrelas
drawNivelRealcado :: Nivel -> NivelID -> Texturas -> Picture
drawNivelRealcado n id t = Pictures [drawEstrelas n t, drawNivel n t False, drawNivelID id t]

drawNivelID :: NivelID -> Texturas -> Picture
drawNivelID id texturas = Translate 0 (h*7/20) $ Scale 1 1 $ drawOpcaoNumerada TexturaNivel id texturas True
    where h = fromIntegral alturaJanela

drawNivel :: Nivel -> Texturas -> Bool -> Picture
drawNivel (j, _, _) t bloqueado = Scale (4/9) (1/2) $ Pictures [drawModoJogo j t, drawGrid bloqueado]

drawEstrelas :: Nivel -> Texturas -> Picture
drawEstrelas (_, m, o) (pacotes, tID) = Pictures [estrelaEsquerda, estrelaMeio, estrelaDireita]
    where
        ed = (fromIntegral dimensaoBloco) / 2
        mx = (fromIntegral larguraJanela) / 12
        my = -((fromIntegral alturaJanela) / 3)
        nEstrelas = numEstrelas m o
        (mapTexturas, _) = pacotes !! tID
        estrelaPreta = snd (fromJust (Data.Map.lookup TexturaSemEstrela mapTexturas))
        dimensao = fromIntegral dimensaoBloco
        estrela = snd (fromJust (Data.Map.lookup TexturaEstrela mapTexturas))
        estrelaEsquerda = Translate (-mx) my $ if nEstrelas > 0 then estrela else estrelaPreta
        estrelaMeio = Translate 0 my $ if nEstrelas > 1 then estrela else estrelaPreta
        estrelaDireita = Translate mx my $ if nEstrelas > 2 then estrela else estrelaPreta



drawNivelSecundario :: Nivel -> NivelID -> Texturas -> Direcao -> Int -> Picture
drawNivelSecundario n i (pacotes, tID) dir atual = Translate mx 0 $ Pictures [nivelPicture, numero]
    where
        dx = (fromIntegral larguraJanela) * 13 / 36
        mx = if dir == Este then dx else -dx
        x = (fromIntegral larguraJanela) / 9
        y = (fromIntegral alturaJanela) / 8
        bloqueado = (dir == Este) && (atual <= i-1)
        nivelPicture = Pictures ([Scale (0.5) (0.5) $ drawNivel n (pacotes, tID) bloqueado])
        (mapTexturas, _) = pacotes !! tID
        (_, numNivel) = fromJust (Data.Map.lookup (TexturaNumeroR i) mapTexturas)
        alturaNumero = (fromIntegral larguraJanela) / 7
        numero = Translate 0 alturaNumero $ Scale (0.8) (0.8) numNivel

numEstrelas :: MovimentosMin -> MovimentosOtimos -> Int
numEstrelas Nothing _ = 0
numEstrelas m o
    | x <= y = 3
    | x > y+5 = 1
    | otherwise = 2
    where
        x = fromIntegral (fromJust m)
        y = fromIntegral o

----------------------------------------------------------------

drawModoJogo :: Jogo -> Texturas -> Picture
drawModoJogo jogo texturas = Pictures (drawJogo jogo texturas)

drawJogo :: Jogo -> Texturas -> [Picture]
drawJogo (Jogo mapa (Jogador c d b)) texturas = drawJogoAux m (Jogador c d b) camara texturas
    where
        m = desconstroiMapa mapa
        dimMapa = maioresCoordenadas m
        dimJanela = (blocosLargura, blocosAltura)
        camara = calculaCamara dimMapa dimJanela c

drawJogoAux :: [(Peca,Coordenadas)] -> Jogador -> Camara -> Texturas -> [Picture]
drawJogoAux [] jogador cam texturas = drawPlayer jogador cam texturas
drawJogoAux ((p,c):t) (Jogador (x,y) d temCaixa) cam texturas
    | not naJanela || pecaTemJogador || pecaTemCaixa = drawJogoAux t (Jogador (x,y) d temCaixa) cam texturas
    | otherwise = drawPeca (p,(px,-py)) texturas : (drawJogoAux t (Jogador (x,y) d temCaixa) cam texturas)
    where
        (px,py) = subtraiCoordenadas c (meioCamara cam)
        naJanela = estaNaJanela c cam
        pecaTemJogador = c == (x,y)
        pecaTemCaixa = temCaixa && (c == (x,y-1))

meioCamara :: Camara -> Coordenadas
meioCamara ((x1,y1),(x2,y2)) = (x1+(div (x2-x1) 2) + (mod (x2-x1) 2) , y1 + (div (y2-y1) 2))

estaNaJanela :: Coordenadas -> Camara -> Bool
estaNaJanela (x,y) ((minX, minY),(maxX, maxY)) = minX <= x && x <= maxX && minY <= y && y <= maxY

drawPeca :: (Peca, Coordenadas) -> Texturas -> Picture
drawPeca (Bloco, coord) t = drawBloco coord t
drawPeca (Caixa, coord) t = drawCaixa coord t
drawPeca (Porta, coord) t = drawPorta coord t

drawSquare :: Coordenadas -> (PictureInfo, Picture) -> Picture
drawSquare (x,y) ((_, largura, altura), p) = Translate (i+d/2) (j+d/2) $ Scale (d/l) (d/a) p
    where
        d = fromIntegral dimensaoBloco
        i = (fromIntegral x) * d
        j = (fromIntegral y) * d
        l = fromIntegral largura
        a = fromIntegral altura

drawBloco :: Coordenadas -> Texturas -> Picture
drawBloco coord (pacotes, tID) = drawSquare coord bloco
    where
        (mapTexturas, _) = pacotes !! tID
        bloco = fromJust (Data.Map.lookup TexturaBloco mapTexturas)

drawCaixa :: Coordenadas -> Texturas -> Picture
drawCaixa coord (pacotes, tID) = drawSquare coord caixa
    where
        (mapTexturas, _) = pacotes !! tID
        caixa = fromJust (Data.Map.lookup TexturaCaixa mapTexturas)

drawPorta :: Coordenadas -> Texturas -> Picture
drawPorta coord (pacotes, tID) = drawSquare coord porta
    where
        (mapTexturas, _) = pacotes !! tID
        porta = fromJust (Data.Map.lookup TexturaPorta mapTexturas)

drawPlayer :: Jogador -> Camara -> Texturas -> [Picture]
drawPlayer (Jogador c d temCaixa) cam (pacotes, tID) = (drawSquare (x,-y) jogador) : (if temCaixa then [drawCaixa (x,-y+1) (pacotes, tID)] else [])
    where
        (x,y) = subtraiCoordenadas c (meioCamara cam)
        (mapTexturas, _) = pacotes !! tID
        jogador = fromJust (Data.Map.lookup (TexturaJogador d temCaixa) mapTexturas)

drawTriangulo :: Coordenadas -> Direcao -> Picture
drawTriangulo (x,y) dir
    | dir == Este = polygon [(i,j), (i+d,j+(d/2)), (i,j+d)]
    | otherwise = polygon [(i+d,j), (i,j+(d/2)), (i+d,j+d)]
    where
        d = fromIntegral dimensaoBloco
        i = (fromIntegral x) * 40
        j = (fromIntegral y) * 40

calculaCamara :: DimMapa -> DimJanela -> PosJogador -> Camara
calculaCamara (x1,y1) (x2,y2) (x3,y3)
    | (o > 0) == (e < x1) && (n > 0 ) == (s < y1) = ((o,n),(e,s))
    | (o > 0) == (e < x1) = ((o, n+dV), (e, s+dV))
    | (n > 0 ) == (s < y1) = ((o+dH, n), (e+dH, s))
    | otherwise = ((o+dH, n+dV), (e+dH, s+dV)) -- por <=/>= em todos
    where
        o = x3 - (div x2 2)
        e = x3 + (div x2 2) + (mod x2 2) - 1
        n = y3 - (div y2 2) - (mod y2 2) + 1
        s = y3 + (div y2 2)
        dV = if abs (y1 - s) < (abs n) then y1 - s else -n
        dH = if abs (x1 - e) < (abs o) then x1 - e else -o

{-
TESTES UNITARIOS FUNCAO CalculaCamara
(5,5) (3,3) | (4,1) -> ((3,0),(5,2))
            | (1,1) -> ((0,0),(2,2))
            | (1,4) -> ((0,3),(2,5))
            | (4,4) -> ((3,3),(5,5))

            | (0,0) -> ((0,0),(2,2))
            | (5,0) -> ((3,0),(5,2))
            | (5,5) -> ((3,3),(5,5))
            | (0,5) -> ((0,3),(2,5))

(5,5) (4,4) (3,2) -> ((1,1),(4,4))
-}

subtraiCoordenadas :: Coordenadas -> Coordenadas -> Coordenadas
subtraiCoordenadas (x1, y1) (x2, y2) = (x1-x2, y1-y2)


drawGrid :: Bool -> Picture
drawGrid bloqueado = Color cor $ Pictures ((map linhaVertical xList) ++ (map linhaHorizontal yList) ++ nivelBloqueado)
    where
        x = fromIntegral blocosLargura / 2
        y = fromIntegral blocosAltura / 2
        xList = [(-x),x]
        yList = [(-y),y]
        dx = fromIntegral larguraJanela / 2
        dy = fromIntegral alturaJanela / 2
        cor = if bloqueado then makeColorI 255 30 30 120 else white
        nivelBloqueado = if bloqueado then [polygon [(dx,dy), (dx,-dy), (-dx,-dy), (-dx,dy)]] else []

linhaHorizontal :: Float -> Picture
linhaHorizontal y = line [(-i, j), (i,j)]
    where
        i = fromIntegral larguraJanela / 2
        j = y * fromIntegral dimensaoBloco

linhaVertical :: Float -> Picture
linhaVertical x = line [(i, -j), (i, j)]
    where
        i =  x * fromIntegral dimensaoBloco
        j = fromIntegral alturaJanela / 2

----------------------------------------------------------------

drawVenceuJogo :: Texturas -> Picture
drawVenceuJogo (pacotes, tID) = venceu
    where
        (mapTexturas, _) = pacotes !! tID
        (_, venceu) = fromJust (Data.Map.lookup TexturaVenceu mapTexturas)

----------------------------------------------------------------

drawGuardaBlockDude :: Texturas -> Picture
drawGuardaBlockDude (pacotes, tID) = guardou
    where
        (mapTexturas, _) = pacotes !! tID
        (_, guardou) = fromJust (Data.Map.lookup TexturaGuardou mapTexturas)

----------------------------------------------------------------

drawMenuDefinicoes :: Definicoes -> Texturas -> Picture
drawMenuDefinicoes d (pacotes, pID) = Pictures [
    Translate 0 (a/8) $ texturaPacote,
    Translate 0 (-a/8) $ drawMenuOption texturaBack]
    where
        a = fromIntegral alturaJanela
        realcado = if d == PackTexturas then TexturaPackR else TexturaPack
        texturaPacote = drawOpcaoNumerada realcado pID (pacotes, pID) (d == PackTexturas)
        (mapTexturas, _) = pacotes !! pID
        texturaBack = fromJust (Data.Map.lookup (if d == Voltar then TexturaBackR else TexturaBack) mapTexturas)

drawOpcaoNumerada :: Textura -> Int -> Texturas -> Bool -> Picture
drawOpcaoNumerada t x (pacotes, pID) r = Pictures [
    (Translate (-ln/2) 0 $ drawMenuOption ((ft, lt, at), textura) ),
    Translate (larguraTexto/2) 0 texturaNumero]
    where
        (mapTexturas, _) = pacotes !! pID
        (texturaNumero, ln) = drawNumero x (pacotes, pID) r
        ((ft, lt, at), textura) = fromJust (Data.Map.lookup t mapTexturas)
        larguraTexto = (fromIntegral lt) * (fromIntegral dimensaoBloco) / (fromIntegral at)

drawNumero :: Int -> Texturas -> Bool -> (Picture, Float)
drawNumero x t r = (Translate (-largura/2+d/2) 0 (Pictures pictures), largura)
    where
        d = fromIntegral dimensaoBloco
        algarismos = calculaAlgarismos x
        texturas = drawAlgarismos algarismos t r
        (pictures, largura) = drawNumeroAux texturas ([], 0)

drawNumeroAux :: [(PictureInfo,Picture)] -> ([Picture], Float) -> ([Picture], Float)
drawNumeroAux [] p = p
drawNumeroAux (((_, largura, altura), picture):t) (ps, ls) = drawNumeroAux t (psh, lsh + espaco)
    where
        l = fromIntegral largura
        a = fromIntegral altura
        d = fromIntegral dimensaoBloco
        p = Translate ls 0 $ Scale (d/a) (d/a) picture
        psh = ps ++ [p]
        lsh = ls + l*d/a
        espaco = if t == [] then 0 else calculaEspaco altura

calculaAlgarismos :: Int -> [Int]
calculaAlgarismos x = if x >= 10 then calculaAlgarismos (div x 10) ++ [(mod x 10)] else [x]

drawAlgarismos :: [Int] -> Texturas -> Bool -> [(PictureInfo, Picture)]
drawAlgarismos [] _ _ = []
drawAlgarismos (h:t) texturas r = procuraTexturaNumero h texturas r : drawAlgarismos t texturas r

procuraTexturaNumero :: Int -> Texturas -> Bool -> (PictureInfo, Picture)
procuraTexturaNumero x (pacotes, pID) r = texturaNumero
    where
        (mapTexturas, _) = pacotes !! pID
        texturaNumero = fromJust (Data.Map.lookup ((if r then TexturaNumeroR else TexturaNumero) x) mapTexturas)

calculaEspaco :: Int -> Float
calculaEspaco fatorEscala = 7*d/fe
    where
        fe = fromIntegral fatorEscala
        d = fromIntegral dimensaoBloco
