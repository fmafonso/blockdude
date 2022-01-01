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
drawAux (MainMenu opcao, _, texturas) = return (drawMainMenu opcao texturas)
drawAux (PlayMenu nivel, niveis, texturas) = return (drawPlayMenu (PlayMenu nivel, niveis, texturas))
drawAux (ModoJogo jogo _ _, _, t) = return (drawModoJogo jogo t)
drawAux (VenceuJogo, _, _) = return drawVenceuJogo
drawAux (GuardaBlockDude, _, _) = return drawGuardaBlockDude
drawAux (MenuDefinicoes definicao, _, t) = return (drawMenuDefinicoes definicao t)

----------------------------------------------------------------

drawMainMenu :: Opcoes -> Texturas -> Picture
drawMainMenu op (pacotes, pID) = Pictures [
    Translate (0) (a*5/24) $ drawMenuOption play,
    Translate (0) (a*5/48) $ drawMenuOption guardar,
    drawMenuOption definicoes,
    Translate (0) (-(a*5/48)) $ drawMenuOption creditos,
    Translate (0) (-(a*5/24)) $ drawMenuOption sair]
    where
        a = fromIntegral alturaJanela
        (mapTextura, _) = pacotes !! pID
        play = fromJust (Data.Map.lookup (if op == Jogar then TexturaPlayR else TexturaPlay) mapTextura)
        guardar = fromJust (Data.Map.lookup (if op == Guardar then TexturaGuardarR else TexturaGuardar) mapTextura)
        definicoes = fromJust (Data.Map.lookup (if op == OpcaoDefinicoes then TexturaDefinicoesR else TexturaDefinicoes) mapTextura)
        creditos = fromJust (Data.Map.lookup (if op == Creditos then TexturaCreditosR else TexturaCreditos) mapTextura)
        sair = fromJust (Data.Map.lookup (if op == Sair then TexturaSairR else TexturaSair) mapTextura)

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
    | i == n = drawNivelRealcado h i texturas : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n-1 = drawNivelSecundario h i Oeste : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n+1 = [drawNivelSecundario h i Este]
    | otherwise = drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)

-- separar em drawJogo drawEstrelas
drawNivelRealcado :: Nivel -> NivelID -> Texturas -> Picture
drawNivelRealcado n id t = Pictures [drawEstrelas n t, drawNivel n t, drawNivelID id t]

drawNivelID :: NivelID -> Texturas -> Picture
drawNivelID id (texturas, tID) = Translate 0 (h*3/10) $ Scale (0.5) (0.5) $ Text (show id)
    where h = fromIntegral alturaJanela

drawNivel :: Nivel -> Texturas -> Picture
drawNivel (j, _, _) t = Scale (4/9) (1/2) $ Pictures ([drawModoJogo j t] ++ drawGrid)

drawEstrelas :: Nivel -> Texturas -> Picture
drawEstrelas (_, m, o) (pacotes, tID) = Pictures [estrelaEsquerda, estrelaMeio, estrelaDireita]
    where
        ed = (fromIntegral dimensaoBloco) / 2
        mx = (fromIntegral larguraJanela) / 12
        my = -((fromIntegral alturaJanela) / 3)
        nEstrelas = numEstrelas m o
        (mapTextura, _) = pacotes !! tID
        estrelaPreta = snd (fromJust (Data.Map.lookup TexturaSemEstrela mapTextura))
        dimensao = fromIntegral dimensaoBloco
        estrela = snd (fromJust (Data.Map.lookup TexturaEstrela mapTextura))
        estrelaEsquerda = Translate (-mx) my $ if nEstrelas > 0 then estrela else estrelaPreta
        estrelaMeio = Translate 0 my $ if nEstrelas > 1 then estrela else estrelaPreta
        estrelaDireita = Translate mx my $ if nEstrelas > 2 then estrela else estrelaPreta



drawNivelSecundario :: Nivel -> NivelID -> Direcao -> Picture
drawNivelSecundario _ i dir = Pictures [nivelPicture, numNivel]
    where
        dx = (fromIntegral larguraJanela) * 13 / 36
        mx = if dir == Este then dx else -dx
        d = (fromIntegral alturaJanela) / 6
        nivelPolygon = polygon [(-d,-d), (d,-d), (d,d), (-d,d)]
        nivelPicture = Color red $ Translate mx 0 $ nivelPolygon
        numNivel = Color white $ Translate mx 0 $ Scale (0.5) (0.5) $ Text (show i)

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
        (mapTextura, _) = pacotes !! tID
        bloco = fromJust (Data.Map.lookup TexturaBloco mapTextura)

drawCaixa :: Coordenadas -> Texturas -> Picture
drawCaixa coord (pacotes, tID) = drawSquare coord caixa
    where
        (mapTextura, _) = pacotes !! tID
        caixa = fromJust (Data.Map.lookup TexturaCaixa mapTextura)

drawPorta :: Coordenadas -> Texturas -> Picture
drawPorta coord (pacotes, tID) = drawSquare coord porta
    where
        (mapTextura, _) = pacotes !! tID
        porta = fromJust (Data.Map.lookup TexturaPorta mapTextura)

drawPlayer :: Jogador -> Camara -> Texturas -> [Picture]
drawPlayer (Jogador c d temCaixa) cam (pacotes, tID) = (drawSquare (x,-y) jogador) : (if temCaixa then [drawCaixa (x,-y+1) (pacotes, tID)] else [])
    where
        (x,y) = subtraiCoordenadas c (meioCamara cam)
        (mapTextura, _) = pacotes !! tID
        jogador = fromJust (Data.Map.lookup (TexturaJogador d temCaixa) mapTextura)

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


drawGrid :: [Picture]
drawGrid = (map linhaVertical xList) ++ (map linhaHorizontal yList)
    where
        xList = [(-x),x]
        yList = [(-y),y]
        x = (div blocosLargura 2)
        y = (div blocosAltura 2)

linhaHorizontal :: Int -> Picture
linhaHorizontal y = line [(-i, j), (i,j)]
    where
        i = fromIntegral (div larguraJanela 2)
        j = fromIntegral (y * dimensaoBloco)

linhaVertical :: Int -> Picture
linhaVertical x = line [(i, -j), (i, j)]
    where
        i = fromIntegral (x * dimensaoBloco)
        j = fromIntegral (div alturaJanela 2)

----------------------------------------------------------------

drawVenceuJogo :: Picture
drawVenceuJogo = Translate (-200) 0 $ Color red $ Text "Victory!"

----------------------------------------------------------------

drawGuardaBlockDude :: Picture
drawGuardaBlockDude = Translate (-200) 0 $ Color red $ Text "Saved!"

----------------------------------------------------------------

drawMenuDefinicoes :: Definicoes -> Texturas -> Picture
drawMenuDefinicoes d t = Pictures [
    (if d == PackTexturas then Color blue else id) $ drawOptions ("Texture Pack " ++ show (snd t)),
    (if d == Voltar then Color blue else id) $ Translate 0 (-50) $ drawOptions "Back"]















drawOptions :: String -> Picture
drawOptions option = Translate (-50) 0 $ Scale (0.3) (0.3) $ Text option