{- |
Module      : Tarefa5_2021li1g032
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import LI12122
import Data.Map (Map, empty, insert, lookup)
import Tarefa5_types
import Tarefa2_2021li1g032
import Tarefa4_2021li1g032
import Data.Maybe

-- data Opcoes = Jogar
--             | Creditos
--             | Sair
--     deriving (Eq)

-- type Movimentos = Int
-- type NivelID = Int

-- data Janela = MainMenu Opcoes
--              | PlayMenu Int
--              | ModoJogo Jogo NivelID Movimentos
--              | VenceuJogo

-- type BlockDude = (Janela, Niveis, Texturas)

-- data Textura = TexturaBloco
--              | TexturaCaixa
--              | TexturaPorta
--              | TexturaFundo
--              | TexturaJogador Direcao
--              | TexturaEstrela

--     deriving (Eq, Ord)

-- type Texturas = ([Map Textura Picture], Int)

-- type JogosPorPagina = Int
-- type NivelAtual = Int

-- type MovimentosMin = Maybe Int
-- type MovimentosOtimos = Int
-- type Nivel = (Jogo, MovimentosMin, MovimentosOtimos)

-- type Niveis = ([Nivel], NivelAtual)


loadNiveis :: Niveis
loadNiveis = ([
    (jogoFAQ1, Nothing, 97), (jogoFAQ2, Nothing, 19), (jogoFAQ3, Nothing, 129),
    (jogoFAQ1, Nothing, 97), (jogoFAQ2, Nothing, 19), (jogoFAQ3, Nothing, 129)], 3)

dimensaoBloco :: Int
dimensaoBloco = 40

larguraJanela :: Int
larguraJanela = 720

alturaJanela :: Int
alturaJanela = 480

blocosLargura :: Int
blocosLargura = div larguraJanela dimensaoBloco

blocosAltura :: Int
blocosAltura = div alturaJanela dimensaoBloco

window :: Coordenadas -> Display
window (x,y) = InWindow "Block Dude" (larguraJanela, alturaJanela) (pontoX, pontoY)
    where
        centroX = div x 2
        centroY = div y 2
        pontoX = centroX - (div larguraJanela 2)
        pontoY = centroY - (div alturaJanela 2)

fr :: Int
fr = 25


type DimMapa = Coordenadas
type DimJanela = Coordenadas
type PosJogador = Coordenadas
type Camara = (Coordenadas, Coordenadas)

f :: DimMapa -> DimJanela -> PosJogador -> Camara
f (x1,y1) (x2,y2) (x3,y3) = ((o,n),(e,s))
    where
        o = x3 - (div x2 2)
        e = x3 + (div x2 2) + (mod x2 2) - 1
        n = y3 - (div y2 2) - (mod y2 2) + 1
        s = y3 + (div y2 2)

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

drawJogo :: Jogo -> [Picture]
drawJogo (Jogo mapa (Jogador c d b)) = (drawJogoAux m (Jogador c d b) camara) ++ (drawDEBUG dimMapa dimJanela c)
    where
        m = desconstroiMapa mapa
        dimMapa = maioresCoordenadas m
        dimJanela = (blocosLargura, blocosAltura)
        camara = calculaCamara dimMapa dimJanela c

drawDEBUG :: DimMapa -> DimJanela -> PosJogador -> [Picture]
drawDEBUG dimMapa dimJanela c = [
    (Translate (-800) 360 $ Color red $ Text ((show cam0) ++ (show cam))),
    (Translate (-800) (-360) $ Color red $ Text ((show c)++(show meio)))]
    where
        cam = calculaCamara dimMapa dimJanela c
        cam0 = f dimMapa dimJanela c
        meio = meioCamara cam


drawJogoAux :: [(Peca,Coordenadas)] -> Jogador -> Camara -> [Picture]
drawJogoAux [] jogador cam = drawPlayer jogador cam
drawJogoAux ((p,c):t) (Jogador (x,y) d temCaixa) cam
    | not naJanela || pecaTemJogador || pecaTemCaixa = drawJogoAux t (Jogador (x,y) d temCaixa) cam
    | otherwise = drawPeca (p,(px,-py)) : (drawJogoAux t (Jogador (x,y) d temCaixa) cam)
    where
        (px,py) = subtraiCoordenadas c (meioCamara cam)
        naJanela = estaNaJanela c cam
        pecaTemJogador = c == (x,y)
        pecaTemCaixa = temCaixa && (c == (x,y-1))

meioCamara :: Camara -> Coordenadas
meioCamara ((x1,y1),(x2,y2)) = (x1+(div (x2-x1) 2) + (mod (x2-x1) 2) , y1 + (div (y2-y1) 2))

estaNaJanela :: Coordenadas -> Camara -> Bool
estaNaJanela (x,y) ((minX, minY),(maxX, maxY)) = minX <= x && x <= maxX && minY <= y && y <= maxY

drawPeca :: (Peca, Coordenadas) -> Picture
drawPeca (Bloco, coord) = drawBloco coord
drawPeca (Caixa, coord) = drawCaixa coord
drawPeca (Porta, coord) = drawPorta coord

drawBloco :: Coordenadas -> Picture
-- drawBloco (x,y) = Translate i j imagemBloco
drawBloco (x,y) = polygon [(i,j), (i+d,j), (i+d,j+d), (i,j+d)]
    where
        d = fromIntegral dimensaoBloco
        i = (fromIntegral x) * 40
        j = (fromIntegral y) * 40

drawCaixa :: Coordenadas -> Picture
drawCaixa coord = Color blue (drawBloco coord)

drawPorta :: Coordenadas -> Picture
drawPorta coord = Color red (drawBloco coord)

drawPlayer :: Jogador -> Camara -> [Picture]
drawPlayer (Jogador c d temCaixa) cam = (Color green (drawTriangulo (x,-y) d)) : (if temCaixa then [drawCaixa (x,-y+1)] else [])
    where (x,y) = subtraiCoordenadas c (meioCamara cam)

drawTriangulo :: Coordenadas -> Direcao -> Picture
drawTriangulo (x,y) dir
    | dir == Este = polygon [(i,j), (i+d,j+(d/2)), (i,j+d)]
    | otherwise = polygon [(i+d,j), (i,j+(d/2)), (i+d,j+d)]
    where
        d = fromIntegral dimensaoBloco
        i = (fromIntegral x) * 40
        j = (fromIntegral y) * 40

subtraiCoordenadas :: Coordenadas -> Coordenadas -> Coordenadas
subtraiCoordenadas (x1, y1) (x2, y2) = (x1-x2, y1-y2)

{-
TESTES UNITARIOS FUNCAO G
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

draw :: BlockDude -> Picture
draw (MainMenu opcao, _, _) = drawMainMenu opcao
draw (PlayMenu nivel, niveis, texturas) = drawPlayMenu (PlayMenu nivel, niveis, texturas)
draw (ModoJogo jogo _ _, _, _) = drawModoJogo jogo
draw (VenceuJogo, _, _) = drawVenceuJogo

drawVenceuJogo :: Picture
drawVenceuJogo = Translate (-200) 0 $ Color red $ Text "Victory!"

drawMainMenu :: Opcoes -> Picture
drawMainMenu op = Pictures [
    if op == Jogar then Color blue $ drawOptions "Play" else drawOptions "Play",
    if op == Creditos then Color blue $ Translate (0) (-50) $ drawOptions "Credits" else Translate (0) (-50) $ drawOptions "Credits",
    (if op == Sair then Color blue else id) $ Translate (0) (-100) $ drawOptions "Quit"]

drawPlayMenu :: BlockDude -> Picture
drawPlayMenu (PlayMenu n, niveis, texturas) = Pictures (drawPlayMenuAux 0 (PlayMenu n, niveis, texturas))

drawPlayMenuAux :: NivelID -> BlockDude -> [Picture]
drawPlayMenuAux _ (_, ([], _), _) = []
drawPlayMenuAux i (PlayMenu n,((h:t), atual), texturas)
    | i == n = drawNivelRealcado h i texturas : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n-1 = drawNivelSecundario h i Oeste : drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)
    | i == n+1 = [drawNivelSecundario h i Este]
    | otherwise = drawPlayMenuAux (i+1) (PlayMenu n, (t, atual), texturas)


-- drawPlayMenuAux :: Int -> Int -> Niveis -> Float -> [Picture]
-- drawPlayMenuAux _ _ ([], _) _ = []
-- drawPlayMenuAux i n ((h:t), atual) y
--     | n == i = drawNivelRealcado h i y : drawPlayMenuAux (i+1) n (t, atual) (y-50)
--     | otherwise = drawNivelSecundario h i y : drawPlayMenuAux (i+1) n (t, atual) (y-50)

-- tirar o Int e o Float
drawNivelRealcado :: Nivel -> NivelID -> Texturas -> Picture
drawNivelRealcado (_, m, o) i (texturas, tID) = Pictures [nivelPicture, estrelaMeio, estrelaEsquerda, estrelaDireita, numNivel]
    where
        ndx = (fromIntegral larguraJanela) / 4.5
        ndy = (fromIntegral alturaJanela) / 4
        nivelPicture = Color blue $ polygon [(-ndx,-ndy), (ndx,-ndy), (ndx,ndy), (-ndx, ndy)]
        numNivel = Color white $ Scale (0.5) (0.5) $ Text (show i)
        ed = (fromIntegral dimensaoBloco) / 2
        mx = (fromIntegral larguraJanela) / 12
        my = -((fromIntegral alturaJanela) / 3)
        nEstrelas = numEstrelas m o
        mapTextura = texturas !! tID
        estrelaPreta = polygon [(ed,ed),(-ed,ed),(-ed,-ed),(ed,-ed)]
        estrela = fromJust (Data.Map.lookup TexturaEstrela mapTextura)
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

-- Translate 0 y $ drawOptions ("Level " ++ show i)

numEstrelas :: MovimentosMin -> MovimentosOtimos -> Int
numEstrelas Nothing _ = 0
numEstrelas m o
    | x == y = 3
    | x > y*4/3 = 1
    | otherwise = 2
    where
        x = fromIntegral (fromJust m)
        y = fromIntegral o

drawOptions :: String -> Picture
drawOptions option = Translate (-50) 0 $ Scale (0.3) (0.3) $ Text option

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

drawGrid :: [Picture]
drawGrid = (map linhaVertical xList) ++ (map linhaHorizontal yList)
    where
        xList = [(-x)..x]
        yList = [(-y)..y]
        x = (div blocosLargura 2)
        y = (div blocosAltura 2)


drawModoJogo :: Jogo -> Picture
drawModoJogo jogo = Pictures (drawJogo jogo ++ drawGrid)

-- TODO tirar as texturas
event :: Event -> BlockDude -> BlockDude
event ev ((MainMenu op), niveis, texturas) = eventMenu ev ((MainMenu op), niveis, texturas)
event ev ((PlayMenu n), niveis, texturas) = eventPlayMenu ev ((PlayMenu n), niveis, texturas)
event ev ((ModoJogo estadoJogo n m), niveis, texturas) = eventModoJogo ev ((ModoJogo estadoJogo n m), niveis, texturas)
event ev (VenceuJogo, niveis, texturas) = eventVenceuJogo ev (VenceuJogo, niveis, texturas)

eventMenu :: Event -> BlockDude -> BlockDude
eventMenu (EventKey (SpecialKey KeyUp) Down  _ _ ) (MainMenu Jogar, niveis, texturas) = (MainMenu Sair, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Creditos, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Sair , niveis, texturas) = (MainMenu Creditos, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Jogar, niveis, texturas) = (MainMenu Creditos, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Creditos, niveis, texturas) = (MainMenu Sair, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Sair, niveis, texturas) = (MainMenu Jogar, niveis, texturas)
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Sair, niveis, texturas) = undefined
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Jogar, (niveis, atual), texturas) = (PlayMenu atual, (niveis, atual), texturas)
eventMenu _ bd = bd

eventVenceuJogo :: Event -> BlockDude -> BlockDude
eventVenceuJogo _ (VenceuJogo, (niveis, atual), texturas) = (PlayMenu atual, (niveis, atual), texturas)

eventPlayMenu :: Event -> BlockDude -> BlockDude
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
eventModoJogo (EventKey (Char r) Down _ _ ) (ModoJogo estadoJogo n m, (niveis, atual), texturas) = (ModoJogo (fst3 (niveis !! n)) n 0, (niveis, atual), texturas)
eventModoJogo _ (ModoJogo (Jogo mapa (Jogador c d b)) n m, (niveis, atual), texturas)
    | acederPeca mapa c == Porta && atual == n = (VenceuJogo, (atualizaMovs m niveis n, atual+1), texturas)
    | acederPeca mapa c == Porta = (VenceuJogo, (atualizaMovs m niveis n, atual), texturas)
    | otherwise = (ModoJogo (Jogo mapa (Jogador c d b)) n m, (niveis, atual), texturas)

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


fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

time :: Float -> BlockDude -> BlockDude
time _ w = w

estado :: Niveis -> Texturas -> BlockDude
estado n t = (MainMenu Jogar, n, t)

loadTexturas :: [[Picture]] -> Texturas
loadTexturas [[b,e]] = ([mEstrela], 0)
    where
        mBloco = Data.Map.insert TexturaBloco b Data.Map.empty
        mEstrela = Data.Map.insert TexturaEstrela (Scale x x $ e) mBloco
        x = (fromIntegral dimensaoBloco) / 400

main :: IO ()
main = do
    imagemBloco <- loadBMP "../assets/bloco.bmp"
    imagemEstrela <- loadBMP "../assets/star.bmp"
    dimensoes <- getScreenSize
    play (window dimensoes) white fr (estado loadNiveis (loadTexturas [[imagemBloco, imagemEstrela]])) draw event time













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