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
import Tarefa2_2021li1g032
import Tarefa4_2021li1g032


data Opcoes = Jogar
            | Creditos
            | Sair
    deriving (Eq)

data Windows = MainMenu Opcoes
             | PlayMenu Int
             | ModoJogo Jogo
             | VenceuJogo


type World = (Windows, Jogo)



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

draw :: World -> Picture
draw (MainMenu opcao, jogo) = drawMainMenu opcao
draw (PlayMenu nivel, _) = drawPlayMenu nivel
draw (ModoJogo jogo, _) = drawModoJogo jogo
draw (VenceuJogo, _) = drawVenceuJogo

drawVenceuJogo :: Picture
drawVenceuJogo = Translate (-200) 0 $ Color red $ Text "Victory!"

drawMainMenu :: Opcoes -> Picture
drawMainMenu op = Pictures [
    if op == Jogar then Color blue $ drawOptions "Play" else drawOptions "Play",
    if op == Creditos then Color blue $ Translate (0) (-50) $ drawOptions "Credits" else Translate (0) (-50) $ drawOptions "Credits",
    (if op == Sair then Color blue else id) $ Translate (0) (-100) $ drawOptions "Quit"]

drawPlayMenu :: Int -> Picture
-- drawPlayMenu atual = Translate (-800) 400 $ drawOptions ((show atual) ++ (show niveis) ++ (show y))
drawPlayMenu atual = Pictures (drawPlayMenuAux atual niveis y) 
    where
        niveis = [0,1,2]
        y = 50

drawPlayMenuAux :: Int -> [Int] -> Float -> [Picture]
drawPlayMenuAux _ [] _ = []
drawPlayMenuAux atual (h:t) y = ((if atual == h then Color blue else id) $ Translate 0 y $ drawOptions ("Level " ++ show h)) : drawPlayMenuAux atual t (y-50)

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

event :: Event -> World -> World
event ev ((MainMenu op), jogo) = eventMenu ev ((MainMenu op), jogo)
event ev ((PlayMenu n), jogo) = eventPlayMenu ev ((PlayMenu n), jogo)
event ev ((ModoJogo estadoJogo), jogo) = eventModoJogo ev ((ModoJogo estadoJogo), jogo)
event ev (VenceuJogo, jogo) = eventVenceuJogo ev (VenceuJogo, jogo)

eventMenu :: Event -> World -> World
eventMenu (EventKey (SpecialKey KeyUp) Down  _ _ ) (MainMenu Jogar, jogo) = (MainMenu Sair, jogo)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Creditos, jogo) = (MainMenu Jogar, jogo)
eventMenu (EventKey (SpecialKey KeyUp) Down _ _ ) (MainMenu Sair , jogo) = (MainMenu Creditos, jogo)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Jogar, jogo) = (MainMenu Creditos, jogo)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Creditos, jogo) = (MainMenu Sair, jogo)
eventMenu (EventKey (SpecialKey KeyDown) Down _ _ ) (MainMenu Sair, jogo) = (MainMenu Jogar, jogo)
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Sair, jogo) = undefined
eventMenu (EventKey (SpecialKey KeyEnter) Down _ _ ) (MainMenu Jogar, jogo) = (PlayMenu 0, jogo)
eventMenu _ w = w

eventVenceuJogo :: Event -> World -> World
eventVenceuJogo _ (VenceuJogo, jogo) = (MainMenu Jogar , jogo)

eventPlayMenu :: Event -> World -> World
eventPlayMenu (EventKey (SpecialKey k) Down  _ _ ) (PlayMenu n, jogo)
    | k == KeyUp = (PlayMenu (if n-1 < 0 then (length niveis) - 1 else n-1), jogo)
    | k == KeyDown = (PlayMenu (if n+1 >= (length niveis) then 0 else n+1), jogo)
    | k == KeyEnter = (ModoJogo nivel, nivel)
    | otherwise = (PlayMenu n, jogo)
    where
        niveis = [jogoFAQ1, jogoFAQ2, jogoFAQ3]
        nivel = niveis !! n
eventPlayMenu _ w = w

eventModoJogo :: Event -> World -> World
eventModoJogo (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoJogo estadoJogo, jogo) = (ModoJogo (moveJogador estadoJogo Trepar), jogo)
eventModoJogo (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoJogo estadoJogo, jogo) = (ModoJogo (moveJogador estadoJogo InterageCaixa), jogo)
eventModoJogo (EventKey (SpecialKey KeyLeft) Down _ _ ) (ModoJogo estadoJogo, jogo) = (ModoJogo (moveJogador estadoJogo AndarEsquerda), jogo)
eventModoJogo (EventKey (SpecialKey KeyRight) Down _ _ ) (ModoJogo estadoJogo, jogo) = (ModoJogo (moveJogador estadoJogo AndarDireita), jogo)
eventModoJogo (EventKey (Char r) Down _ _ ) (ModoJogo estadoJogo, jogo) = (ModoJogo jogo, jogo)
eventModoJogo _ (ModoJogo (Jogo mapa (Jogador c d b)), jogo)
    | acederPeca mapa c == Porta = (VenceuJogo, jogo)
    | otherwise = (ModoJogo (Jogo mapa (Jogador c d b)), jogo)



time :: Float -> World -> World
time _ w = w

estado :: World
estado = (MainMenu Jogar, jogoFAQ1)


main :: IO ()
main = do
    imagemBloco <- loadBMP "../assets/bloco.bmp"
    dimensoes <- getScreenSize
    play (window dimensoes) white fr estado draw event time
















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