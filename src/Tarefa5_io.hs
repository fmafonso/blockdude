{-# LANGUAGE DeriveGeneric #-}

module Tarefa5_io where

import GHC.Generics
import Data.Map
import Graphics.Gloss
import Data.Aeson
import Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe
import LI12122
import Tarefa5_types
import Tarefa2_2021li1g032
import Tarefa3_2021li1g032


data JogadorJSON = JogadorJSON {
      jx :: Int
    , jy  :: Int
    , direcao :: String
    , temCaixa :: Bool
    } deriving (Generic, Show)
instance ToJSON JogadorJSON
instance FromJSON JogadorJSON

data MapaJSON = MapaJSON {
    peca :: String
    , px :: Int
    , py :: Int
} deriving (Generic, Show)
instance ToJSON MapaJSON
instance FromJSON MapaJSON

data JogoJSON = JogoJSON {
    mapa :: [MapaJSON]
    , jogador :: JogadorJSON
} deriving (Generic, Show)
instance ToJSON JogoJSON
instance FromJSON JogoJSON

data NivelJSON = NivelJSON {
    jogo :: JogoJSON
    ,movMinimos :: Maybe Int
    , movOtimos :: Int
} deriving (Generic, Show)
instance ToJSON NivelJSON
instance FromJSON NivelJSON

data NiveisJSON = NiveisJSON {
    niveisLista :: [NivelJSON]
    , nivelAtual :: Int
} deriving (Generic, Show)
instance ToJSON NiveisJSON
instance FromJSON NiveisJSON

data TexturaJSON = TexturaJSON {
    textura :: String
    , ficheiro :: String
    , largura :: Int
    , altura :: Int
} deriving (Generic, Show)
instance ToJSON TexturaJSON
instance FromJSON TexturaJSON

data ColorJSON = ColorJSON {
    r :: Int
    , g :: Int
    , b :: Int
} deriving (Generic, Show)
instance ToJSON ColorJSON
instance FromJSON ColorJSON

data PackTexturasJSON = PackTexturasJSON {
    listaTexturas :: [TexturaJSON]
    , fundo :: ColorJSON
} deriving (Generic, Show)
instance ToJSON PackTexturasJSON
instance FromJSON PackTexturasJSON

data TexturasJSON = TexturasJSON {
    pacotesTexturas :: [PackTexturasJSON]
    , pacoteAtual :: Int
} deriving (Generic, Show)
instance ToJSON TexturasJSON
instance FromJSON TexturasJSON

data BlockDudeJSON = BlockDudeJSON {
    niveis :: NiveisJSON
    , texturas :: TexturasJSON
} deriving (Generic, Show)
instance ToJSON BlockDudeJSON
instance FromJSON BlockDudeJSON



loadDirecao :: String -> Direcao
loadDirecao "Este" = Este
loadDirecao "Oeste" = Oeste

saveDirecao :: Direcao -> String
saveDirecao Este = "Este"
saveDirecao Oeste = "Oeste"

loadPeca :: String -> Peca
loadPeca "Vazio" = Vazio
loadPeca "Bloco" = Bloco
loadPeca "Caixa" = Caixa
loadPeca "Porta" = Porta

savePeca :: Peca -> String
savePeca Vazio = "Vazio"
savePeca Bloco = "Bloco"
savePeca Caixa = "Caixa"
savePeca Porta = "Porta"

loadTextura :: String -> Textura
loadTextura "caixa" = TexturaCaixa
loadTextura "porta" = TexturaPorta
loadTextura "bloco" = TexturaBloco
loadTextura "jogadorOeste" = TexturaJogador Oeste False
loadTextura "jogadorEste" = TexturaJogador Este False
loadTextura "jogadorCaixaOeste" = TexturaJogador Oeste True
loadTextura "jogadorCaixaEste" = TexturaJogador Este True
loadTextura "estrela" = TexturaEstrela
loadTextura "semEstrela" = TexturaSemEstrela
loadTextura "playR" = TexturaPlayR
loadTextura "play" = TexturaPlay
loadTextura "guardarR" = TexturaGuardarR
loadTextura "guardar" = TexturaGuardar
loadTextura "definicoesR" = TexturaDefinicoesR
loadTextura "definicoes" = TexturaDefinicoes
loadTextura "creditosR" = TexturaCreditosR
loadTextura "creditos" = TexturaCreditos
loadTextura "sairR" = TexturaSairR
loadTextura "sair" = TexturaSair


saveTextura :: Textura -> String
saveTextura TexturaCaixa = "caixa"
saveTextura TexturaPorta = "porta"
saveTextura TexturaBloco = "bloco"
saveTextura (TexturaJogador Oeste False) = "jogadorOeste"
saveTextura (TexturaJogador Este False) = "jogadorEste"
saveTextura (TexturaJogador Oeste True) = "jogadorCaixaOeste"
saveTextura (TexturaJogador Este True) = "jogadorCaixaEste"
saveTextura TexturaEstrela = "estrela"
saveTextura TexturaSemEstrela = "semEstrela"
saveTextura TexturaPlayR = "playR"
saveTextura TexturaPlay = "play"
saveTextura TexturaGuardarR = "guardarR"
saveTextura TexturaGuardar = "guardar"
saveTextura TexturaDefinicoesR = "definicoesR"
saveTextura TexturaDefinicoes = "definicoes"
saveTextura TexturaCreditosR = "creditosR"
saveTextura TexturaCreditos = "creditos"
saveTextura TexturaSairR = "sairR"
saveTextura TexturaSair = "sair"

loadCor :: ColorJSON -> Color
loadCor json = makeColorI (r json) (g json) (b json) 255

saveCor :: Color -> ColorJSON
saveCor color = ColorJSON {r = round (red*255), g = round (green*255), b = round (blue*255)}
    where
        (red, green, blue, _) = rgbaOfColor color

loadJogador :: JogadorJSON -> Jogador
loadJogador json = Jogador (jx json, jy json) (loadDirecao (direcao json)) (temCaixa json)

saveJogador :: Jogador -> JogadorJSON
saveJogador (Jogador (x,y) d b) = JogadorJSON {jx = x, jy = y, direcao = saveDirecao d, temCaixa = b}

loadPecaCoord :: MapaJSON -> (Peca, Coordenadas)
loadPecaCoord json = (loadPeca (peca json), (px json, py json))

savePecaCoord :: (Peca, Coordenadas) -> MapaJSON
savePecaCoord (p, (x,y)) = MapaJSON {peca = savePeca p, px = x, py = y}

loadMapa :: [MapaJSON] -> Mapa
loadMapa json = constroiMapa $ Prelude.map loadPecaCoord json

saveMapa :: Mapa -> [MapaJSON]
saveMapa mapa = Prelude.map savePecaCoord (desconstroiMapa mapa)

loadJogo :: JogoJSON -> Jogo
loadJogo json = Jogo (loadMapa (mapa json)) (loadJogador (jogador json))

saveJogo :: Jogo -> JogoJSON
saveJogo (Jogo m j) = JogoJSON {mapa = saveMapa m, jogador = saveJogador j}

loadNivel :: NivelJSON -> Nivel
loadNivel json = (loadJogo (jogo json), movMinimos json, movOtimos json)

saveNivel :: Nivel -> NivelJSON
saveNivel (j, min, opt) = NivelJSON {jogo = saveJogo j, movMinimos = min, movOtimos = opt}

loadNiveis :: NiveisJSON -> Niveis
loadNiveis json = (Prelude.map loadNivel (niveisLista json), nivelAtual json)

saveNiveis :: Niveis -> NiveisJSON
saveNiveis (n, a) = NiveisJSON {niveisLista = Prelude.map saveNivel n, nivelAtual = a}

loadTexturaPicture :: TexturaJSON -> IO (Textura, (PictureInfo, Picture))
loadTexturaPicture json = do
    picture <- loadBMP (ficheiro json)
    let d = fromIntegral dimensaoBloco
        a = fromIntegral (altura json)
        l = fromIntegral (largura json)
        info = (ficheiro json, largura json, altura json)
    return ((loadTextura (textura json)), (info, picture))

saveTexturaPicture :: (Textura, (PictureInfo, Picture)) -> TexturaJSON
saveTexturaPicture (t, ((f, l, a), _)) = TexturaJSON {textura = saveTextura t, ficheiro = f, largura = l, altura = a}

loadPacoteTexturasAux :: [TexturaJSON] -> IO (Map Textura (PictureInfo, Picture))
loadPacoteTexturasAux [] = do
    return Data.Map.empty
loadPacoteTexturasAux (h:t) = do
    tail <- loadPacoteTexturasAux t
    (textura, (info, picture)) <- loadTexturaPicture h
    return (Data.Map.insert textura (info, picture) tail)

loadPacoteTexturas :: PackTexturasJSON -> IO (Map Textura (PictureInfo, Picture), Color)
loadPacoteTexturas json = do
    texturas <- loadPacoteTexturasAux (listaTexturas json)
    return (texturas, loadCor (fundo json))

savePacoteTexturas :: (Map Textura (PictureInfo, Picture), Color) -> PackTexturasJSON
savePacoteTexturas (m, c) = PackTexturasJSON {listaTexturas = t, fundo = f}
    where
        t = Prelude.map saveTexturaPicture (toList m)
        f = saveCor c

loadPacotesTexturas :: [PackTexturasJSON] -> IO [(Map Textura (PictureInfo, Picture), Color)]
loadPacotesTexturas [] = do
    return []
loadPacotesTexturas (h:t) = do
    tail <- loadPacotesTexturas t
    head <- loadPacoteTexturas h
    return ((head:tail))

savePacotesTexturas :: [(Map Textura (PictureInfo, Picture), Color)] -> [PackTexturasJSON]
savePacotesTexturas l = Prelude.map savePacoteTexturas l

loadTexturas :: TexturasJSON -> IO Texturas
loadTexturas json = do
    pacotes <- loadPacotesTexturas (pacotesTexturas json)
    return (pacotes, pacoteAtual json)

saveTexturas :: Texturas -> TexturasJSON
saveTexturas (l, i) = TexturasJSON {pacotesTexturas = savePacotesTexturas l, pacoteAtual = i}

loadBlockDudeAux :: Maybe BlockDudeJSON -> IO (Maybe BlockDude)
loadBlockDudeAux Nothing = do
    return Nothing
loadBlockDudeAux (Just json) = do
    texturas <- loadTexturas (texturas json)
    let nvs = loadNiveis (niveis json)
    return (Just (MainMenu Jogar, nvs, texturas))

loadBlockDude :: IO (Maybe BlockDude)
loadBlockDude = do
    byteString <- Char8.readFile "../data/blockdude.json"
    blockDude <- loadBlockDudeAux (decode byteString :: Maybe BlockDudeJSON)
    return blockDude

saveBlockDudeAux :: BlockDude -> BlockDudeJSON
saveBlockDudeAux (_, n, t) = BlockDudeJSON {niveis = saveNiveis n,texturas = saveTexturas t}

saveBlockDude :: BlockDude -> IO ()
saveBlockDude blockdude = do
    let json = saveBlockDudeAux blockdude
    Char8.writeFile "../data/blockdude.json" (encode json)



-- main = do
--     blockdude <- loadBlockDude
--     saveBlockDude (fromJust blockdude)

