{- |
Module      : Tarefa5_types
Description : Tipos de dados para Tarefa 5
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa5_types where

import LI12122
import Data.Map
import Graphics.Gloss
import Tarefa3_2021li1g032


data Opcoes = Jogar
            | Guardar
            | OpcaoDefinicoes
            | Creditos
            | Sair
    deriving (Eq, Ord, Show)

type Movimentos = Int
type NivelID = Int

data Definicoes = PackTexturas
                | Voltar
    deriving (Eq, Ord, Show)

data Janela = MainMenu Opcoes
             | PlayMenu Int
             | ModoJogo Jogo NivelID Movimentos
             | VenceuJogo
             | GuardaBlockDude
             | MenuCreditos
             | MenuDefinicoes Definicoes
    deriving (Eq, Ord, Show)

type BlockDude = (Janela, Niveis, Texturas)

data Textura = TexturaBloco
             | TexturaCaixa
             | TexturaPorta
             | TexturaFundo
             | TexturaJogador Direcao Bool
             | TexturaEstrela
             | TexturaSemEstrela
             | TexturaPlayR
             | TexturaPlay
             | TexturaGuardarR
             | TexturaGuardar
             | TexturaDefinicoesR
             | TexturaDefinicoes
             | TexturaCreditosR
             | TexturaCreditos
             | TexturaSairR
             | TexturaSair
             | TexturaPackR
             | TexturaPack
             | TexturaBackR
             | TexturaBack
             | TexturaNivel
             | TexturaVenceu
             | TexturaGuardou
             | TexturaCreditosM
             | TexturaNumeroR Int
             | TexturaNumero Int
    deriving (Eq, Ord, Show)

type Texturas = ([(Map Textura (PictureInfo, Picture), Color)], Int)

type Largura = Int
type Altura = Int
type PictureInfo = (String, Largura, Altura)

type JogosPorPagina = Int
type NivelAtual = Int

type MovimentosMin = Maybe Int
type MovimentosOtimos = Int
type Nivel = (Jogo, MovimentosMin, MovimentosOtimos)

type Niveis = ([Nivel], NivelAtual)

type DimMapa = Coordenadas
type DimJanela = Coordenadas
type PosJogador = Coordenadas
type Camera = (Coordenadas, Coordenadas)


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
