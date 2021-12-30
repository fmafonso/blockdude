module Tarefa5_types where

import LI12122


data Opcoes = Jogar
            | Creditos
            | Sair
    deriving (Eq)

type Movimentos = Int
type NivelID = Int

data Janela = MainMenu Opcoes
             | PlayMenu Int
             | ModoJogo Jogo NivelID Movimentos
             | VenceuJogo

type BlockDude = (Janela, Niveis, Texturas)

data Textura = TexturaBloco
             | TexturaCaixa
             | TexturaPorta
             | TexturaFundo
             | TexturaJogador Direcao
             | TexturaEstrela

    deriving (Eq, Ord)

type Texturas = ([Map Textura Picture], Int)

type JogosPorPagina = Int
type NivelAtual = Int

type MovimentosMin = Maybe Int
type MovimentosOtimos = Int
type Nivel = (Jogo, MovimentosMin, MovimentosOtimos)

type Niveis = ([Nivel], NivelAtual)