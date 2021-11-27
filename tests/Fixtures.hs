module Fixtures where

import LI12122
import LI12122 (Jogo, Mapa)
import GHC.ByteCode.Types (BCONPtr)

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Caixa, (4, 3)),
    (Bloco, (4, 4)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m2 :: [(Peca, Coordenadas)]
m2 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 3)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (4, 2)),
    (Caixa, (4, 3)),
    (Bloco, (4, 4)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4))
  ]

m2r :: Mapa
m2r= 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Porta, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco]
  ]

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (5, 3) Este True)

m2e2 :: Jogo
m2e2 = Jogo m2r (Jogador (4, 1) Oeste False)

m3 :: [(Peca, Coordenadas)]
m3 =
  [ (Bloco, (0, 3)),
    (Caixa, (1, 2)),
    (Bloco, (1, 3)),
    (Caixa, (2, 2)),
    (Bloco, (2, 3)),
    (Caixa, (3, 2)),
    (Bloco, (3, 3)),
    (Bloco, (4, 1)),
    (Bloco, (4, 3)),
    (Porta, (5, 0)),
    (Bloco, (5, 1)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m3r :: Mapa
m3r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
    [Vazio, Caixa, Caixa, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3e1 :: Jogo
m3e1 = Jogo m3r (Jogador (0, 2) Este False)

m3e2 :: Jogo
m3e2 = Jogo m3r (Jogador (4, 2) Oeste False)