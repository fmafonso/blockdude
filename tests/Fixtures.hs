module Fixtures where

import LI12122
import GHC.ByteCode.Types (BCONPtr)

-- .......
-- ......X
-- ......X
-- P...C.X
-- XXXXXXX

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
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3)),
    (Bloco, (6, 4))
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


-- .......
-- .......
-- ....X..
-- PX..C..
-- XXXXXXX

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

m3s :: String
m3s =
  "       \n\
  \     PX\n\
  \    XXX\n\
  \ CCC  X\n\
  \XXXXXXX"

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


--Mapa 1.1 do enunciado

m5s :: String
m5s =
  "                \n\
  \                \n\
  \                \n\
  \                \n\
  \X               \n\
  \X               \n\
  \X               \n\
  \X  X C     X    \n\
  \XP X < X   XC   \n\
  \XXXXXXXXXXXXXXXX"

m5 :: [(Peca, Coordenadas)]
m5 =
  [ (Bloco, (0, 4)),
    (Bloco, (0, 5)),
    (Bloco, (0, 6)),
    (Bloco, (0, 7)),
    (Bloco, (0, 8)),
    (Bloco, (0, 9)),
    (Porta, (1, 8)),
    (Bloco, (1, 9)),
    (Bloco, (2, 9)),
    (Bloco, (3, 7)),
    (Bloco, (3, 8)),
    (Bloco, (3, 9)),
    (Bloco, (4, 9)),
    (Bloco, (5, 9)),
    (Bloco, (6, 9)),
    (Bloco, (7, 8)),
    (Bloco, (7, 9)),
    (Bloco, (8, 9)),
    (Bloco, (9, 9)),
    (Bloco, (10, 9)),
    (Bloco, (11, 7)),
    (Bloco, (11, 8)),
    (Bloco, (11, 9)),
    (Caixa, (12, 8)),
    (Bloco, (12, 9)),
    (Bloco, (13, 9)),
    (Bloco, (14, 9)),
    (Bloco, (15, 9))
  ]

m5r :: Mapa
m5r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m5e1 :: Jogo
m5e1 = Jogo m5r (Jogador (5, 8) Oeste True)


--Mapas do 1.1.1
-- a

m6s :: String
m6s = 
  "P <   \n\
  \XXXXXX"

m6 :: [(Peca, Coordenadas)]
m6 =
  [ (Porta, (0, 0)),
    (Bloco, (0, 1)),
    (Bloco, (1, 1)),
    (Bloco, (2, 1)),
    (Bloco, (3, 1)),
    (Bloco, (4, 1)),
    (Bloco, (5, 1))
  ]

m6r :: Mapa
m6r =
  [ [Porta, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m6e1 :: Jogo
m6e1 = Jogo m6r (Jogador (2, 0) Oeste False)

-- b
m7s :: String
m7s =
  "P X<  \n\
  \XXXXXX"

m7 :: [(Peca, Coordenadas)]
m7 =
  [ (Porta, (0, 0)),
    (Bloco, (0, 1)),
    (Bloco, (1, 1)),
    (Bloco, (2, 0)),
    (Bloco, (2, 1)),
    (Bloco, (3, 1)),
    (Bloco, (4, 1)),
    (Bloco, (5, 1))
  ]

m7r :: Mapa
m7r =
  [ [Porta, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m7e1 :: Jogo
m7e1 = Jogo m7r (Jogador (3, 0) Oeste False)

-- c e d
m8s :: String
m8s =
  "     <\n\
  \     X\n\
  \     X\n\
  \     X\n\
  \P    X\n\
  \XXXXXX"


m8 :: [(Peca,Coordenadas)]
m8 =
  [ (Porta, (0, 4)),
    (Bloco, (0, 5)),
    (Bloco, (1, 5)),
    (Bloco, (2, 5)),
    (Bloco, (3, 5)),
    (Bloco, (4, 5)),
    (Bloco, (5, 1)),
    (Bloco, (5, 2)),
    (Bloco, (5, 3)),
    (Bloco, (5, 4)),
    (Bloco, (5, 5))
  ]

m8r :: Mapa
m8r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m8e1 :: Jogo
m8e1 = Jogo m8r (Jogador (5, 0) Oeste False)

m8e2 :: Jogo
m8e2 = Jogo m8r (Jogador (4, 4) Oeste False)

--Mapas do 1.1.2
-- a e b
m9 :: [(Peca,Coordenadas)]
m9 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m9r :: Mapa
m9r = 
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ]

m9e1s :: String
m9e1s =
  "      X\n\
  \      X\n\
  \P X>  X\n\
  \XXXXXXX"

m9e1 :: Jogo
m9e1 = Jogo m9r (Jogador (3, 2) Este False)

m9e2s :: String
m9e2s =
  "      X\n\
  \      X\n\
  \P X < X\n\
  \XXXXXXX"

m9e2 :: Jogo
m9e2 = Jogo m9r (Jogador (4, 2) Oeste False)

-- c
m10s :: String
m10s =
  "      X\n\
  \  X   X\n\
  \P X< CX\n\
  \XXXXXXX"

m10 :: [(Peca,Coordenadas)]
m10 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Caixa, (5, 2)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m10r :: Mapa
m10r =
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
      [Porta, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco],
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ]

m10e1 :: Jogo
m10e1 = Jogo m10r (Jogador (3, 2) Oeste False)

-- d
m11s :: String
m11s =
  "  X   X\n\
  \      X\n\
  \P X< CX\n\
  \XXXXXXX"

m11 :: [(Peca,Coordenadas)]
m11 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 0)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Caixa, (5, 2)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m11r :: Mapa
m11r =
  [ [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m11e1 :: Jogo
m11e1 = Jogo m11r (Jogador (3, 2) Oeste False)

-- e e f
m12 :: [(Peca,Coordenadas)]
m12 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Caixa, (3, 2)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m12r :: Mapa
m12r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m12e1 :: Jogo
m12e1 = Jogo m12r (Jogador (4, 2) Oeste False)

m12e2 :: Jogo
m12e2 = Jogo m12r (Jogador (3, 1) Oeste False)

-- Mapas do 1.1.3
-- a
m13 :: [(Peca,Coordenadas)]
m13 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Caixa, (5, 2)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m13r :: Mapa
m13r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m13e1 :: Jogo
m13e1 = Jogo m13r (Jogador (4, 2) Este False)

-- b
m14 :: [(Peca,Coordenadas)]
m14 =
  [ (Porta, (0, 2)),
    (Bloco, (0, 3)),
    (Bloco, (1, 3)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (2, 3)),
    (Bloco, (3, 3)),
    (Bloco, (4, 3)),
    (Bloco, (5, 3)),
    (Bloco, (6, 0)),
    (Bloco, (6, 1)),
    (Bloco, (6, 2)),
    (Bloco, (6, 3))
  ]

m14r :: Mapa
m14r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m14e1 :: Jogo
m14e1 = Jogo m14r (Jogador (4, 2) Este True)

-- c
m15 :: [(Peca, Coordenadas)]
m15 =
  [ (Porta, (0, 0)),
    (Bloco, (0, 1)),
    (Bloco, (1, 1)),
    (Caixa, (2, 0)),
    (Bloco, (2, 1)),
    (Bloco, (3, 1)),
    (Bloco, (4, 1)),
    (Bloco, (5, 1))
  ]

m15r :: Mapa
m15r =
  [ [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m15e1 :: Jogo
m15e1 = Jogo m15r (Jogador (3, 0) Este False)

-- d
m16 :: [(Peca, Coordenadas)]
m16 =
  [ (Porta, (0, 1)),
    (Bloco, (0, 2)),
    (Bloco, (1, 2)),
    (Caixa, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (3, 0)),
    (Bloco, (3, 2)),
    (Bloco, (4, 2)),
    (Bloco, (5, 2))
  ]

m16r :: Mapa
m16r =
  [ [Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m16e1 :: Jogo
m16e1 = Jogo m16r (Jogador (3, 1) Oeste False)

-- e
m17 :: [(Peca, Coordenadas)]
m17 =
  [ (Porta, (0, 1)),
    (Bloco, (0, 2)),
    (Bloco, (1, 2)),
    (Bloco, (2, 0)),
    (Caixa, (2, 1)),
    (Bloco, (2, 2)),
    (Bloco, (3, 2)),
    (Bloco, (4, 2)),
    (Bloco, (5, 2))
  ]

m17r :: Mapa
m17r =
  [ [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m17e1 :: Jogo
m17e1 = Jogo m17r (Jogador (3, 1) Oeste False)
