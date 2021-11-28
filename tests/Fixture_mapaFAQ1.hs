module Fixture_mapaFAQ1 where

import LI12122


mapaFAQ1s :: String
mapaFAQ1s =
  " X                 \n\
  \ X   XXXXXXXXXXXXX \n\
  \X X X             X\n\
  \X  X              X\n\
  \X                CX\n\
  \X               CCX\n\
  \X XXX    <   XC XX \n\
  \X X X    X  XXXXX  \n\
  \X X XCC XX  X      \n\
  \XPX XXXXXX XX      \n\
  \XXX XX   XXX       "


mapaFAQ1 :: [(Peca,Coordenadas)]
mapaFAQ1 =
  [ (Bloco,(0,2)), (Bloco,(0,3)), (Bloco,(0,4)), (Bloco,(0,5)), (Bloco,(0,6)),
    (Bloco,(0,7)), (Bloco,(0,8)), (Bloco,(0,9)), (Bloco,(0,10)), (Bloco,(1,0)),
    (Bloco,(1,1)), (Porta,(1,9)), (Bloco,(1,10)), (Bloco,(2,2)), (Bloco,(2,6)),
    (Bloco,(2,7)), (Bloco,(2,8)), (Bloco,(2,9)), (Bloco,(2,10)), (Bloco,(3,3)),
    (Bloco,(3,6)), (Bloco,(4,2)), (Bloco,(4,6)), (Bloco,(4,7)), (Bloco,(4,8)),
    (Bloco,(4,9)), (Bloco,(4,10)), (Bloco,(5,1)), (Caixa,(5,8)), (Bloco,(5,9)),
    (Bloco,(5,10)), (Bloco,(6,1)), (Caixa,(6,8)), (Bloco,(6,9)), (Bloco,(7,1)),
    (Bloco,(7,9)), (Bloco,(8,1)), (Bloco,(8,8)), (Bloco,(8,9)), (Bloco,(9,1)),
    (Bloco,(9,7)), (Bloco,(9,8)), (Bloco,(9,9)), (Bloco,(9,10)), (Bloco,(10,1)),
    (Bloco,(10,10)), (Bloco,(11,1)), (Bloco,(11,9)), (Bloco,(11,10)), (Bloco,(12,1)),
    (Bloco,(12,7)), (Bloco,(12,8)), (Bloco,(12,9)), (Bloco,(13,1)), (Bloco,(13,6)),
    (Bloco,(13,7)), (Bloco,(14,1)), (Caixa,(14,6)), (Bloco,(14,7)), (Bloco,(15,1)),
    (Bloco,(15,7)), (Bloco,(16,1)), (Caixa,(16,5)), (Bloco,(16,6)), (Bloco,(16,7)),
    (Bloco,(17,1)), (Caixa,(17,4)), (Caixa,(17,5)), (Bloco,(17,6)), (Bloco,(18,2)),
    (Bloco,(18,3)), (Bloco,(18,4)), (Bloco,(18,5))
  ]

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

tapaBuracoM1 = [AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,InterageCaixa]
fazDegrauM1 = [AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,InterageCaixa]
moveCaixaDireitaM1 = [Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarDireita,InterageCaixa]
fazPassagemMeioM1 = [Trepar,Trepar,InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa]
movRepEsquerdaM1 = [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
movRepDireitaM1 = [AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita]
pegaEscada1M1 = [AndarDireita,Trepar,Trepar,AndarDireita,Trepar,InterageCaixa]
poeEscada1M1 = movRepEsquerdaM1 ++ [AndarEsquerda,InterageCaixa]
pegaEscada2M1 = movRepDireitaM1 ++ [Trepar,AndarDireita,InterageCaixa]
poeEscada2M1 = [AndarEsquerda] ++ movRepEsquerdaM1 ++ [InterageCaixa]
pegaEscada3M1 = movRepDireitaM1 ++ [InterageCaixa]
poeEscada3M1 = movRepEsquerdaM1 ++ [Trepar,InterageCaixa]
fimMapa1 = [Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda]
movimentosM1 = tapaBuracoM1++fazDegrauM1++moveCaixaDireitaM1++fazPassagemMeioM1++pegaEscada1M1++poeEscada1M1++pegaEscada2M1++poeEscada2M1++pegaEscada3M1++poeEscada3M1++fimMapa1


mapaFAQ1AposMov :: Mapa
mapaFAQ1AposMov =
  [ [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Caixa,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Caixa,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

jogoFAQ1AposMov :: Jogo
jogoFAQ1AposMov = Jogo mapaFAQ1AposMov (Jogador (1,9) Oeste False)