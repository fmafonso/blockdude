module Fixture_mapaFAQ2 where

import LI12122

mapaFAQ2 :: [(Peca,Coordenadas)]
mapaFAQ2 =
  [ (Bloco,(0,0)), (Bloco, (0,1)), (Bloco,(0,2)), (Bloco,(0,3)), (Bloco,(0,4)),
    (Bloco,(0,5)), (Bloco,(0,6)), (Porta,(1,5)), (Bloco,(1,6)), (Bloco,(2,6)),
    (Bloco,(3,6)), (Bloco,(4,4)), (Bloco,(4,5)), (Bloco,(4,6)), (Bloco,(5,6)),
    (Bloco,(6,6)), (Bloco,(7,6)), (Bloco,(8,5)), (Bloco,(8,6)), (Bloco,(9,6)),
    (Caixa,(10,5)), (Bloco,(10,6)), (Bloco,(11,6)), (Bloco,(12,4)), (Bloco,(12,5)),
    (Bloco,(12,6)), (Bloco,(13,6)), (Caixa,(14,5)), (Bloco,(14,6)), (Bloco,(15,6)),
    (Bloco,(16,6)), (Bloco,(17,6)), (Bloco,(18,6)), (Bloco,(19,0)), (Bloco,(19,1)),
    (Bloco,(19,2)), (Bloco,(19,3)), (Bloco,(19,4)), (Bloco,(19,5)), (Bloco,(19,6))
  ]

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

movimentosM2 = [
  AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,Trepar,
  AndarEsquerda,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,
  InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda
  ]

mapaFAQ2AposMov :: Mapa
mapaFAQ2AposMov =
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ]

jogoFAQ2AposMov :: Jogo
jogoFAQ2AposMov = Jogo mapaFAQ2AposMov (Jogador (1,5) Oeste False)