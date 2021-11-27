{- |
Module      : LI12122
Description : Módulo auxiliar para LI1 21/22

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2021/22.
 -}
module LI12122 (
    -- * Tipos de dados
    -- ** Básicos
  Coordenadas , Direcao(..),
    -- ** Mapas
  Mapa , Peca(..),
    -- ** Jogo
  Jogo(..) , Jogador(..) , Movimento(..),
    -- * Funções auxiliares
  acederPeca,
  comparaPorCoordenadasXY,
  comparaPorCoordenadasYX,
  inserePeca,
  insertionSort,
  maioresCoordenadas,
  removeVazios,
  ) where

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | Uma peça no 'Mapa'.
data Peca
  = Bloco -- ^ um bloco que é indestrutível e não movivel
  | Caixa -- ^ a caixa é como um bloco mas pode ser movida pelo 'Jogador'
  | Porta -- ^ a porta é a posição final do jogo
  | Vazio -- ^ um espaço vazio no 'Mapa'
  deriving (Show, Read, Eq, Ord)

type Mapa = [[Peca]]

-- | Direção de um 'Jogador' no 'Mapa'.
data Direcao
  = Este
  | Oeste
  deriving (Show, Read, Eq, Ord)

-- | O personagem que é controlado pelo 'Jogador'.
data Jogador =
  Jogador
    Coordenadas -- ^ a posição atual no 'Mapa'
    Direcao -- ^ a direção atual
    Bool -- ^ um booleano que indica se o 'Jogador' está a carregar uma 'Caixa' ou não
  deriving (Show, Read, Eq, Ord)

-- | O nível de um jogo, que inclui o puzzle (mapa) e o personagem (jogador).
data Jogo =
  Jogo
    Mapa -- ^ o puzzle em si
    Jogador -- ^ o personagem do jogo
  deriving (Read, Eq)

-- | Os movimentos que podem ser tomados pelo jogador em cada estado do 'Jogo'.
data Movimento
  = AndarEsquerda -- ^ a acção de andar para a esquerda
  | AndarDireita -- ^ a ação de andar para a direita
  | Trepar -- ^ a ação de trepar uma caixa ou bloco
  | InterageCaixa -- ^ a ação de pegar ou largar uma caixa
  deriving (Show, Read, Eq, Ord)



-------------------------------------------------------------------------------
--                             FUNÇÕES AUXILIARES                            --
-------------------------------------------------------------------------------

-- | ordena uma lista usando o algoritmo insertion sort
insertionSort :: Ord a => [a] -> (a -> a -> Bool) -> [a]
insertionSort [a] _ = [a]
insertionSort (x:y:t) f = insert x (insertionSort (y:t) f) f

-- | função auxiliar do insertion sort
insert :: Ord a => a -> [a] -> (a -> a -> Bool) -> [a]
insert m [] _ = [m]
insert m (x:xs) f
    | f m x = x:insert m xs f
    | otherwise = m:x:xs


-- | Comparador para peças no mapa com base nas coordenadas
comparaPorCoordenadasXY :: (Peca, Coordenadas) -> (Peca, Coordenadas) -> Bool
comparaPorCoordenadasXY (_, c1) (_, c2) = c1 > c2


-- | Comparator para peças no mapa com base nas coordenadas
--   com prioridade para a coordenada y
comparaPorCoordenadasYX :: (Peca, Coordenadas) -> (Peca, Coordenadas) -> Bool
comparaPorCoordenadasYX (_, (x1, y1)) (_, (x2,y2)) = y1 > y2 || y1 == y2 && x1 > x2


-- | Devolve a peça correspondente às coordenadas no mapa
acederPeca :: Mapa -> Coordenadas -> Peca
acederPeca mapa coordPeca = acederPecaAux mapa coordPeca (0, 0)

acederPecaAux :: Mapa -> Coordenadas -> Coordenadas -> Peca
acederPecaAux ([]:t) coordPeca atuais = acederPecaAux t coordPeca (0, snd atuais + 1)
acederPecaAux ((h : hs) : t) coordPeca atuais
    | coordPeca == atuais = h
    | otherwise = acederPecaAux (hs:t) coordPeca (fst atuais + 1, snd atuais)

-- | Insere uma peça numa coordenada do mapa
inserePeca :: Mapa -> Peca -> Coordenadas -> Mapa
inserePeca mapa peca coordPeca = inserePecaAux mapa peca coordPeca (0, 0)

inserePecaAux :: Mapa -> Peca -> Coordenadas -> Coordenadas -> Mapa
inserePecaAux ([]:t) peca coordPeca atuais = [] : inserePecaAux t peca coordPeca (0, snd atuais + 1)
inserePecaAux ((h : hs) : t) peca coordPeca atuais
    | coordPeca == atuais = (peca : hs):t
    | otherwise = (h : head restoDoMapa) : tail restoDoMapa
    where restoDoMapa = inserePecaAux (hs:t) peca coordPeca (fst atuais + 1, snd atuais)

-- | Remove as peças "Vazio" da lista
removeVazios :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removeVazios [] = []
removeVazios ((p,c):t)
    | p == Vazio = removeVazios t
    | otherwise = (p,c) : removeVazios t

-- | Calcula o maior par de coordenadas (x, y)
--
--   __NOTA__: as coordenadas x e y podem vir de peças diferentes
maioresCoordenadas :: [(Peca, Coordenadas)] -> Coordenadas
maioresCoordenadas [(p,c)] = c
maioresCoordenadas ((p, (x1, y1)):(_, (x2, y2)):t)
    | x1 > x2 && y1 > y2 = maioresCoordenadas ((p, (x1, y1)):t)
    | x1 > x2 = maioresCoordenadas ((p, (x1, y2)):t)
    | y1 > y2 = maioresCoordenadas ((p, (x2, y1)):t)
    | otherwise = maioresCoordenadas ((p, (x2, y2)):t)
