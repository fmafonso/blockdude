{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Map
import Data.Aeson
import Data.ByteString.Lazy.Char8 as Char8


----------------------------------------------------------------------------------------------------------------------
type Coordenadas = (Int, Int)

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
----------------------------------------------------------------------------------------------------------------------

jogadorToJson :: Jogador -> JogadorJSON
jogadorToJson _ = undefined

-- Just (JogadorJSON {x = 15, y = 67, direcao = "Este", temCaixa = False})

stringToDirecao :: String -> Direcao
stringToDirecao "Este" = Este
stringToDirecao "Oeste" = Oeste

jsonToJogador :: Maybe JogadorJSON -> Maybe Jogador
jsonToJogador Nothing = Nothing
jsonToJogador (Just json) = Just (Jogador (x json, y json) (stringToDirecao (direcao json)) (temCaixa json))

data JogadorJSON = JogadorJSON {
      x :: Int
    , y  :: Int
    , direcao :: String
    , temCaixa :: Bool
    } deriving (Generic, Show)

instance ToJSON JogadorJSON

instance FromJSON JogadorJSON

main = do
    bs <- Char8.readFile "../data/exemplo.json"
    let objeto = decode bs :: Maybe JogadorJSON
    print (jsonToJogador objeto)