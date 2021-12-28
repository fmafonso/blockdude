
import Data.HashMap
import Data.Hashable
import Tarefa3_2021li1g032
import LI12122

instance Hashable Jogo where
    hashWithSalt s (Jogo m j) = s + (hash m) + (hash j)

instance Hashable Jogador where
    hashWithSalt s (Jogador c d b) = s + (hash c) + (hash d) + (hash b)

instance Hashable Direcao where
    hashWithSalt s d = s + (hash d)

instance Hashable Peca where
    hashWithSalt s p = s + (hash p)


procura :: (Hashable k, Ord k) => k -> Map k v -> Maybe v
procura x mapa = Data.HashMap.lookup x mapa

insereNoMapa :: (Eq k, Hashable k, Ord k) => k -> v -> Map k v -> Map k v
insereNoMapa key value mapa = Data.HashMap.insert key value mapa

main :: IO ()
main = do print (insereNoMapa (Jogo [[Vazio, Porta,Vazio],[Bloco,Bloco,Bloco]] (Jogador (2,0) Oeste False)) 5 empty)