{- |
Module      : Tarefa6_2021li1g032
Description : Resolve um jogo
Copyright   : João Gomes Dias de Faria <a100553@alunos.uminho.pt>;
            : Francisco Manuel Afonso  <a100691@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g032 where

import LI12122
import Tarefa4_2021li1g032
import Tarefa3_2021li1g032
import Data.Maybe
import qualified Data.Map as M (Map, empty, insert, lookup, toList, unionWith)



data DoubleMap outerKey innerKey value = DMap (M.Map outerKey (M.Map innerKey value))
    deriving (Eq, Show)

-- | Calcula, caso exista, uma solução perfeita do `Jogo` com um número máximo de movimentos
resolveJogo :: Int               -- ^ Número máximo de movimentos
            -> Jogo              -- ^ Estado inicial do `Jogo`
            -> Maybe [Movimento] -- ^ Solução do `Jogo`, caso exista, ou `Nothing`
resolveJogo max jogo
    | caminho == Nothing = caminho
    | length (fromJust caminho) <= max = caminho
    | otherwise = Nothing
    where caminho = fst (resolveJogoAux jogo max (DMap M.empty))

resolveJogoAux :: Jogo                                            -- ^ Estado atual do `Jogo`
               -> Int                                             -- ^ Número de movimentos restantes
               -> DoubleMap Jogador Mapa (Int, Maybe [Movimento]) -- ^ `Map` de `Jogo` para a sua possível resolução e movimentos restantes
               -> (Maybe [Movimento], DoubleMap Jogador Mapa (Int, Maybe [Movimento])) -- ^ Par com a possível resolução e `Map` de visitados
resolveJogoAux jogo max visitados
    | value /= Nothing && max <= fst (fromJust value) = (snd (fromJust value), visitados)
    | max < 0 = (Nothing, visitados)
    | estaResolvido jogo = (Just [], insere jogo (max, Just []) (snd auxInterage))
    | otherwise = (caminhoMaisCurto, insere jogo (max, caminhoMaisCurto) (snd auxInterage))
    where
        value = procura jogo visitados
        visitadosFrente
            | value == Nothing = insere jogo (max, Nothing) visitados
            | otherwise = visitados
        auxDireita = resolveJogoAux (moveJogador jogo AndarDireita) (max-1) visitadosFrente
        auxEsquerda = resolveJogoAux (moveJogador jogo AndarEsquerda) (max-1) (snd auxDireita)
        auxTrepar = resolveJogoAux (moveJogador jogo Trepar) (max-1) (snd auxEsquerda)
        auxInterage = resolveJogoAux (moveJogador jogo InterageCaixa) (max-1) (snd auxTrepar)
        moveDireita = insereMaybeLista AndarDireita (fst auxDireita)
        moveEsquerda = insereMaybeLista AndarEsquerda (fst auxEsquerda)
        moveTrepar = insereMaybeLista Trepar (fst auxTrepar)
        moveInterage = insereMaybeLista InterageCaixa (fst auxInterage)
        caminhoMaisCurto = juntaMaybeLista [moveDireita, moveEsquerda, moveTrepar, moveInterage]

-- | Insere um Jogo no `Map` de visitados
insere :: Eq v
       => Jogo                     -- ^ `Jogo` inserido
       -> v                        -- ^ Valor a inserir no `Map`
       -> DoubleMap Jogador Mapa v -- ^ `Map` atual de visitados
       -> DoubleMap Jogador Mapa v -- ^ Novo `Map` de visitados
insere (Jogo mapa jogador) v (DMap dMap) = DMap (M.insert jogador (M.insert mapa v innerMap) dMap)
    where
        procuraJogador = M.lookup jogador dMap
        innerMap = if procuraJogador == Nothing then M.empty else fromJust procuraJogador

-- | Procura um Jogador nos visitados, caso encontre, procura um Mapa
procura :: Eq v
        => Jogo                     -- ^ `Jogo` a ser procurado no `Map`
        -> DoubleMap Jogador Mapa v -- ^ `Map` atual de visitados
        -> Maybe v                  -- ^ Valor do `Map` para o `Jogo` providenciado, ou `Nothing` caso este não esteja no `Map`
procura (Jogo mapa jogador) (DMap dMap)
    | procuraJogador == Nothing = Nothing
    | otherwise = M.lookup mapa (fromJust procuraJogador)
    where procuraJogador = M.lookup jogador dMap

-- | Insere um valor numa lista se esta existir
insereMaybeLista :: Eq a
                 => a         -- ^ Valor a ser inserido na lista
                 -> Maybe [a] -- ^ Possível lista onde vai ser inserido o valor
                 -> Maybe [a] -- ^ Lista final
insereMaybeLista x lista
    | lista == Nothing = Nothing
    | otherwise = Just (x : fromJust lista)

-- | Verifica se o jogador está na porta
estaResolvido :: Jogo -> Bool
estaResolvido (Jogo mapa (Jogador coord direcao caixa)) = acederPeca mapa coord == Porta

-- | Escolhe, de uma lista, a lista mais curta que nao seja `Nothing`, caso exista
juntaMaybeLista :: Eq a 
                => [Maybe [a]] -- ^ Lista da qual vai ser escolhido um elemento
                -> Maybe [a]   -- ^ Lista selecionada
juntaMaybeLista [] = Nothing
juntaMaybeLista [l] = l
juntaMaybeLista (Nothing:t) = juntaMaybeLista t
juntaMaybeLista (h1:h2:t)
    | h2 == Nothing = juntaMaybeLista (h1:t)
    | length (fromJust h1) <= length (fromJust h2) = juntaMaybeLista (h1:t)
    | otherwise = juntaMaybeLista (h2:t)
