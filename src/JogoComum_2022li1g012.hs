{-
   Copyright 2022 Humberto Gomes, José Lopes

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-|
Module      : JogoComum_2022li1g012
Description : Funções comuns aos dois modos de jogo (infinito e frogger)
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module JogoComum_2022li1g012 (
  -- * Input do utilizador
  eventosJogo,
  -- * Animação do mapa
  tempoJogo, mapaFalso, zerarMapa,
  -- ** Funções auxiliares
  tempoLinha, avancosLinha
  ) where

import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import Tarefa3_2022li1g012

{-|
  'eventosJogo' analisa um evento de input do jogador, e devolve a jogada que
  deve ser executada.
-}
eventosJogo :: Event  -- ^ Evento de input
            -> Jogada -- ^ Jogada a ser executada
eventosJogo (EventKey (Char c) Down _ _)
  | c' == 'W' = Move Cima
  | c' == 'S' = Move Baixo
  | c' == 'A' = Move Esquerda
  | c' == 'D' = Move Direita
  where c' = toUpper c
eventosJogo (EventKey (SpecialKey KeyUp)    Down _ _) = Move Cima
eventosJogo (EventKey (SpecialKey KeyDown)  Down _ _) = Move Baixo
eventosJogo (EventKey (SpecialKey KeyLeft)  Down _ _) = Move Esquerda
eventosJogo (EventKey (SpecialKey KeyRight) Down _ _) = Move Direita
eventosJogo _ = Parado

{-|
  'avancosLinha' calcula, conforme o tempo passado desde a última atualização
  de uma linha do mapa, quantas vezes é que esta se deve mexer na sua direção
  de um em um obstáculo.
-}
avancosLinha :: Float      -- ^ Tempo desde a útlima atualização da linha
             -> Velocidade -- ^ Velocidade da linha
             -> Int        -- ^ Número de translações unitárias
avancosLinha t n
  | t >= dt   = 1 + avancosLinha (t - dt) n
  | otherwise = 0
  where dt = 1 / (abs $ fromIntegral n)

{-|
  'tempoLinha' é função auxiliar de 'tempoJogo' e atualiza uma linha do mapa
  dado o tempo passado desde a última atualização, devolvendo o tempo restante
  e a linha atualizada.

  === Nota

  Se se passam @1.5s@ e uma linha move-se uma vez por segundo (@Estrada 1@), o
  tempo restante será @0.5s@, que deve ser contabilizado para a atualização
  seguinte.

  === Exemplo

  Uma linha de terreno @Rio 2@ move-se uma unidade para a direita duas vezes
  por segundo. Assim, será movida uma unidade se o tempo passado for superior
  a @0.5s@, duas unidades se for superior a @1.0s@, ...

  >>> tempoLinha 3 (Jogador (0, 0)) 1.1 (Rio 2, [Tronco, Nenhum, Nenhum])
  (0.1, [Nenhum, Nenhum, Tronco])
-}
tempoLinha :: Largura                         -- ^ Largura da linha
           -> Jogador                         -- ^ Posição do jogador
           -> Float                           -- ^ Tempo desde a última atualização
           -> (Terreno, [Obstaculo])          -- ^ Linha a atualizar
           -> (Float, (Terreno, [Obstaculo])) -- ^ Tempo e linha atualizada
tempoLinha _ _ _ l@(Relva,     _) = (0, l)
tempoLinha _ _ _ l@(Rio 0,     _) = (0, l)
tempoLinha _ _ _ l@(Estrada 0, _) = (0, l)
tempoLinha lg j t l@(ter, obs) =
  let (_, obs') = animaLinha' lg j (edit ter, obs) in (resto, (ter, obs'))
  where -- Saber número de avanços e tempo restante
        vlinha (Rio n)     = n
        vlinha (Estrada n) = n
        v = vlinha ter
        avcs = avancosLinha t v
        resto = t - (fromIntegral avcs * (1 / (abs $ fromIntegral $ v)))

        -- Saber velocidade do terreno para o número de avanços
        edit (Rio vt)     = Rio (avcs * signum vt)
        edit (Estrada vt) = Estrada (avcs * signum vt)

{-|
  'tempoJogo' atualiza todas as linhas do mapa sabendo o tempo que se passou
  desde a última atualização de cada linha. As linhas são animadas obstáculo a
  obstáculo, conforme o tempo passado.

  === Exemplo

  Ver 'tempoLinha'.
-}
tempoJogo :: Jogador         -- ^ Posição do jogador
          -> [Float]         -- ^ Tempo desde a última atualização das linhas
          -> Mapa            -- ^ Mapa a ser atualizado
          -> ([Float], Mapa) -- ^ Tempos restantes e mapa atualizado
tempoJogo j ts (Mapa lg lns) = (ts', Mapa lg lns')
  where (ts', lns') = unzip $ map (uncurry $ tempoLinha lg j) $ zip ts lns

{-|
  'mapaFalso' altera a velocidade das linhas do mapa para corresponder ao
  moviemento executado numa fração de segundo. Isto é útil para mover o jogador
  em troncos, dado que os troncos podem não se mover a totalidade da velocidade
  do rio.

  === Exemplo

  Se se passa meio segundo, @Rio (-2)@ é movido uma unidade para a esquerda.
  Nessa linha, esta função devolve @Rio (-1)@.
-}
mapaFalso :: [Float] -- ^ Tempo desde a última atualização das linhas
          -> Mapa    -- ^ Mapa a ser editado
          -> Mapa    -- ^ Mapa com velocidades falsas
mapaFalso ts (Mapa lg lns) = Mapa lg lns'
  where v _ (Relva,     o) = (Relva,                                 o)
        v t (Rio v,     o) = (Rio     (avancosLinha t v * signum v), o)
        v t (Estrada v, o) = (Estrada (avancosLinha t v * signum v), o)
        lns' = map (uncurry v) $ zip ts lns

{-|
  'zerarMapa' transforma o mapa de modo a que todas as linhas tenham velocidade
  nula. Isto é útil para o jogador não ser movido quando carrega numa tecla
  enquanto num rio. O movimento será executado quando o rio se mover com a
  passagem do tempo.
-}
zerarMapa :: Mapa -> Mapa
zerarMapa (Mapa lg lns) = Mapa lg $ map aux lns
  where aux (Relva,     o) = (Relva,     o)
        aux (Rio _,     o) = (Rio 0,     o)
        aux (Estrada _, o) = (Estrada 0, o)
