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
Module      : Main
Description : Ponto de entrada para o programa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Main where

import LI12223
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Concurrent -- TODO - remover. Para motivos de testagem

{-|
  'renderizarGloss', dado um estado de jogo, devolve os conteúdos que devem ser
  desenhados no ecrã.
-}
renderizarGloss :: EstadoJogo -- ^ Atual parente do estado de jogo
                -> IO Picture    -- ^ Conteúdos no ecrã
renderizarGloss j@(EJ _ (FJ _ _ r)) = r j

{-|
  'eventosGloss' reage a ('Graphics.Gloss.Interface.IO.Game.Event')s do gloss.
  Eventos de redimensionamento não são enviados para as funções de atualização
  de um estado de jogo.
-}
eventosGloss :: Event         -- ^ Evento em análise
             -> EstadoJogo    -- ^ Atual estado de jogo
             -> IO EstadoJogo -- ^ Estado de jogo com o novo evento
eventosGloss (EventResize t) j = return j
eventosGloss e j@(EJ _ (FJ _ r _)) = r e j

{-|
  'tempoGloss' reage à passagem de tempo, atualizando o estado de jogo.
-}
tempoGloss :: Float         -- ^ Tempo passado desde a última atualização
           -> EstadoJogo    -- ^ Estado de jogo e eventos atuais
           -> IO EstadoJogo -- ^ Estado de jogo seguinte
tempoGloss t j@(EJ _ (FJ a _ _)) = a t j

-- TODO - remover. Isto é para testagem apenas.
tempoMenu t (EJ (MenuP xy _) f) = return $ EJ (MenuP xy t) f

eventoMenu (EventMotion (x, y)) (EJ (MenuP _ t) f) =
  return $ EJ (MenuP (x, y) t) f
eventoMenu _ e = return e

renderizarMenu (EJ (MenuP (x, y) t) _) = sleep $ Color c $ Pictures [ txt, Circle 50 ]
  where txt = Scale 0.1 0.1 $ Text $ show t
        c = if x ** 2 + y ** 2 <= 50 ** 2 then red else white
        sleep x = threadDelay (5 * 10^4) >>= (\ _ -> return x)


-- | Ponto de entrada do programa, onde se abre a janela com o jogo.
main :: IO ()
main = let janela = InWindow "Crossy Road" (160, 160) (0, 0)
           inicial = EJ (MenuP (0, 0) 0) (FJ tempoMenu eventoMenu renderizarMenu)
  in playIO janela black 60 inicial renderizarGloss eventosGloss tempoGloss
