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
Module      : GameOver_2022li1g012
Description : Tela de "Game Over"
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module GameOver_2022li1g012 (
    -- * Funções expostas
  inicializarGO,
  -- * Funções internas
  -- ** Gloss
  tempoGO, eventoGO, renderizarGO
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012
import Frogger_2022li1g012
import {-# SOURCE #-} MenuP_2022li1g012

-- O Tempo não importa para este menu

tempoGO :: Float 
        -> EstadoJogo 
        -> IO EstadoJogo
tempoGO _ = return

{-|
  'eventoGO' reage ao input do jogador (movimentação do rato por cima dos 
  botões e clique de botão esquerdo do rato). Existe uma adaptação para
  que o botão "Jogar de novo" possa iniciar tanto o 'Frogger' como o 'Modo
  infinito', dependendo do que foi jogado anteriormente.
-}

eventoGO :: Event           -- ^ Evento que modifica o EstadoJogo
         -> EstadoJogo 
         -> IO (EstadoJogo) -- ^ Estado de jogo atualizado de acordo com o evento
eventoGO (EventMotion (x, y)) (EJ (GameOver _ bts fp) fj a) 
  = return $ EJ (GameOver (x, y) bts fp) fj a
eventoGO (EventKey (MouseButton LeftButton) Up _ p)
  (EJ (GameOver _ bts fp) _ a)
  -- TODO - modo infinito
  | dentro (fst (bts !! 0)) p = if null fp then undefined else
      inicializarFrogger a fp
  | dentro (fst (bts !! 1)) p = inicializarMenu a
eventoGO _ e = return e

-- | 'renderizarGO' desenha o menu principal com um conjunto de Pictures.

renderizarGO :: EstadoJogo 
             -> IO Picture
renderizarGO (EJ (GameOver p bts _) fj a) = return $ Pictures [
  Translate 0 250 $ Scale 5 5 $ snd $ mrTexto (fonte a) TCentro "Game\nOver",
  balde a, Pictures $ map (imagemBotao p) bts ]

-- | 'inicializarGO' devolve o estado estado inicial do menu de "Game Over".
inicializarGO :: Assets         -- ^ Recursos do jogo
              -> FilePath       -- ^ Vazio para modo infinito, ou mapa frogger
              -> IO EstadoJogo  -- ^ Estado inicial do menu
inicializarGO a fp = return $ EJ (GameOver (0,0) bts fp) funcoesJogoGO a
  where b1@((_, (w1, h1)), _) = scaleBt 2 $ mrBotao (fonte a) "Jogar de novo"
        b2@((_, (w2, h2)), _) = scaleBt 2 $ mrBotao (fonte a) "Voltar ao menu"
        b1' = translateBt (384 - w1 / 2 - 16)  (-384 + h1 / 2 + 16) b1
        b2' = translateBt (-384 + w2 / 2 + 16) (-384 + h2 / 2 + 16) b2
        bts = [b1', b2']
        funcoesJogoGO = FJ tempoGO eventoGO renderizarGO
