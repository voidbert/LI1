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
Module      : ErroM_2022li1g012
Description : Menu de erro, com opção de voltar ao menu principal
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module ErroM_2022li1g012 (
  -- * Funções expostas
  inicializarErroM,
  -- * Funções internas
  tempoErroM, eventoErroM, renderizarErroM
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012
import {-# SOURCE #-} MenuP_2022li1g012 -- módulos mutuamente recursivos

-- | 'tempoErroM' reage à passagem do tempo (nenhuma)
tempoErroM :: Float -> EstadoJogo -> IO EstadoJogo
tempoErroM _ = return

{-|
  'eventoErroM' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoErroM :: Event -> EstadoJogo -> IO EstadoJogo
eventoErroM (EventMotion (x, y)) (EJ (ErroM _ t bt) f b) =
  return $ EJ (ErroM (x, y) t bt) f b
eventoErroM (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (ErroM _ _ bt) _ b)
  | dentro (fst bt) (x, y) = inicializarMenu b
eventoErroM _ e = return e

-- | 'renderizarErroM' é responsável por desenhar a mensagem de erro no ecrã.
renderizarErroM :: EstadoJogo -> IO Picture
renderizarErroM (EJ (ErroM p t bt) _ b) = return $ Pictures [ t, imagemBotao ]
  where imagemBotao = let (r, (p1, p2)) = bt in if dentro r p then p2 else p1

-- | 'inicializarErroM' devolve o estado inicial da mensagem de erro.
inicializarErroM :: Assets        -- ^ Assets do jogo
                 -> String        -- ^ Texto da mensagem de erro
                 -> IO EstadoJogo -- ^ Estado de jogo da mensagem de error
inicializarErroM a s = return $ EJ (ErroM (0, 0) txt bt') funcoesMenuP a
  where txt = Scale 2 2 $ snd $ mrTexto (fonte a) TCentro s
        bt@((_, (_, h)), _) = scaleBt 2 $ mrBotao (fonte a) "Menu Principal"
        bt' = translateBt 0 (-384 + h / 2 + 16) bt
        funcoesMenuP = FJ tempoErroM eventoErroM renderizarErroM

