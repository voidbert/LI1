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
Module      : Frogger_2022li1g012
Description : Modo frogger
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Frogger_2022li1g012 (
  -- * Funções expostas
  inicializarFrogger,
  -- * Funções internas
  tempoFrogger, eventoFrogger, renderizarFrogger,
  -- ** Funções auxiliares
  renderizarPontos
) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import FicheiroMapa_2022li1g012
import RenderMapa_2022li1g012
import UI_2022li1g012
import ErroM_2022li1g012

-- | 'tempoFrogger' reage à passagem do tempo (nenhuma).
tempoFrogger :: Float -> EstadoJogo -> IO EstadoJogo
tempoFrogger _ = return

-- | 'eventoFrogger' reage ao input do utilizador.
eventoFrogger :: Event -> EstadoJogo -> IO EstadoJogo
eventoFrogger _ e = return e

-- | 'renderizarPontos' devolve a imagem da pontuação do jogador.
renderizarPontos :: BitmapData -- ^ Imagem da fonte
                  -> (Int, Int) -- ^ Pontuação e recorde
                  -> Picture    -- ^ Imagem resultante
renderizarPontos f (p, r)
  | p > r     = texto $ dourado $ show p
  | otherwise = texto $ show p
  where texto = Translate 0 340 . Scale 5 5 . snd . mrTexto f TCentro  

-- | 'renderizarFrogger' é responsável por desenhar o jogo no ecrã.
renderizarFrogger :: EstadoJogo -> IO Picture
renderizarFrogger (EJ (Frogger t _ j d _ r) _ a) = return $ Pictures [
  Translate (-320) (-320 - 32) $ renderizarJogo (tiles a) t d j,
  renderizarPontos (fonte a) r]

-- | 'inicializarFrogger' devolve o estado inicial do modo frogger.
inicializarFrogger :: Assets        -- ^ Recurso do jogo
                   -> FilePath      -- ^ Caminho de ficheiro do mapa
                   -> IO EstadoJogo -- ^ Estado inicial do menu frogger
inicializarFrogger a fp = do
  let funcoesFrogger = FJ tempoFrogger eventoFrogger renderizarFrogger
  m <- lerFicheiroMapa fp
  case m of Nothing -> inicializarErroM a
                         ("Falha ao abir o mapa:\n" ++ nomeMapa fp ++ " :(")
            Just (r, m') -> let f = Frogger 0 fp (Jogo (Jogador (10, 19)) m')
                                      Cima True (0, r)
                            in return $ EJ f funcoesFrogger a

