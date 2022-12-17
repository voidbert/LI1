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
Module      : MenuP_2022li1g012
Description : Menu principal do jogo
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module MenuP_2022li1g012 where

import System.Exit (exitSuccess)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012

-- | 'tempoMenuP' reage à passagem do tempo (nenhuma)
tempoMenuP :: Float -> EstadoJogo -> IO EstadoJogo
tempoMenuP _ = return

{-|
  'eventoMenuP' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoMenuP :: Event -> EstadoJogo -> IO EstadoJogo
eventoMenuP (EventMotion (x, y)) (EJ (MenuP _ bts) f b) =
  return $ EJ (MenuP (x, y) bts) f b
eventoMenuP (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (MenuP _ bts) _ _)
  | dentro (fst (bts !! 2)) (x, y) = exitSuccess -- Botão sair
eventoMenuP _ e = return e

-- | 'renderizarMenuP' é responsável por desenhar o menu principal no ecrã.
renderizarMenuP :: EstadoJogo -> IO Picture
renderizarMenuP (EJ (MenuP p bts) _ b) = return $ Pictures [
  Translate 0 250 $ Scale 9 9 $ snd $ mrTexto (fonte b) TCentro "Crossy\nRoad",
  Pictures $ map imagemBotao bts,
  Translate (384 - w / 2 - 8) (h / 2 - 384 + 8) cpr
  ]
  where imagemBotao (r, (p1, p2)) = if dentro r p then p2 else p1
        ((w, h), cpr) = mrTexto (fonte b) TCentro
          "(C) Humberto Gomes & José Lopes"


-- | 'inicializarMenu' devolve o estado inicial do menu principal.
inicializarMenu :: Assets -> IO EstadoJogo
inicializarMenu a = return $ EJ (MenuP (0, 0) bts) funcoesMenuP a
  where bts = map (translateBt 0 (-120)) $
          gerarBotoesEsp (fonte a) 15 3 textoBotoes
        textoBotoes = [ "Modo infinito", "Modo frogger", "Sair" ]
        funcoesMenuP = FJ tempoMenuP eventoMenuP renderizarMenuP
