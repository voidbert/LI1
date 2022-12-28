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
Module      : MenuD_2022li1g012
Description : Menu de escolha de dificuldade
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module MenuD_2022li1g012 (
  -- * Funções expostas
  inicializarMenuD,
  -- * Funções internas
  tempoMenuD, eventoMenuD, renderizarMenuD
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012
import Gerador_2022li1g012
import Infinito_2022li1g012
import Audio_2022li1g012

-- | 'tempoMenuP' reage à passagem do tempo (nenhuma)
tempoMenuD :: Float -> EstadoJogo -> IO EstadoJogo
tempoMenuD _ = return

{-|
  'eventoMenuD' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoMenuD :: Event -> EstadoJogo -> IO EstadoJogo
eventoMenuD (EventMotion (x, y)) (EJ (MenuD _ bts) f b) =
  return $ EJ (MenuD (x, y) bts) f b
eventoMenuD (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (MenuD _ bts) _ a)
  | dentro (fst (bts !! 0)) (x, y) = pararAudios (musica a) >> inicializarInf a facil
  | dentro (fst (bts !! 1)) (x, y) = pararAudios (musica a) >> inicializarInf a medio
  | dentro (fst (bts !! 2)) (x, y) = pararAudios (musica a) >> inicializarInf a dificil
eventoMenuD _ e = return e

-- | 'renderizarMenuD' é responsável por desenhar o menu no ecrã.
renderizarMenuD :: EstadoJogo -> IO Picture
renderizarMenuD (EJ (MenuD p bts) _ b) = return $ Pictures [
  Translate 0 250 $ Scale 7 7 $ snd $ mrTexto (fonte b) TCentro
  "Escolha a\n\ndificuldade", Pictures $ map (imagemBotao p) bts ]

-- | 'inicializarMenuD' devolve o estado inicial do menu de dificuldades.
inicializarMenuD :: Assets -> IO EstadoJogo
inicializarMenuD a = return $ EJ (MenuD (0, 0) bts) funcoesMenuD a
  where bts = map (translateBt 0 (-120)) $ snd $
          gerarBotoesEspV (fonte a) 15 3 textoBotoes
        textoBotoes = [ "Crossy Street", "Crossy Road", "Crossy Highway" ]
        funcoesMenuD = FJ tempoMenuD eventoMenuD renderizarMenuD

