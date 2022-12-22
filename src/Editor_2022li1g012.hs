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
Module      : Editor_2022li1g012
Description : Editor de mapas
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Editor_2022li1g012 (
  -- * Funções expostas
  inicializarEditor,
  -- * Funções internas
  -- ** Gloss
  tempoEditor, eventoEditor, renderizarEditor,
  -- ** Inicialização
  novoMapa
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import FicheiroMapa_2022li1g012
import UI_2022li1g012
import ErroM_2022li1g012
import {-# SOURCE #-} MenuF_2022li1g012

-- | 'tempoEditor' reage à passagem do tempo (nenhuma)
tempoEditor :: Float -> EstadoJogo -> IO EstadoJogo
tempoEditor _ = return

{-|
  'eventoEditor' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoEditor :: Event -> EstadoJogo -> IO EstadoJogo
eventoEditor (EventMotion (x, y)) (EJ (Editor _ fp m bts) f a) =
  return $ EJ (Editor (x, y) fp m bts) f a
eventoEditor (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (Editor _ fp m bts) _ a)
  | dentro (fst (bts !! 0)) (x, y) = guardarFicheiroMapa 0 m fp >>=
      \ r -> if r then inicializarMenuF a 0 else
               inicializarErroM a "Erro ao guardar\n\no mapa :("
  | dentro (fst (bts !! 1)) (x, y) = inicializarMenuF a 0
eventoEditor _ e = return e

-- | 'renderizarEditor' é responsável por desenhar o editor no ecrã.
renderizarEditor :: EstadoJogo -> IO Picture
renderizarEditor (EJ (Editor p _ m bts) _ b) =
  return $ Pictures $ map (imagemBotao p) bts

-- | 'novoMapa' é o mapa vazio que deve ser usado quando um mapa é criado
novoMapa = Mapa 20 $ replicate 20 (Relva, replicate 20 Nenhum)

-- | 'inicializarEditor' devolve o estado inicial do editor de mapas.
inicializarEditor :: Assets        -- ^ Assets do jogo
                  -> Bool          -- ^ Se o mapa existe (criar um se não)
                  -> FilePath      -- ^ Onde guardar o mapa
                  -> IO EstadoJogo -- ^ Estado inicial do editor
inicializarEditor a ex fp = do
  let funcoesEditor = FJ tempoEditor eventoEditor renderizarEditor
      b1@((_, (w1, h1)), _) = scaleBt 2 $ mrBotao (fonte a) "Guardar e sair"
      b2@((_, (w2, h2)), _) = scaleBt 2 $ mrBotao (fonte a) "Sair sem guardar"
      b1' = translateBt (384 - w1 / 2 - 16)  (-384 + h1 / 2 + 16) b1
      b2' = translateBt (-384 + w2 / 2 + 16) (-384 + h2 / 2 + 16) b2
  m <- if ex then lerFicheiroMapa fp else return $ Just $ (0, novoMapa)
  case m of Nothing -> inicializarErroM a
                         ("Falha ao abir o mapa:\n" ++ nomeMapa fp ++ " :(")
            (Just m') -> return $ EJ (Editor (0, 0) fp (snd m') [b1', b2'])
                           funcoesEditor a

