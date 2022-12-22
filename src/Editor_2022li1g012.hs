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
import RenderMapa_2022li1g012
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

{-|
  'renderizarTerrenos' devolve a imagem para mostrar o tipo de terreno e a
  sua velocidade de cada linha do mapa. __A imagem não está centrada!__ Na
  origem está o centro da tile do terreno superior da imagem resultante.
-}
renderizarTerrenos :: Assets  -- ^ Recursos do jogo
                   -> Mapa    -- ^ Mapa cujos terrenos serão desenhados
                   -> Picture -- ^ Imagem resultante
renderizarTerrenos b (Mapa _ lns) =
  Pictures $ map (\ (n, l) -> Translate 0 (-n * 32) $ aux l) $ zip [0..] lns
  where show' n = if n >= 0 then ' ' : show n else show n -- Alinhamento números
        aux (Relva, _) = renderizarObstaculo (tiles b) Relva Relva 0 Nenhum
        aux (Rio n, _) = let ((w, _), t) = mrTexto (fonte b) TCentro (show' n)
          in Pictures [
          renderizarObstaculo (tiles b) (Rio 0) (Rio 0) 0 Nenhum,
          Translate (24 + w) 0 $ Scale 2 2 $ t]
        aux (Estrada n, _) = let ((w, _), t) = mrTexto (fonte b) TCentro (show' n)
          in Pictures [
          renderizarObstaculo (tiles b) (Estrada 0) (Estrada 0) 0 Nenhum,
          Translate (24 + w) 0 $ Scale 2 2 $ t]

-- | 'renderizarEditor' é responsável por desenhar o editor no ecrã.
renderizarEditor :: EstadoJogo -> IO Picture
renderizarEditor (EJ (Editor p _ m bts) _ b) = return $ Pictures [
  Pictures $ map (imagemBotao p) bts,
  Translate (-384 + 32) (10 * 32 + 16) $ renderizarTerrenos b m,
  Translate (-10 * 32 + 48) (-10 * 32 + 32) $ renderizarMapa (tiles b) 0 m ]

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

