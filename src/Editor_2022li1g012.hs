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
  -- ** Comuns
  posicaoRatoMapa, ratoVelocidades, ratoTerrenos,
  -- ** Inicialização
  novoMapa,
  -- ** Edição do mapa
  obstaculoSeguinteMapa, obstaculoSeguinteLinha, velocidadeSeguinte,
  terrenoSeguinte,
  -- ** Renderização
  renderizarTerrenos, retanguloRato
) where

import Data.Maybe
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
  'posicaoRatoMapa' transforma a posição do rato numa posição do mapa, caso o
  cursor se encontre na área visível do mapa representado.
-}
posicaoRatoMapa :: (Float, Float)   -- ^ Posição do rato
                -> Maybe (Int, Int) -- ^ Coordenadas no mapa
posicaoRatoMapa (x, y) = if 0 <= x' && x' < 20 && 0 <= y' && y' < 20 then
  Just (x', y') else Nothing
  where x' = floor ((x + 272) / 32)
        y' = floor ((-y + 353) / 32)

{-|
  'ratoVelocidades' transforma a posição do rato no índice de uma linha do
  mapa, caso o cursor se encontre sobre o texto de mudança de velocidade do
  terreno.
-}
ratoVelocidades :: (Float, Float) -- ^ Posição do rato
                -> Maybe Int      -- ^ Linha do mapa a ser mudada a velocidade
ratoVelocidades (x,y) = if 0 <= x' && x' < 64 && 0 <= y' && y' < 20 then
  Just y' else Nothing
  where x' = floor (x + (384 - (16 + 32)))
        y' = floor ((-y + 353) / 32)

{-|
  'ratoTerrenos' transforma a posição do rato no índice de uma linha do
  mapa, caso o cursor se encontre sobre a tile correspondente ao tipo de
  terreno.
-}
ratoTerrenos :: (Float, Float) -- ^ Posição do rato
             -> Maybe Int      -- ^ Linha do mapa a ser mudada a velocidade
ratoTerrenos (x,y) = if 0 <= x' && x' < 32 && 0 <= y' && y' < 20 then
  Just y' else Nothing
  where x' = floor (x + (384 - 16))
        y' = floor ((-y + 353) / 32)

{-|
  'obstaculoSeguinteLinha' é uma função auxiliar de 'obstaculoSeguinteMapa' que
  substitui um obstáculo numa linha de um mapa pelo que o procede.
-}
obstaculoSeguinteLinha :: (Terreno, [Obstaculo]) -- ^ Linha a editar
                       -> Int                    -- ^ Posição do obstáculo
                       -> (Terreno, [Obstaculo]) -- ^ Linha editada
obstaculoSeguinteLinha (t, os) n = (t, a ++ seguinte t x : b)
  where (a, (x:b)) = splitAt n os
        seguinte Relva       Nenhum = Arvore
        seguinte Relva       _      = Nenhum
        seguinte (Rio _)     Nenhum = Tronco
        seguinte (Rio _)     _      = Nenhum
        seguinte (Estrada _) Nenhum = Carro
        seguinte (Estrada _) _      = Nenhum

{-|
  'obstaculoSeguinteMapa' substitui um obstáculo numa posição do mapa pelo que
  o procede quando o utilizado clica para o mudar.
-}
obstaculoSeguinteMapa :: Mapa       -- ^ Mapa a editar
                      -> (Int, Int) -- ^ Posição no mapa
                      -> Mapa       -- ^ Mapa editado
obstaculoSeguinteMapa (Mapa lg lns) (x, y) =
  Mapa lg (a ++ obstaculoSeguinteLinha l x : b)
  where (a, (l:b)) = splitAt y lns

{-|
  'velocidadeSeguinte' altera a velocidade de uma linha do mapa para a
  velocidade seguinte na ciclo -3, -2, -1, 0, 1, 2, 3, -3, -2, ...
-}
velocidadeSeguinte :: Mapa -- ^ Mapa inicial
                   -> Int  -- ^ Número da linha
                   -> Mapa -- ^ Mapa final
velocidadeSeguinte (Mapa l lns) n =
  Mapa l (a ++ (seguinte t, obs) : b)
  where (a, ((t, obs):b)) = splitAt n lns
        numero n = if n >= 3 then (-3) else n + 1
        seguinte (Rio     v) = Rio     $ numero v
        seguinte (Estrada v) = Estrada $ numero v
        seguinte Relva       = Relva

{-|
  'terrenoSeguinte' altera o tipo de terreno de uma linha do mapa para o tipo
  de terreno seguinte.
-}
terrenoSeguinte :: Mapa -- ^ Mapa inicial
                -> Int  -- ^ Número da linha
                -> Mapa -- ^ Mapa final
terrenoSeguinte (Mapa l lns) n =
  Mapa l (a ++ (seguinte t, replicate 20 Nenhum) : b)
  where (a, ((t, obs):b)) = splitAt n lns
        seguinte (Rio     _) = Estrada 0
        seguinte (Estrada _) = Relva
        seguinte Relva       = Rio 0

{-|
  'eventoEditor' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoEditor :: Event -> EstadoJogo -> IO EstadoJogo
eventoEditor (EventMotion (x, y)) (EJ (Editor _ fp m bts) f a) =
  return $ EJ (Editor (x, y) fp m bts) f a
eventoEditor (EventKey (MouseButton LeftButton) Up _ p@(x, y))
            ej@(EJ (Editor _ fp m bts) f a)
  | dentro (fst (bts !! 0)) (x, y) = guardarFicheiroMapa 0 m fp >>=
      \ r -> if r then inicializarMenuF a 0 else
               inicializarErroM a "Erro ao guardar\n\no mapa :("
  | dentro (fst (bts !! 1)) (x, y) = inicializarMenuF a 0
  | isJust pm = return $
      EJ (Editor p fp (obstaculoSeguinteMapa m (fromJust pm)) bts) f a
  | isJust pv = return $
      EJ (Editor p fp (velocidadeSeguinte m (fromJust pv)) bts) f a
  | isJust pt = return $
      EJ (Editor p fp (terrenoSeguinte m (fromJust pt)) bts) f a
  where pm = posicaoRatoMapa p
        pv = ratoVelocidades p
        pt = ratoTerrenos    p
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

{-|
  'retanguloRato' devolve um retângulo para ser desenhado, que faz sobressair
  em que é que o jogador está a clicar (mudar um obstáculo, uma velocidade ou
  um terreno).
-}
retanguloRato :: Mapa           -- ^ Mapa a ser editado
              -> (Float, Float) -- ^ Posição do rato
              -> Picture        -- ^ Imagem do retângulo
retanguloRato (Mapa _ lns) p
  | isJust pm = let (x, y) = fromJust pm in color red $
      Translate (fromIntegral x * 32 - 256) (- (fromIntegral y * 32 - 337)) $
      rectangleWire 32 32
  | isJust pv = let y = fromJust pv in if fst (lns !! y) == Relva then Blank
      else color red $ Translate (-384 + (16 + 32 + 32))
      (- (fromIntegral y * 32 - 337)) $ rectangleWire 64 32
  | isJust pt = let y = fromJust pt in color red $
      Translate (-384 + (16 + 16)) (- (fromIntegral y * 32 - 337)) $
      rectangleWire 32 32
  | otherwise = Blank
  where pm = posicaoRatoMapa p
        pv = ratoVelocidades p
        pt = ratoTerrenos    p

-- | 'renderizarEditor' é responsável por desenhar o editor no ecrã.
renderizarEditor :: EstadoJogo -> IO Picture
renderizarEditor (EJ (Editor p _ m bts) _ b) = return $ Pictures [
  Pictures $ map (imagemBotao p) bts,
  Translate (-384 + 32) (10 * 32 + 16) $ renderizarTerrenos b m,
  Translate (-10 * 32 + 48) (-10 * 32 + 32) $ renderizarMapa (tiles b) 0 m,
  retanguloRato m p]

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

