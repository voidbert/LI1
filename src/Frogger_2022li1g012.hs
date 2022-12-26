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
  renderizarPontos, chegouPonta, verificarGameOver, guardarRecorde
) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import Tarefa3_2022li1g012
import Tarefa4_2022li1g012
import FicheiroMapa_2022li1g012
import RenderMapa_2022li1g012
import JogoComum_2022li1g012
import UI_2022li1g012
import ErroM_2022li1g012
import {-# SOURCE #-} GameOver_2022li1g012 -- módulos mutuamente recursivos

{-|
  'guardarRecorde' regista o recorde do jogador no ficheiro do mapa. Este
  apenas é editado caso o recorde tenha sido batido.
-}
guardarRecorde :: (Int, Int) -- ^ Pontuação e recorde
               -> FilePath   -- ^ Mapa onde guardar recorde
               -> IO Bool    -- ^ Se a operação teve sucesso
guardarRecorde (p, r) fp
  | p > r = lerFicheiroMapa fp >>= guardar
  | otherwise = return True
  where guardar Nothing = return False
        guardar (Just (_, m)) = guardarFicheiroMapa p m fp

{-|
  'verificarGameOver' verifica se o jogador faleceu ou não, indo para o menu
  de game over se necessário. Também regista o recorde do jogador.
-}
verificarGameOver :: EstadoJogo -> IO EstadoJogo
verificarGameOver ej@(EJ (Frogger _ _ fp j _ _ r) _ a)
  | jogoTerminou j = guardarRecorde r fp >>= \ x -> if x then
      inicializarGO a (Left fp) else inicializarErroM a
     "Falha ao guardar\n\nrecorde :("
  | otherwise = return $ ej

-- | 'tempoFrogger' reage à passagem do tempo (nenhuma).
tempoFrogger :: Float -> EstadoJogo -> IO EstadoJogo
tempoFrogger dt (EJ (Frogger t tl fp (Jogo j m) d l r) f a) = verificarGameOver
  $ EJ (Frogger (t + dt) tl' fp (Jogo j' m') d l r) f a
  where tempos = (map (+ dt) tl)
        (tl', m') = tempoJogo j tempos m
        mFalso = mapaFalso tempos m
        j' = animaJogador j Parado mFalso

{-|
  'chegouPonta' verifica se o jogador chegou à ponta do mapa em que a sua
  pontuação aumenta.
-}
chegouPonta :: Jogador -- ^ Posição do jogador
            -> Direcao -- ^ Direção do movimento
            -> Bool    -- ^ Ponta a ser atingida
            -> Bool    -- ^ Se a ponta foi atingida
chegouPonta (Jogador (_, 0))  Cima  True  = True
chegouPonta (Jogador (_, 19)) Baixo False = True
chegouPonta _                 _     _     = False

-- | 'eventoFrogger' reage ao input do utilizador.
eventoFrogger :: Event -> EstadoJogo -> IO EstadoJogo
eventoFrogger e ej@(EJ (Frogger t tl fp (Jogo j m) d l (p, r)) f a)
  | jgd == Parado = return $ ej
  | otherwise     = let j' = animaJogador j jgd $ zerarMapa m
                        d' = let (Move s) = jgd in s
                        pt = chegouPonta j' d' l
                        (l', p') = if pt then (not l, p + 1) else (l, p)
                    in verificarGameOver $
                       EJ (Frogger t tl fp (Jogo j' m) d' l' (p', r)) f a
  where jgd = eventosJogo e

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
renderizarFrogger (EJ (Frogger t _ _ j d _ r) _ a) = return $ Pictures [
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
            Just (r, m') -> let f = Frogger 0 (replicate 20 0) fp
                                      (Jogo (Jogador (10, 19)) m') Cima True
                                      (0, r)
                            in return $ EJ f funcoesFrogger a

