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
Module      : Infinito_2022li1g012
Description : Modo infinto
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Infinito_2022li1g012 (
  -- * Funções expostas
  inicializarInf,
  -- * Funções internas
  -- ** Gloss
  tempoInf, eventoInf, renderizarInf,
  -- ** Recorde e pontuação
  calcularPontos, guardarRecorde,
  -- ** Deslize do mapa
  deslizarTempo, deslizarPosicao,
  --
  mapaInicial, renderizarPontos, verificarGameOver
) where

import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import Tarefa2_2022li1g012
import Tarefa3_2022li1g012
import Tarefa4_2022li1g012
import Tarefa5_2022li1g012
import FicheiroMapa_2022li1g012
import RenderMapa_2022li1g012
import JogoComum_2022li1g012
import UI_2022li1g012
import ErroM_2022li1g012
import {-# SOURCE #-} GameOver_2022li1g012 -- módulos mutuamente recursivos

-- | 'calcularPontos' calcula a pontuação do jogador
calcularPontos :: Jogador    -- ^ Posição do jogador
               -> Float      -- ^ Deslize vertical
               -> (Int, Int) -- ^ Pontuação máxima e recorde
               -> (Int, Int) -- ^ Pontuação e recorde
calcularPontos (Jogador (_, y)) d (m, r) = (max m (18 - y + floor d), r)

{-|
  'guardarRecorde' regista o recorde do jogador num ficheiro caso tenha sido
  batido.
-}
guardarRecorde :: (Int, Int) -- ^ Pontuação e recorde
               -> IO Bool    -- ^ Se a operação teve sucesso
guardarRecorde (p, r) = if p > r then guardarRecordeInf p else return True

{-|
  'verificarGameOver' verifica se o jogador faleceu ou não, indo para o menu
  de game over se necessário. Também regista o recorde do jogador.
-}
verificarGameOver :: EstadoJogo -> IO EstadoJogo
verificarGameOver ej@(EJ (Inf _ _ dv j@(Jogo jgd _) _ r) _ a)
  | jogoTerminou j = guardarRecorde (calcularPontos jgd dv r) >>=
      \ x -> if x then inicializarGO a "" else inicializarErroM a
        "Falha ao guardar\n\nrecorde :("
  | otherwise = return $ ej

{-|
  'deslizarTempo' desliza o mapa conforme a passagem do tempo, sendo a
  velocidade de deslize aumentada à medida que a pontuação aumenta.
-}
deslizarTempo :: Int           -- ^ Número aleatório de 1 a 100
              -> (Int, Int)    -- ^ Pontuação e recorde
              -> Float         -- ^ Tempo desde a útlima atualização (dt)
              -> Float         -- ^ Estado de deslizamento vertical
              -> Jogo          -- ^ Jogo a ser atualizado
              -> (Float, Jogo) -- ^ Deslizamento e jogo atualizados
deslizarTempo i (p, _) dt dv j =
  (dv + ddv, if atl then deslizaJogo i j else j)
  where -- Diferença da velocidade em função da pontuação
        ddv = (0.5 * dt) * (1 - 1 / (0.2 * (fromInteger $ toInteger (p + 5))))
        -- Se é preciso (ou não) avançar o mapa (e não só translação no ecrã)
        atl = ceiling dv == floor (dv + ddv) && ddv /= 0

-- | 'deslizarPosicao' avança o mapa caso o jogador avance muito para a frente.
deslizarPosicao :: Int           -- ^ Número aleatório de 1 a 100
                -> Float         -- ^ Estado do deslize vertical
                -> Jogo          -- ^ Jogador e mapa
                -> (Float, Jogo) -- ^ Estado de deslize e jogo atualizados
deslizarPosicao i dv j@(Jogo (Jogador (_, y)) _) = if y >= 18 then (dv, j) else
  (dv + 1, deslizaJogo i j)

-- | 'tempoInf' reage à passagem do tempo, deslizando e animando o mapa.
tempoInf :: Float -> EstadoJogo -> IO EstadoJogo
tempoInf dt (EJ (Inf t tl dv (Jogo j m) d r) f a) = do
  rdm <- randomRIO (0, 100)
  let tempos = (map (+ dt) tl)
      (tl', m') = tempoJogo j tempos m
      mFalso = mapaFalso tempos m
      j' = animaJogador j Parado mFalso
      (dv', jg) = deslizarTempo rdm r dt dv (Jogo j' m')
  verificarGameOver $ EJ (Inf (t + dt) tl' dv' jg d r) f a

-- | 'eventoInf' reage ao input do utilizador, controlando o jogador.
eventoInf :: Event -> EstadoJogo -> IO EstadoJogo
eventoInf e ej@(EJ (Inf t tl dv (Jogo j m) l r) f a)
  | jgd == Parado = return $ ej
  | otherwise     = randomRIO (0, 100) >>= \ i ->
                      let j' = animaJogador j jgd $ zerarMapa m
                          d' = let (Move s) = jgd in s
                          r' = calcularPontos j dv r
                          (dv', jogo) = deslizarPosicao i dv (Jogo j' m)
                      in verificarGameOver $
                         EJ (Inf t tl dv' jogo d' r') f a
  where jgd = eventosJogo e

-- | 'renderizarPontos' devolve a imagem da pontuação do jogador.
renderizarPontos :: BitmapData -- ^ Imagem da fonte
                 -> (Int, Int) -- ^ Pontuação e recorde
                 -> Picture    -- ^ Imagem resultante
renderizarPontos f (p, r)
  | p > r     = texto $ dourado $ show p
  | otherwise = texto $ show p
  where texto = Translate 0 340 . Scale 5 5 . snd . mrTexto f TCentro

-- | 'renderizarInf' é responsável por desenhar o jogo no ecrã.
renderizarInf :: EstadoJogo -> IO Picture
renderizarInf (EJ (Inf t _ dv j@(Jogo jgd _) d r) _ a) = return $ Pictures [
  Translate (-320) (-320 - 32 - dy) $ renderizarJogo (tiles a) t d j,

  -- Retângulos para esconder extra do mapa (deslize)
  Translate 0 (-320 - 64) $ Color black $ rectangleSolid 768 64,
  Translate 0 320 $ Color black $ rectangleSolid 768 64,

  renderizarPontos (fonte a) $ calcularPontos jgd dv r ]

  -- Posição do mapa para o smooth scrolling
  where dy = (dv - (fromIntegral $ floor dv)) * 32

-- | 'mapaInicial' gera o mapa para o começo do jogo.
mapaInicial :: IO Mapa
mapaInicial = aux 17 $ Mapa 20 [le, lr, lr, lr]
  where lr = (Relva,     replicate 20 Nenhum)
        le = (Estrada 0, replicate 20 Nenhum)
        aux n m = if n <= 0 then return m else
                    randomRIO (0, 100) >>= aux (n - 1) . estendeMapa m

-- | 'inicializarInf' devolve o estado inicial do modo infinito.
inicializarInf :: Assets        -- ^ Recurso do jogo
               -> IO EstadoJogo -- ^ Estado inicial do menu frogger
inicializarInf a = do
  let funcoesInf = FJ tempoInf eventoInf renderizarInf
  r <- lerRecordeInf
  m' <- mapaInicial
  case r of Nothing   -> inicializarErroM a "Falha ao ler\n\no recorde :("
            (Just r') -> let i = Inf 0 (replicate 21 0) 0
                                     (Jogo (Jogador (10, 18)) m') Cima (0, r')
                            in return $ EJ i funcoesInf a

