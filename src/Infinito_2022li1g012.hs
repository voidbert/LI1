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
  deslizarTempo, deslizarPosicao, deslizarLinhas,
  --
  mapaInicial, renderizarPontos, verificarGameOver
) where

import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import Tarefa3_2022li1g012
import Tarefa4_2022li1g012
import Gerador_2022li1g012
import FicheiroMapa_2022li1g012
import RenderMapa_2022li1g012
import JogoComum_2022li1g012
import UI_2022li1g012
import ErroM_2022li1g012
import Audio_2022li1g012
import {-# SOURCE #-} GameOver_2022li1g012 -- módulos mutuamente recursivos

-- | 'calcularPontos' calcula a pontuação do jogador
calcularPontos :: Jogador    -- ^ Posição do jogador
               -> Int        -- ^ Unidades de deslize vertical
               -> (Int, Int) -- ^ Pontuação máxima e recorde
               -> (Int, Int) -- ^ Pontuação e recorde
calcularPontos (Jogador (_, y)) d (m, r) = (max m (18 - y + d), r)

{-|
  'guardarRecorde' regista o recorde do jogador num ficheiro caso tenha sido
  batido.
-}
guardarRecorde :: Dificuldade -- ^ Dificuldade do jogo
               -> (Int, Int)  -- ^ Pontuação e recorde
               -> IO Bool     -- ^ Se a operação teve sucesso
guardarRecorde d (p, r) = if p > r then guardarRecordeInfDif d p
  else return True

{-|
  'verificarGameOver' verifica se o jogador faleceu ou não, indo para o menu
  de game over se necessário. Também regista o recorde do jogador.
-}
verificarGameOver :: EstadoJogo -> IO EstadoJogo
verificarGameOver ej@(EJ (Inf dif _ _ _ udv j@(Jogo jgd _) _ r) _ a)
  | jogoTerminou j = guardarRecorde dif (calcularPontos jgd udv r) >>=
      \ x -> pararAudio' a jogoAudio >>= if x then (flip inicializarGO) (Right dif)
        else (flip inicializarErroM) "Falha ao guardar\n\nrecorde :("
  | otherwise = return $ ej

{-|
  'deslizarLinhas' é uma função que tem de ser invocada quando se chama
  'deslizaJogo''. Esta "desliza" a lista de tempos desde a última atualização
  de cada linha, de modo a acompanhar o mapa.
-}
deslizarLinhas :: Float   -- ^ Tempo decorrido desde o início
               -> [Float] -- ^ Tempos desde as últimas atualizações das linhas
               -> [Float] -- ^ Tempos atualizados
deslizarLinhas t ls = init ((t - (fromIntegral . floor) t) : ls)

{-|
  'deslizarTempo' desliza o mapa conforme a passagem do tempo, sendo a
  velocidade de deslize aumentada à medida que a pontuação aumenta.
-}
deslizarTempo :: RandomGen gen
              => gen                -- ^ Seed para geração de números aleatórios
              -> Float              -- ^ Tempo desde a última atualização
              -> DadosJogo          -- ^ Informações sobre o jogo
              -> (Float, Int, Jogo, [Float]) -- ^ Deslizamento, unidades, jogo e tempos de linhas atualizados
deslizarTempo sd dt (Inf d t tl dv udv j _ (p, _)) = if atl then
  (dv', udv + 1, deslizaJogo' sd d j, deslizarLinhas t tl) else (dv', udv, j, tl)
  where -- Diferença da velocidade em função da pontuação
        ddv = (0.5 * dt) * (1 - 1 / (0.2 * (fromInteger $ toInteger (p + 5))))
        -- Novo valor de dv e se é preciso atualizar o mapa
        (dv', atl) = if dv + ddv >= 1 then (dv + ddv - 1, True)
                        else (dv + ddv, False)

-- | 'deslizarPosicao' avança o mapa caso o jogador avance muito para a frente.
deslizarPosicao :: RandomGen gen
                => gen           -- ^ Seed para geração de números aleatórios
                -> Dificuldade   -- ^ Dificuldade do jogo
                -> Float         -- ^ Tempo decorrido total
                -> [Float]       -- ^ Tempo desde a atualização das linhas
                -> Int           -- ^ Unidades de deslize vertical
                -> Jogo          -- ^ Jogador e mapa
                -> (Int, Jogo, [Float]) -- ^ Unidades de deslize, jogo e linhas atualizados
deslizarPosicao sd d t tl udv j@(Jogo (Jogador (_, y)) _) = if y >= 18 then
  (udv, j, tl) else (udv + 1, deslizaJogo' sd d j, deslizarLinhas t tl)

-- | 'tempoInf' reage à passagem do tempo, deslizando e animando o mapa.
tempoInf :: Float -> EstadoJogo -> IO EstadoJogo
tempoInf dt (EJ (Inf dif t tl dv udv (Jogo j m) d r) f a) = do
  rdm <- newStdGen
  let tempos = (map (+ dt) tl)
      (tl', m') = tempoJogo tempos m
      mFalso = mapaFalso tempos m
      j' = animaJogador j Parado mFalso
      (dv', udv', jg, tl'') =
        deslizarTempo rdm dt $ Inf dif t tl' dv udv (Jogo j' m') d r
  verificarGameOver $ EJ (Inf dif (t + dt) tl'' dv' udv' jg d r) f a

-- | 'eventoInf' reage ao input do utilizador, controlando o jogador.
eventoInf :: Event -> EstadoJogo -> IO EstadoJogo
eventoInf e ej@(EJ (Inf dif t tl dv udv (Jogo j m) l r) f a)
  | jgd == Parado = return $ ej
  | otherwise     = newStdGen >>= \ sd ->
                      let j' = animaJogador j jgd $ zerarMapa m
                          d' = let (Move s) = jgd in s
                          r' = calcularPontos j udv r
                          (udv', jogo, tl') =
                            deslizarPosicao sd dif t tl udv (Jogo j' m)
                      in verificarGameOver $
                         EJ (Inf dif t tl' dv udv' jogo d' r') f a
  where jgd = eventosJogo e

-- | 'renderizarPontos' devolve a imagem da pontuação do jogador.
renderizarPontos :: BitmapData -- ^ Imagem da fonte
                 -> (Int, Int) -- ^ Pontuação e recorde
                 -> Picture    -- ^ Imagem resultante
renderizarPontos f (p, r)
  | p > r     = texto $ dourado $ show p
  | otherwise = texto $ show p
  where texto = Translate 0 340 . Scale 5 5 . snd . mrTexto f TCentro

{-|
  'renderizarRestaurante' devolve a imagem para desenhar o restaurante no
  início do jogo.
-}
renderizarRestaurante :: BitmapData -- ^ Mapa de tiles
                      -> Jogador    -- ^ Posição do jogador
                      -> Float      -- ^ Estado de deslizamento smooth
                      -> Int        -- ^ Unidades de deslizamento vertical
                      -> Picture    -- ^ Imagem resultante
renderizarRestaurante a (Jogador (_, y)) dv udv =
  let dy = fromIntegral udv + dv in Translate (320 - 64) (-320 + 32 - dy * 32)
    $ Scale 2 2 $ BitmapSection (Rectangle (0, 16) (64, 64)) a

-- | 'renderizarInf' é responsável por desenhar o jogo no ecrã.
renderizarInf :: EstadoJogo -> IO Picture
renderizarInf (EJ (Inf _ t tl dv udv j@(Jogo jgd@(Jogador (_, y)) _) d r) _ a) =
  return $ Pictures [
  Translate (-320) (-320 - 32 - dy) $ renderizarJogo (tiles a) t d j,
  renderizarRestaurante (tiles a) jgd dv udv,

  -- Retângulos para esconder extra do mapa (deslize)
  Translate 0 (-320 - 64) $ Color black $ rectangleSolid 768 64,
  Translate 0 320 $ Color black $ rectangleSolid 768 64,

  renderizarPontos (fonte a) $ calcularPontos jgd udv r]

  -- Posição do mapa para o smooth scrolling
  where dy = (dv - (fromIntegral $ floor dv)) * 32

-- | 'mapaInicial' gera o mapa para o começo do jogo.
mapaInicial :: Dificuldade -> IO Mapa
mapaInicial d = aux 17 $ Mapa 20 [le, lr, lr, lr]
  where lr = (Relva,     replicate 16 Nenhum ++ replicate 4 Arvore)
        le = (Estrada 0, replicate 20 Nenhum)
        aux n m = if n <= 0 then return m else newStdGen >>=
                    aux (n - 1) . (\ r -> estendeMapa' r d m)

-- | 'inicializarInf' devolve o estado inicial do modo infinito.
inicializarInf :: Assets        -- ^ Recursos do jogo
               -> Dificuldade   -- ^ Dificuldade do jogo
               -> IO EstadoJogo -- ^ Estado inicial do menu frogger
inicializarInf a d = do
  let funcoesInf = FJ tempoInf eventoInf renderizarInf
  r <- lerRecordeInfDif d
  m' <- mapaInicial d
  case r of Nothing   -> inicializarErroM a "Falha ao ler\n\no recorde :("
            (Just r') -> let i = Inf d 0 (replicate 21 0) 0 0
                                     (Jogo (Jogador (10, 18)) m') Cima (0, r')
                            in comecarAudio' a jogoAudio >>=
                               return . EJ i funcoesInf 

