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
Module      : GameOver_2022li1g012
Description : Tela de "Game Over"
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module GameOver_2022li1g012 where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012


--função a retirar ao dar merge, só para conseguir usar o imagemBotao neste cenário 
imagemBotao :: (Float, Float) -> Botao -> Picture
imagemBotao p (r, (p1, p2)) = if dentro r p then p2 else p1

-- N importa no caso
tempoGO :: Float 
        -> EstadoJogo 
        -> IO EstadoJogo
tempoGO _ = return

{- 
-}
eventoGO :: Event 
         -> EstadoJogo 
         -> IO (EstadoJogo)
eventoGO (EventMotion (x, y)) (EJ (GameOver _ bts) fj a) 
  = return $                   EJ (GameOver (x, y) bts) fj a
eventoGo _ e = return e 

{- 
-}
renderizarGO :: EstadoJogo 
             -> IO Picture
renderizarGO (EJ (GameOver p bts) fj a) = return $ Pictures [
  Translate 0 250 $ Scale 5 5 $ snd $ mrTexto (fonte a) TCentro "Game\nOver",
  Pictures $ map (imagemBotao p) bts]

{-
-}
inicializarGO :: Assets 
                -> IO EstadoJogo
inicializarGO a = return $ EJ (GameOver (0,0) bts) funcoesJogoPE a
  where b1@((_, (w1, h1)), _) = scaleBt 2 $ mrBotao (fonte a) "Jogar de novo"
        b2@((_, (w2, h2)), _) = scaleBt 2 $ mrBotao (fonte a) "Voltar ao menu"
        b1' = translateBt (384 - w1 / 2 - 16)  (-384 + h1 / 2 + 16) b1
        b2' = translateBt (-384 + w2 / 2 + 16) (-384 + h2 / 2 + 16) b2
        bts = [b1', b2']
        funcoesJogoPE = FJ tempoGO eventoGO renderizarGO 
