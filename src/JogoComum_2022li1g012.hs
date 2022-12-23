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
Module      : JogoComum_2022li1g012
Description : Funções comuns aos dois modos de jogo (infinito e frogger)
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module JogoComum_2022li1g012 where

import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223

{-|
  'eventosJogo' analisa um evento de input do jogador, e devolve a jogada que
  deve ser executada.
-}
eventosJogo :: Event  -- ^ Evento de input
            -> Jogada -- ^ Jogada a ser executada
eventosJogo (EventKey (Char c) Down _ _)
  | c' == 'W' = Move Cima
  | c' == 'S' = Move Baixo
  | c' == 'A' = Move Esquerda
  | c' == 'D' = Move Direita
  where c' = toUpper c
eventosJogo (EventKey (SpecialKey KeyUp)    Down _ _) = Move Cima
eventosJogo (EventKey (SpecialKey KeyDown)  Down _ _) = Move Baixo
eventosJogo (EventKey (SpecialKey KeyLeft)  Down _ _) = Move Esquerda
eventosJogo (EventKey (SpecialKey KeyRight) Down _ _) = Move Direita
eventosJogo _ = Parado
