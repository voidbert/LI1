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

{- |
Module      : Tarefa4_2022li1g012
Description : Determinar se o jogo terminou
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g012 where

import Tarefa2_2022li1g012
import LI12223

deslizaJogo :: Jogo -> Jogo
deslizaJogo j
  = removeLFinal (adicionaLinha j)

adicionaLinha :: Jogo -> Jogo
adicionaLinha (Jogo (Jogador (x,y)) (Mapa larg t))
  = let i = y
    in (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa larg t) i))

removeLFinal :: Jogo -> Jogo
removeLFinal (Jogo (Jogador (x,y)) (Mapa larg t))
  = Jogo (Jogador (x,y)) (Mapa larg (init t))
