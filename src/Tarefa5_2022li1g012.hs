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
Module      : Tarefa5_2022li1g012
Description : Deslize do mapa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g012 where

import Tarefa2_2022li1g012
import LI12223

{- |
  A função 'deslizaJogo' acrescenta uma nova linha ao topo do @Mapa@ e retira a 
  final de modo a que este mantenha o seu comprimento original; além de
  corrigir a posição do jogador adicionando +1 à sua coordenada y, de modo a
  que exista um efeito de deslize com que o jogador fique para trás. Esta 
  função usufrui de duas auxiliares.
  
  === Exemplos

  >>> deslizaJogo 1 (Jogo (Jogador  (1,2)) 
                    (Mapa 5 [(Rio 1, [Tronco,Tronco,Nenhum,Tronco,Tronco]),
                    (Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum])]))                         
  Jogo (Jogador (1,3)) (Mapa 5 [(Rio (-3),[Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                                (Rio 1, [Tronco,Tronco,Nenhum,Tronco,Tronco])])
  
  >>> deslizaJogo 5 (Jogo (Jogador  (1,0)) 
                    (Mapa 5 [(Rio 1, [Tronco,Tronco,Nenhum,Tronco,Tronco]),
                             (Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum])]))
  Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                (Rio 1,[Tronco,Tronco,Nenhum,Tronco,Tronco])])
-}

deslizaJogo :: Int  -- ^ Inteiro aleatório de 0 a 100
            -> Jogo -- ^ Posição do jogador e @Mapa@
            -> Jogo
deslizaJogo i jg@(Jogo j m) = Jogo (deslizaJogador j) (deslizaMapa i jg)

{- |
  A função 'deslizaMapa' apenas adicinona uma nova linha ao topo do @Mapa@ com
  o auxilio de um número aleatório de 0 a 100 e, de seguida, retira o último
  elemento da lista criada, mantendo o tamanho original.
  
  === Exemplo

  >>> deslizaMapa 2 (Jogo (Jogador (1,1)) 
                    (Mapa 5 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum])]))
  Mapa 5 [(Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum])]
  
  >>> deslizaMapa 2 (Jogo (Jogador (1,2)) 
                    (Mapa 5 [(Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                    (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum])]))
  Mapa 5 [(Estrada 4,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
          (Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum])]
-}

deslizaMapa :: Int  -- ^ Inteiro aleatório de 0 a 100
            -> Jogo -- ^ Posição do jogador e @Mapa@
            -> Mapa
deslizaMapa i (Jogo (Jogador (_, y)) m) = Mapa l (init lns)
  where (Mapa l lns) = estendeMapa m ((y + i) `mod` 100)

{- |
  A função 'deslizaJogador' adiciona (+1) ao valor do y na posição do jogador
  (x,y) cada vez que se acrescenta uma nova linha ao topo do mapa e se retira 
  a última, fazendo com o que o jogador "deslize" com o mapa.

  === Exemplo 

  >>> deslizaJogador (Jogador (0,0))
  Jogador (0,1)
-}

deslizaJogador :: Jogador -- ^ Posiçãpo (x,y) do jogador 
               -> Jogador
deslizaJogador (Jogador (x, y)) = Jogador (x, y + 1)

