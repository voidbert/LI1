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
Description : Determinar se o jogo terminou
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

  >>> deslizaJogo 2 (Jogo (Jogador  (1,2)) 
                    (Mapa 5 [(Rio 1, [Tronco,Tronco,Nenhum,Tronco,Tronco]),
                    (Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum])]))                         
  Jogo (Jogador (1,2)) (Mapa 5 [(Rio (-3),[Tronco,Tronco,Tronco,Nenhum,Nenhum]),
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
deslizaJogo v j
  = removeLFinal $ adicionaLinha v j

{- |
  A função 'adicionaLinha' apenas adicinona uma nova linha ao topo do @Mapa@ com
  o auxilio de um número aleatório de 0 a 100 e altera as coordenadas do jogador.
  
  === Exemplo

  >>> adicionaLinha 2 (Jogo (Jogador (1,1)) 
                      (Mapa 5 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum])]))
  Jogo (Jogador (1,2)) (Mapa 5 [(Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                                (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum])])
-}


adicionaLinha :: Int  -- ^ Inteiro aleatório de 0 a 100 
              -> Jogo -- ^ Posição do jogador e @Mapa@
              -> Jogo
adicionaLinha v (Jogo (Jogador (x,y)) (Mapa larg t))
  = let i = mod (y + v) 100 
    in Jogo (Jogador (x,y+1)) (estendeMapa (Mapa larg t) i)

{- |
  A funcão 'removeLFinal' remove a linha final do @Jogo@ fornecido pela função
  'adicionaLinha' descrita anteriormente.

  === Exemplo 

  >>> removeLFinal (Jogo (Jogador (1,2)) 
                   (Mapa 5 [(Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum]),
                            (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum])]))
  Jogo (Jogador (1,2)) (Mapa 5 [(Rio 3,[Tronco,Tronco,Tronco,Nenhum,Nenhum])])
-}


removeLFinal :: Jogo -- ^ Posição do jogador e @Mapa@
             -> Jogo
removeLFinal (Jogo (Jogador (x,y)) (Mapa larg t))
  = Jogo (Jogador (x,y)) (Mapa larg (init t))
