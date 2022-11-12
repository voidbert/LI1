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
module Tarefa4_2022li1g012 where

import LI12223
import Data.Maybe


{-|
  A função 'jogoTerminou' avalia se o Jogador morreu devido a se encontrar na 
  posição de obstáculos não adequados, ou por se encontrar fora dos limites do
  mapa.

  === Exemplos

  >>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro])]))
  False

  >>> jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro])]))
  True

  >>> jogoTerminou (Jogo (Jogador (3,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro])]))
  True
-}


jogoTerminou :: Jogo 
             -> Bool
jogoTerminou (Jogo jgd@(Jogador (x,_)) (Mapa l lns))
  | not (dentroMapaLados l jgd) = True
  | isNothing ln = True -- dentro dos limites verticais (linha achada no mapa)
  | otherwise = let jln@(ter, jlno) = fromMaybe (Relva, []) ln
                    obs          = jlno !! x
                in not (obstaculoAdequado ter obs)
  where ln = linhaJogador jgd lns

{-|
  A função 'dentroMapaLados' avalia se o Jogador está dentro do mapa.

  === Exemplos

  >>> dentroMapaLados 4 (Jogador (1,1))
  True

  >>> dentroMapaLados 1 (Jogador (2,1))
  False
-}

dentroMapaLados :: Largura 
                -> Jogador
                -> Bool
dentroMapaLados l (Jogador (x,y)) = x >= 0 && x < l

{-|
  A função 'linhaJogador' devolve a linha em que o jogador se encontra no mapa.
  Se a galinha não for encontrada, ela está fora do mapa pelos limites do topo
  ou pelos limites de baixo, pelo que a função devolve @Nothing@.

  === Exemplos

  >>> linhaJogador (Jogador (1,1)) [(Rio 1, [Nenhum, Tronco]),(Estrada 1, [Carro, Nenhum])]
  Just (Estrada 1,[Carro,Nenhum])

  >>> linhaJogador (Jogador (1,0)) [(Relva, [Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum])]
  Just (Relva,[Nenhum,Arvore])

  >>> linhaJogador (Jogador (0,2)) [(Relva, [Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum])]
  Nothing
-}

linhaJogador :: Jogador
             -> [(Terreno, [Obstaculo])]
             -> Maybe (Terreno, [Obstaculo])
linhaJogador _ [] = Nothing
linhaJogador (Jogador (x, y)) (l:ls)
  | y <  0    = Nothing
  | y == 0    = Just l
  | otherwise = linhaJogador (Jogador (x, y - 1)) ls

{-|
  A função 'obstaculoAdequado'  @t o@ avalia se um obstáculo o@ é apropriado 
  para o Jogador, ou se impede a sobrevivência do mesmo.

  Para esta função consideramos que o jogador não se pode encontrar na mesma
  posição do que o obstáculo @Nenhum@, quando em @Rios _@ e do que obstáculo 
  @Carro@, quando em @Estradas _@. No terreno @Relva@ o Jogador só ocupa a 
  posição de obstáculos @Nenhum@, pelo que nunca afeta a sua sobrevivência.

  === Exemplos

  >>> obstaculoAdequado (Rio (-1)) Nenhum
  False

  >>> obstaculoAdequado (Relva) Arvore
  True

  >>> obstaculoAdequado (Estrada 1) Nenhum
  True
-}

obstaculoAdequado :: Terreno
                  -> Obstaculo
                  -> Bool
obstaculoAdequado (Rio _) o     = o /= Nenhum
obstaculoAdequado (Estrada _) o = o /= Carro
obstaculoAdequado Relva o       = True
