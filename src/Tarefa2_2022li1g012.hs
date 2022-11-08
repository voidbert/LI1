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
Module      : Tarefa2_2022li1g012
Description : Geração contínua de um mapa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g012 where

import LI12223


-- Função que conta os primeiros + ultimos elementos de um
-- Conjunto de Terrenos/Obstaculos

contaConsecutivos :: (a -> Bool) -> [a] -> Int
contaConsecutivos f xs = length $ fst $ span f xs

-- Função Auxiliar de validação de terrenos

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l [])    = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Rio v , _):xs))
  | contarTerrenos (Rio v) lns     < 4 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Estrada v , _):xs))
  | contarTerrenos (Estrada v) lns < 5 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Rio 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Relva, _):xs))
  | contarTerrenos (Relva) lns     < 5 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Rio 0, Estrada 0]

-- Funções Auxiliares para "proximosTerrenosValidos"
-- Conta o numero de terrenos consecutivos de uma dada lista

contarTerrenos :: Terreno -> [(Terreno,[Obstaculo])] -> Int
contarTerrenos t ts = contaConsecutivos (tipologiaTerreno t) $ map (\ (t,o) -> t) ts

tipologiaTerreno :: Terreno -> Terreno -> Bool
tipologiaTerreno (Rio _)     (Rio _)     = True
tipologiaTerreno (Estrada _) (Estrada _) = True
tipologiaTerreno Relva       Relva       = True
tipologiaTerreno _           _           = False

-- Função Auxiliar de validação de obstáculos

-- Funcões Auxiliares para "proximosObstaculosValidos"
-- Quando no final da lista, verifica a circularidade do
-- Mapa calculando a quantidade de obstáculos do inicio
-- E o do fim da lista dada 

obstaculosCirculares :: [Obstaculo] -> Int
obstaculosCirculares [] = 0
obstaculosCirculares l@(o:obs)
  | head r == o = (contaConsecutivos (== o) l) + (contaConsecutivos (==o) r)
  | otherwise   = (contaConsecutivos (== o) l)
  where r = reverse l

-- se só falta um e não há nenhums -> nenhum
-- otherwise -> qualquer outra coisa valida (nenhum, obstaculo caso comprimento correto)

proximosObs :: Int  -- ^ Comprimento final da lista
            -> (Terreno, [Obstaculo]) 
            -> [Obstaculo]
proximosObs l (Rio _, o@(_:obs))
  | length o == l                                                        = []
  | l - length o == 1 && not (elem Nenhum o)                             = [Nenhum]
  | l - length o == 1 && obstaculosCirculares o                      < 5 = [Nenhum, Tronco] 
  | l - length o /= 1 && contaConsecutivos (== Tronco) (reverse obs) < 5 = [Nenhum, Tronco]
  | otherwise                                                            = [Nenhum]
proximosObs l (Estrada _, o@(_:obs))
  | length o == l                                                        = []
  | l - length o == 1 && not (elem Nenhum o)                             = [Nenhum]
  | l - length o == 1 && obstaculosCirculares o                      < 3 = [Nenhum, Carro] 
  | l - length o /= 1 && contaConsecutivos (== Tronco) (reverse obs) < 5 = [Nenhum, Carro]
  | otherwise                                                            = [Nenhum]
proximosObs l (Relva, o@(_:obs))
  | length o == l                                                        = []
  | l - length o == 1 && not (elem Nenhum o)                             = [Nenhum]
  | otherwise                                                            = [Nenhum, Relva]
  






