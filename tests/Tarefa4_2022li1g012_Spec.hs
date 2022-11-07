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

module Tarefa4_2022li1g012_Spec where

import LI12223
import Tarefa4_2022li1g012
import Test.HUnit

-- Testes para colisão de obstáculos em diferentes linhas/terrenos:
-- Estrada:

to1 = "TesteEstradaFalse" ~: False ~=? jogoTerminou (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro])]))
to2 = "TesteEstradaTrue"  ~: True  ~=? jogoTerminou (Jogo (Jogador (0,2)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))

-- Rio:

to3 = "TesteRioTrue"      ~: True  ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Estrada 1,[Carro,Nenhum,Carro])]))
to4 = "TesteRioFalse"     ~: False ~=? jogoTerminou (Jogo (Jogador (2,1)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Rio 2,[Nenhum,Nenhum,Tronco])]))

-- Relva:

to5 = "TesteRelvaFalse"   ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro])]))
to6 = "TesteRelvaFalsel1" ~: False ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))

-- Testes para quando o jogador se encontra fora do mapa:
-- Por Cima/Baixo:

to7 = "ForaMapaporBaixo"  ~: True  ~=? jogoTerminou (Jogo (Jogador (0,2)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))
to8 = "ForaMapaporCima"   ~: True  ~=? jogoTerminou (Jogo (Jogador (0,(-1))) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))   

-- Pela Esquerda/Direita:

to9 = "ForaMapaDireita"   ~: True  ~=? jogoTerminou (Jogo (Jogador (3,0)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))
to10= "ForaMapaEsquerda"  ~: True  ~=? jogoTerminou (Jogo (Jogador ((-1),0)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Arvore])]))


testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [to1,to2,to3,to4,to5,to6,to7,to8,to9,to10]
