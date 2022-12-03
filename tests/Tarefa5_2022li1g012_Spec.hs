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

module Tarefa5_2022li1g012_Spec where

import LI12223
import Tarefa5_2022li1g012
import Test.HUnit

t0 = "Uma linha de Mapa e 0 de valor aleatório"   
    ~: Jogo (Jogador (0,1)) (Mapa 2 [(Estrada 1,[Carro,Nenhum])])
    ~=? deslizaJogo 0 (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1,[Carro,Nenhum])]))

t1 = "Duas linhas de Mapa"
    ~: Jogo (Jogador (0,1)) (Mapa 2 [(Estrada 1,[Carro,Nenhum]),
                                     (Rio 1,[Tronco,Nenhum])])
    ~=? deslizaJogo 1 (Jogo (Jogador (0,0)) (Mapa 2 [(Rio 1,[Tronco,Nenhum]),
                                                     (Relva ,[Nenhum,Arvore])]))

-- Verificação do parametro velocidade em 2 Rios consecutivos

t2 = "Dois Rios Seguidos"
    ~: Jogo (Jogador (0,1)) (Mapa 2 [(Rio (-1),[Tronco,Nenhum]),
                                     (Rio 1,[Tronco,Nenhum])])
    ~=? deslizaJogo 3 (Jogo (Jogador (0,0)) (Mapa 2 [(Rio 1,[Tronco,Nenhum]),
                                                     (Relva ,[Nenhum,Arvore])]))

t3 = "Mapa vazio com potencial de criar uma linha"
    ~: Jogo (Jogador (0,1)) (Mapa 2 [])
    ~=? deslizaJogo 0 (Jogo (Jogador (0,0)) (Mapa 2 [])) 

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test [t0,t1,t2,t3]
