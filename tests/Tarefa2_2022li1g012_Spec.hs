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

module Tarefa2_2022li1g012_Spec where

import LI12223
import Tarefa2_2022li1g012
import Test.HUnit

-- Testes para a função principal 'estendeMapa':

to  = "Próximo é Estrada"   
    ~: Mapa 5 [(Estrada 1,[Carro,Carro,Carro,Nenhum,Nenhum])]
    ~=? estendeMapa (Mapa 5 []) 1 

to1 = "lista vazia"         ~: Mapa 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum])]
                            ~=? estendeMapa (Mapa 4 []) 5

to2 = "lista com elementos" ~: Mapa 1 [(Relva,[Nenhum]),(Relva,[Nenhum])]     
                            ~=? estendeMapa (Mapa 1 [(Relva, [Nenhum])]) 5

to3 = "4 Rios seguidos"     ~: Mapa 1 [(Relva,[Nenhum]),(Rio 1,[Nenhum]),
                                       (Rio (-1),[Nenhum]),(Rio 3,[Nenhum]),
                                       (Rio (-4),[Nenhum])]
                            ~=? estendeMapa (Mapa 1 [(Rio 1, [Nenhum]),
                                                    (Rio (-1), [Nenhum]),
                                                    (Rio 3, [Nenhum]),
                                                    (Rio (-4), [Nenhum])]) 3

to4 = "Rios consecutivos"   ~: Mapa 1 [(Rio (-1),[Nenhum]),(Rio 1,[Nenhum])]  
                            ~=? estendeMapa (Mapa 1 [(Rio 1, [Nenhum])]) 3

to5 = "Rios consecutivos 2" ~: Mapa 1 [(Rio 1,[Nenhum]),(Rio (-1),[Nenhum])]
                            ~=? estendeMapa (Mapa 1 [(Rio (-1), [Nenhum])]) 3

to6 = "5 Estradas seguidas" ~: Mapa 1 [(Rio 1,[Nenhum]),(Estrada 2,[Nenhum]),
                                       (Estrada 2,[Nenhum]),(Estrada 2,[Nenhum]),
                                       (Estrada 2,[Nenhum]),(Estrada 2,[Nenhum])]
                            ~=? estendeMapa (Mapa 1 [(Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum])]) 2

to7 = "5 Relvas seguidas"   ~: Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum]),
                                       (Relva,[Nenhum]),(Relva,[Nenhum]),
                                       (Relva,[Nenhum]),(Relva,[Nenhum])]
                            ~=? Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum]),
                                        (Relva,[Nenhum]),(Relva,[Nenhum]),
                                        (Relva,[Nenhum]),(Relva,[Nenhum])]

to8 = "Largura 0"           ~: Mapa 0 []
                            ~=? estendeMapa (Mapa 0 []) 2

to9 = "Verificar Obstáculos para Rio"     ~: Mapa 2 [(Rio 1,[Tronco,Nenhum])]
                                          ~=? estendeMapa (Mapa 2 []) 3

to10 = "Verificar Obstáculos para Estrada"~: Mapa 2 [(Estrada 1,[Carro,Nenhum])]
                                          ~=? estendeMapa (Mapa 2 []) 1
                
to11 = "Verificar Obstáculos para Relva"  ~: Mapa 2 [(Relva,[Arvore,Nenhum])]
                                          ~=? estendeMapa (Mapa 2 []) 5

to12 = "Inteiro @i@ = 0"    ~: Mapa 2 [(Rio 1,[Nenhum,Nenhum])]
                            ~=? estendeMapa (Mapa 2 []) 0

to13 = "Inteiro @i@ = 100"  ~: Mapa 2 [(Estrada 1,[Nenhum,Nenhum])]
                            ~=? estendeMapa (Mapa 2 []) 100

to14 = "@i@ menor que @l@ e maior que 0" 
     ~: Mapa 3 [(Estrada 1,[Carro,Carro,Nenhum]),
                (Relva,[Arvore,Nenhum,Arvore])]
     ~=? estendeMapa (Mapa 3 [(Relva, [Arvore,Nenhum,Arvore])]) 1

to15 = "@i@ igual a @l@"    ~: Mapa 1 [(Estrada 1,[Nenhum]),(Estrada 3,[Nenhum])]
                            ~=? estendeMapa (Mapa 1 [(Estrada 3, [Nenhum])]) 1

-- Testes para a função secundária 'proximosTerrenosValidos':

to16 = "Lista vazia"     ~: [Rio 0,Estrada 0,Relva]
                         ~=? proximosTerrenosValidos (Mapa 0 [])

to17 = "4 rios seguidos" ~: [Estrada 0,Relva]
                         ~=? proximosTerrenosValidos (Mapa 0 [(Rio 1,[]),
                                                              (Rio (-2),[]),
                                                              (Rio 1,[]),
                                                              (Rio (-1),[])])

to18 = "5 estradas seguidas" ~: [Rio 0,Relva]
                             ~=? proximosTerrenosValidos (Mapa 0 [(Estrada 2, []),
                                                                  (Estrada 1, []),
                                                                  (Estrada (-3), []),
                                                                  (Estrada 1, []),
                                                                  (Estrada 5, [])])

to19 = "5 relvas seguidas"  ~: [Rio 0,Estrada 0]
                            ~=? proximosTerrenosValidos (Mapa 0 [(Relva, []),
                                                                 (Relva, []),
                                                                 (Relva, []),
                                                                 (Relva, []),
                                                                 (Relva, [])])

to20 = "3 terrenos diferentes" ~: [Rio 0,Estrada 0,Relva]
                               ~=? proximosTerrenosValidos (Mapa 0 [(Relva, []),
                                                                    (Rio 1, []),
                                                                    (Estrada (-1), [])])

-- Testes para a função secundária 'proximosObstáculosVálidos':

t21 = "Restrição por circularidade" 
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 5 (Estrada 2, [Carro,Nenhum,Carro,Carro])

t22 = "Nenhum obstáculo @Nenhum@"
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 1 (Relva, [])

t23 = "Nenhum obstáculo @Nenhum@ 2"
    ~: [Nenhum,Arvore]
    ~=? proximosObstaculosValidos 2 (Relva, [])

t24 = "3 carros seguidos" 
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 5 (Estrada 2, [Carro, Carro, Carro])

t25 = "5 troncos seguidos"
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 8 (Rio 1, [Tronco,Tronco,Tronco,Tronco,Tronco])

t26 = "@l@ == (@lgt@-1)"
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 3 (Rio 1, [Tronco,Tronco])

t27 = "@l@ == (@lgt@-1) 2"
    ~: [Nenhum,Tronco]
    ~=? proximosObstaculosValidos 3 (Rio 1, [Tronco,Nenhum])

t28 = "@l@ == @lgt@"
    ~: []
    ~=? proximosObstaculosValidos 3 (Rio 1, [Tronco,Nenhum,Tronco])

t29 = "Restrição por circularidade 2"
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 7 (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco])

t30 = "Terreno @Relva@ sem @Nenhum@"
    ~: [Nenhum]
    ~=? proximosObstaculosValidos 3 (Relva , [Arvore,Arvore])

t31 = "Terreno @Relva@ com @Nenhum@"
    ~: [Nenhum,Arvore]
    ~=? proximosObstaculosValidos 3 (Relva , [Arvore,Nenhum])

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [to,to1,to2,to3,to4,to5,to6,to7,
                                              to8,to9,to10,to11,to12,to13,to14,
                                              to15,to16,to17,to18,to19,to20,
                                              t21,t22,t23,t24,t25,t26,t27,t28,
                                              t29,t30,t31]
