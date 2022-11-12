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

to5 = "5 Estradas seguidas" ~: Mapa 1 [(Rio 1,[Nenhum]),(Estrada 2,[Nenhum]),
                                       (Estrada 2,[Nenhum]),(Estrada 2,[Nenhum]),
                                       (Estrada 2,[Nenhum]),(Estrada 2,[Nenhum])]
                            ~=? estendeMapa (Mapa 1 [(Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum]),
                                                     (Estrada 2, [Nenhum])]) 2

to6 = "5 Relvas seguidas"   ~: Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum]),
                                       (Relva,[Nenhum]),(Relva,[Nenhum]),
                                       (Relva,[Nenhum]),(Relva,[Nenhum])]
                            ~=? Mapa 1 [(Estrada 1,[Nenhum]),(Relva,[Nenhum]),
                                        (Relva,[Nenhum]),(Relva,[Nenhum]),
                                        (Relva,[Nenhum]),(Relva,[Nenhum])]



testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [to3]
