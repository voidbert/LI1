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

module Tarefa1_2022li1g012_Spec where

import LI12223
import Tarefa1_2022li1g012
import Test.HUnit

-- Testes unitários de linhas individuais

tl1 = "Largura=" ~: True  ~=? linhaValida 3 (Relva, [Nenhum, Arvore, Arvore])
tl2 = "Largura<" ~: False ~=? linhaValida 4 (Relva, [Nenhum, Arvore, Arvore])
tl3 = "Largura>" ~: False ~=? linhaValida 2 (Rio 0, replicate 6 Nenhum)
-- Falsa por ausência de Nenhum
tl4 = "Largura0" ~: False  ~=? linhaValida 0 (Estrada 4, [])


tl5 = "Nenhum1" ~: True  ~=? linhaValida 3 (Estrada 0, [Carro, Carro, Nenhum])
tl6 = "Nenhum2" ~: False ~=? linhaValida 4 (Rio (-1),
                               [Tronco, Tronco, Tronco, Tronco])
tl7 = "Nenhum3" ~: True  ~=? linhaValida 10 (Relva, Nenhum : replicate 9 Arvore)


tl8 = "InvalidoRel1" ~: True ~=? linhaValida 20 (Relva,
                                   replicate 3 Nenhum ++ replicate 17 Arvore)
tl9 = "InvalidoRel2" ~: False ~=? linhaValida 3 (Relva, [Carro, Nenhum, Nenhum])
tl10 = "InvalidoRel3" ~: False ~=? linhaValida 2 (Relva, [Nenhum, Tronco])
tl11 = "InvalidoRel4" ~: False ~=? linhaValida 3 (Relva, 
                                     [Arvore, Tronco, Nenhum])

tl12 = "InvalidoE1" ~: True ~=? linhaValida 2 (Estrada 0, [Carro, Nenhum])
tl13 = "InvalidoE2" ~: False ~=? linhaValida 4 (Estrada 1,
                                   [Arvore, Nenhum, Nenhum, Carro])
tl14 = "InvalidoE3" ~: False ~=? linhaValida 3 (Estrada 2, [Nenhum, Tronco])

tl15 = "InvalidoR1" ~: True ~=? linhaValida 3 (Rio 1, [Nenhum, Tronco, Nenhum])
tl16 = "InvalidoR2" ~: False ~=? linhaValida 2 (Rio 2, [Carro, Nenhum])
tl17 = "InvalidoR3" ~: False ~=? linhaValida 3 (Rio 0, [Tronco, Nenhum, Arvore])

tl18 = "Suc1" ~: True ~=? linhaValida 4 (Estrada 0, Nenhum : replicate 3 Carro)
tl19 = "Suc2" ~: False ~=? linhaValida 5 (Estrada 0, Nenhum : replicate 4 Carro)
tl20 = "Suc3" ~: True ~=? linhaValida 8 (Rio 9,
                            Nenhum : replicate 5 Tronco ++ [Nenhum, Tronco])
tl21 = "Suc4" ~: False ~=? linhaValida 7 (Rio 1, replicate 6 Tronco ++ [Nenhum])
tl22 = "Suc5" ~: True ~=? linhaValida 5 (Rio 2, Nenhum : replicate 4 Tronco)
tl23 = "Suc6" ~: True ~=? linhaValida 3 (Estrada 0, [Carro, Carro, Nenhum])

-- Testes miscelâneos de linha individuais (vários critérios)
tl24 = "Misc1" ~: True ~=? linhaValida 10 (Estrada (-1),
  [Carro, Carro, Carro, Nenhum, Carro, Nenhum, Carro, Carro, Nenhum, Nenhum])
tl25 = "Misc2" ~: True ~=? linhaValida 9 (Rio 0,
  [Nenhum, Tronco, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum])
tl26 = "Misc3" ~: False ~=? linhaValida 9 (Rio (-1),
  [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Arvore, Nenhum])
tl27 = "Misc4" ~: False ~=? linhaValida 3 (Rio 3,
  [Nenhum, Tronco, Tronco, Tronco])
tl28 = "Misc5" ~: False ~=? linhaValida 6 (Rio 2,
  [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco])
tl29 = "Misc6" ~: False ~=? linhaValida 5 (Estrada 1,
  [Carro, Nenhum, Arvore, Carro, Carro])


-- Testes de mapas por inteiro

inv1 = (Relva, replicate 6 Arvore)
inv2 = (Estrada 2, [Carro, Carro, Carro, Carro, Nenhum, Carro])
larg = (Rio 3, [Nenhum, Nenhum, Tronco])

rel = (Relva, [Nenhum, Arvore, Arvore, Arvore, Nenhum, Nenhum])
rel2 = (Relva, replicate 6 Nenhum)

riop = (Rio 2, [Tronco, Tronco, Nenhum, Tronco, Nenhum, Nenhum])
rion = (Rio (-1), [Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])

estp = (Estrada 1, [Carro, Carro, Nenhum, Carro, Carro, Carro])
estn = (Estrada (-3), [Nenhum, Nenhum, Carro, Nenhum, Nenhum, Carro])



tm1 = "Larg1" ~: False ~=? mapaValido (Mapa 6 [riop, rion, larg, estp])
tm2 = "Larg2" ~: True ~=? mapaValido (Mapa 3 [larg, (Relva, replicate 3 Nenhum)])
tm3 = "Larg3" ~: False ~=? mapaValido (Mapa 6 [estp, rel, larg])

tm4 = "Inv1" ~: False ~=? mapaValido (Mapa 6 [inv2, riop, estn])
tm5 = "Inv2" ~: False ~=? mapaValido (Mapa 6 [estp, estn, inv1])
tm6 = "Inv3" ~: False ~=? mapaValido (Mapa 6 [riop, inv1, inv2, riop])

tm7 = "Rio1" ~: True ~=? mapaValido (Mapa 6 [riop, rion, rel, rion, riop])
tm8 = "Rio2" ~: False ~=? mapaValido (Mapa 6 [estp, estp, rion, rion])
tm9 = "Rio3" ~: False ~=? mapaValido (Mapa 6 [riop, larg])
tm10 = "Rio4" ~: True ~=? mapaValido (Mapa 6 [riop, estn, estn])

tm11 = "Suc1" ~: True ~=? mapaValido (Mapa 6 [riop, rion, riop, rion])
tm12 = "Suc2" ~: False ~=? mapaValido (Mapa 6 [riop, rion, riop, rion, riop])
tm13 = "Suc3" ~: True ~=? mapaValido (Mapa 6 [estp, estp, estp, rel, rel])
tm14 = "Suc4" ~: False ~=? mapaValido (Mapa 6 [rel, estp, estp, estn, estn, estp, estn])
tm15 = "Suc5" ~: True ~=? mapaValido (Mapa 6 [rel, rel, rel, rel, rel2])
tm16 = "Suc6" ~: False ~=? mapaValido (Mapa 6 [rel, rel2, rel, rel, rel2, rel])

tm17 = "MMisc1" ~: True ~=? mapaValido (Mapa 6 [rel, estp, estp, rel, rel, rion])
tm18 = "MMisc2" ~: True ~=? mapaValido (Mapa 6
  [rel, rel, rel, rel, riop, rion, estn, estn, rion, rel, rel, estn, estp])
tm19 = "MMisc3" ~: False ~=? mapaValido (Mapa 6
  [rel, rel, rel, estp, rion, riop, rion, riop, rion])
tm20 = "MMisc4" ~: True ~=? mapaValido (Mapa 6
  [riop, estp, estp, riop, rel, rel, rel2, rel2, estp])

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [
  tl1, tl2, tl3, tl4, tl5, tl6, tl7, tl8, tl9, tl10, tl11, tl12, tl13, tl14,
  tl15, tl16, tl17, tl18, tl19, tl20, tl21, tl22, tl23, tl24, tl25, tl26, tl27,
  tl28, tl29,

  tm1, tm2, tm3, tm4, tm5, tm6, tm7, tm8, tm9, tm10, tm11, tm12, tm13, tm14,
  tm15, tm16, tm17, tm18, tm19, tm20
  ]
