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

module Tarefa3_2022li1g012_Spec where

import LI12223
import Tarefa3_2022li1g012
import Test.HUnit

-- Testes de animação de linhas. O teste extensivo de mapas é desnecessário,
-- dado que animaMapa anima cada linha individualmente.

lrel = [Nenhum, Nenhum, Arvore, Arvore, Nenhum]
lrio = [Nenhum, Tronco, Tronco, Nenhum, Tronco]
lest = [Nenhum, Carro, Carro, Carro, Nenhum, Carro]

l1 = "Parado1" ~: (Relva, lrel) ~=? animaLinha 5 (Relva, lrel)
l2 = "Parado2" ~: (Rio 0, lrio) ~=? animaLinha 5 (Rio 0, lrio)
l3 = "Mod+"    ~: (Rio 25, lrio) ~=? animaLinha 5 (Rio 25, lrio)
l4 = "Mod-"    ~: (Estrada (-12), lest) ~=? animaLinha 6 (Estrada (-12), lest)

l5 = "Rio+"    ~: (Rio 2, [Nenhum, Tronco, Nenhum, Tronco, Tronco]) ~=?
                    animaLinha 5 (Rio 2, lrio)
l6 = "Rio-"    ~: (Rio (-1), [Tronco, Tronco, Nenhum, Tronco, Nenhum]) ~=?
                    animaLinha 5 (Rio (-1), lrio)
l7 = "RioMod+" ~: (Rio 6, [Tronco, Nenhum, Tronco, Tronco, Nenhum]) ~=?
                    animaLinha 5 (Rio 6, lrio)
l8 = "RioMod-" ~: (Rio (-7), [Tronco, Nenhum, Tronco, Nenhum, Tronco]) ~=?
                    animaLinha 5 (Rio (-7), lrio)

l9  = "Est+" ~: (Estrada 3, [Carro, Nenhum, Carro, Nenhum, Carro, Carro]) ~=?
                 animaLinha 6 (Estrada 3, lest)
l10 = "Est-" ~: (Estrada (-2), [Carro, Carro, Nenhum, Carro, Nenhum, Carro]) ~=?
                  animaLinha 6 (Estrada (-2), lest)
l11 = "EstMod+" ~: (Estrada 8, [Nenhum, Carro, Nenhum, Carro, Carro, Carro]) ~=?
                     animaLinha 6 (Estrada 8, lest)
l12 = "EstMod-" ~: (Estrada (-8), [Carro, Carro, Nenhum, Carro, Nenhum, Carro])
                     ~=? animaLinha 6 (Estrada (-8), lest)

l13 = "Parcial1" ~: (Rio 2,     []) ~=? animaLinha 0 (Rio 2,     [])
l14 = "Parcial2" ~: (Estrada 0, []) ~=? animaLinha 0 (Estrada 0, [])

-- Teste de composição: mover uma linha n elementos para a direita de uma vez
-- deve ser igual a mover essa linha 1 elemento para a direita n vezes
animaLinha' :: Largura -> (Terreno, [Obstaculo]) -> (Terreno, [Obstaculo])
animaLinha' l (t, o) = if velocidadeTerreno t == 0 then
                         (t, o) else (t, snd $ animaLinha l (t1, on))
  where (_, on)  = animaLinha' l (t2, o)
        (t1, t2) = case t of
                     Relva     -> (Relva, Relva)
                     Rio n     -> if n > 0 then (Rio 1, Rio (n - 1))
                                    else (Rio (-1), Rio (n + 1))
                     Estrada n -> if n > 0 then (Estrada 1, Estrada (n - 1))
                                    else (Estrada (-1), Rio (n + 1))

l15 = "Composta" ~: True ~=? all
  (\ n -> animaLinha' 5 (Rio n, lrio) == animaLinha 5 (Rio n, lrio)) [-10..10]

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [
  TestLabel "Testes de Linhas individuais" $ test [
    l1, l2, l3, l4, l5, l6, l7, l8, l9, l11, l12, l13, l14, l15 ]
  ]
