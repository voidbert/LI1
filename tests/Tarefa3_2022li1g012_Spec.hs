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



-- Testes dos obstáculos do jogador (onde está e para onde deseja ir)

m = Mapa 5 [(Relva,        [Nenhum, Arvore, Arvore, Nenhum, Arvore]),
            (Rio 2,        [Tronco, Tronco, Nenhum, Tronco, Tronco]),
            (Estrada (-1), [Nenhum, Carro,  Carro,  Nenhum, Carro])]

-- Alguns testes com o jogador fora do mapa (OOB - out of bounds)
oj1 = "OOB1" ~: (Nothing, Nothing) ~=?
  obstaculosJogador (Jogador (-1, -1)) (Move Baixo) m
oj2 = "OOB2" ~: (Nothing, Nothing) ~=?
  obstaculosJogador (Jogador (-1, 3)) (Move Direita) m
oj3 = "OOB3" ~: (Nothing, Nothing) ~=?
  obstaculosJogador (Jogador (5, -1)) (Move Cima) m
oj4 = "OOB4" ~: (Nothing, Nothing) ~=?
  obstaculosJogador (Jogador (5, 3)) Parado m

-- Jogador fora do mapa a mover-se para o mapa ou vice-versa (IB - in bounds)
oj5 = "IB1" ~: (Just (Relva, Nenhum), Nothing) ~=?
  obstaculosJogador (Jogador (0, 0)) (Move Esquerda) m
oj6 = "IB2" ~: (Nothing, Just (Relva, Arvore)) ~=?
  obstaculosJogador (Jogador (2, -1)) (Move Baixo) m
oj7 = "IB3" ~: (Just (Rio 2, Tronco), Nothing) ~=?
  obstaculosJogador (Jogador (4, 1)) (Move Direita) m
oj8 = "IB4" ~: (Nothing, Just (Estrada (-1), Carro)) ~=?
  obstaculosJogador (Jogador (2, 3)) (Move Cima) m

-- Jogador dentro do mapa
oj9 = "In1" ~: (Just (Rio 2, Tronco), Just (Relva, Arvore)) ~=?
  obstaculosJogador (Jogador (1, 1)) (Move Cima) m
oj10 = "In2" ~: (Just (Rio 2, Tronco), Just (Estrada (-1), Carro)) ~=?
  obstaculosJogador (Jogador (4, 1)) (Move Baixo) m
oj11 = "In3" ~: (Just (Relva, Nenhum), Just (Relva, Arvore)) ~=?
  obstaculosJogador (Jogador (3, 0)) (Move Esquerda) m
oj12 = "In4" ~: (Just (Rio 2, Tronco), Just (Rio 2, Tronco)) ~=?
  obstaculosJogador (Jogador (0, 1)) (Move Direita) m
oj13 = "In5" ~: (Just (Estrada (-1), Nenhum), Just (Estrada (-1), Nenhum)) ~=?
  obstaculosJogador (Jogador (3, 2)) Parado m

-- Testes de animaJogador

-- Colisões com árvores e bordas do mapa
aj1 = "Col1" ~: Jogador (3, 0) ~=?
  animaJogador (Jogador (3, 0)) (Move Esquerda) m
aj2 = "Col2" ~: Jogador (3, 0) ~=?
  animaJogador (Jogador (3, 0)) (Move Direita) m
aj3 = "Col3" ~: Jogador (3, 2) ~=?
  animaJogador (Jogador (3, 2)) (Move Baixo) m
aj4 = "Col4" ~: (Jogador (0, 2)) ~=?
  animaJogador (Jogador (0, 3)) (Move Cima) m -- o jogador pode reentrar no mapa
aj5 = "Col5" ~: (Jogador (5, 5)) ~=?
  animaJogador (Jogador (5, 5)) (Move Esquerda) m

-- Movimentos laterais em troncos
aj6 = "Lat1" ~: Jogador (3, 1) ~=?
  animaJogador (Jogador (1, 1)) Parado m
aj7 = "Lat2" ~: Jogador (2, 1) ~=?
  animaJogador (Jogador (1, 1)) (Move Esquerda) m
aj8 = "Lat3" ~: Jogador (6, 1) ~=?
  animaJogador (Jogador (4, 1)) Parado m -- pode sair do mapa
aj9 = "Lat4" ~: Jogador (-2, 0) ~=?
  -- jogador não afetado pela circularidade
  animaJogador (Jogador (1, 0)) (Move Direita)
  (Mapa 3 [(Rio (-4), [Nenhum, Tronco, Tronco])])

-- Movimento vertical

m2 = Mapa 4 [ (Rio (-1), [Nenhum, Tronco, Tronco, Nenhum]),
              (Rio 2,    [Tronco, Tronco, Nenhum, Tronco]),
              (Relva,    [Nenhum, Nenhum, Nenhum, Arvore])]

-- Sem colisões
aj10 = "Vert1" ~: Jogador (1, 1) ~=?
  animaJogador (Jogador (1, 2)) (Move Cima) m2
aj11 = "Vert2" ~: Jogador (0, 0) ~=?
  animaJogador (Jogador (0, 1)) (Move Cima) m2
aj12 = "Vert3" ~: Jogador (2, 1) ~=?
  animaJogador (Jogador (2, 0)) (Move Baixo) m2
-- Com colisões
aj13 = "Vert4" ~: Jogador (5, 1) ~=?
  animaJogador (Jogador (3, 1)) (Move Baixo) m2
aj14 = "Vert5" ~: Jogador (0, 0) ~=?
  animaJogador (Jogador (1, 0)) (Move Cima) m2

-- animaJogo anima o jogador e o mapa individualmente, algo que já foi testado
-- em profundidade. Apenas um exemplo:

ma = Mapa 5 [(Relva,        [Nenhum, Arvore, Arvore, Nenhum, Arvore]),
             (Rio 2,        [Tronco, Tronco, Tronco, Tronco, Nenhum]),
             (Estrada (-1), [Carro,  Carro,  Nenhum, Carro, Nenhum])]

ajg = "Jogo" ~: Jogo (Jogador (3, 1)) ma ~=?
  animaJogo (Jogo (Jogador (1, 1)) m) (Move Cima)

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [
  TestLabel "Testes de Linhas individuais" $ test [
    l1, l2, l3, l4, l5, l6, l7, l8, l9, l11, l12, l13, l14, l15 ],
  TestLabel "Testes de obstaculosJogador" $ test [
    oj1, oj2, oj3, oj4, oj5, oj6, oj7, oj8, oj9, oj10, oj11, oj12, oj13 ],
  TestLabel "Testes de animaJogador" $ test [
    aj1, aj2, aj3, aj4, aj5, aj6, aj7, aj8, aj9, aj10, aj11, aj12, aj13, aj14
  ], ajg
  ]
