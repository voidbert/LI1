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
Module      : Tarefa3_2022li1g012
Description : Movimentação do personagem e obstáculos
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g012 where

import LI12223

{-|
  'velocidadeTerreno' devolve a velocidade com que se move uma linha com um
  dado tipo de terreno.

  === Exemplos

  >>> velocidadeTerreno Relva
  0

  >>> velocidadeTerreno (Rio 4)
  4
-}
velocidadeTerreno :: Terreno -- ^ Qualquer tipo de terreno de uma linha
                  -> Int     -- ^ A sua velocidade
velocidadeTerreno Relva       = 0
velocidadeTerreno (Rio n)     = n
velocidadeTerreno (Estrada n) = n

{-|
  'animaLinha' devolve a linha resultante após o seu movimento numa jogada.
  Obstáculos em terrenos com velocidade @n@, movem-se @n@ posições para a
  direita ou para a esquerda, caso @n@ seja positivo ou negativo,
  respetivamente. Quando, no seu movimento, um obstáculo sai das bordas da
  linha é colocado no início da mesma (mapa circular).

  === Notas

  Esta função suporta velocidades superiores ao comprimento da linha, graças a
  aritmética modular (como os elementos ressurgem na linha, por exemplo, mover
  uma linha com 5 obstáculos 5 vezes para a direita é equivalente a ficar-se
  com a mesma linha).

  Complexidade: \( O(n) \)

  === Exemplos

  >>> animaLinha 6 (Rio 2, [Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
  (Rio 2, [Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum])

  >>> animaLinha 4 (Estrada (-1), [Carro, Carro, Nenhum, Carro])
  (Estrada (-1), [Carro, Nenhum, Carro, Carro])

  >>> animaLinha 4 (Estrada (-5), [Carro, Carro, Nenhum, Carro])
  (Estrada (-5), [Carro, Nenhum, Carro, Carro])
-}
animaLinha :: Largura                -- ^ Largura do mapa
           -> (Terreno, [Obstaculo]) -- ^ Linha a ser animada
           -> (Terreno, [Obstaculo]) -- ^ Linha no seu estado seguinte
animaLinha l (t, os)
  | vt <  0 = let (a, d) = splitAt v       os in (t, d ++ a)
  | vt == 0 = (t, os)
  | vt >  0 = let (a, d) = splitAt (l - v) os in (t, d ++ a)
  where vt = velocidadeTerreno t
        v = mod (abs vt) l

{-|
  'animaMapa' move todas as linhas de um mapa __válido__ com recurso a
  'animaLinha'.

  === Notas

  Complexidade: \( O(n \times m) \), \(n\) linhas de largura \(m\)

  === Exemplo

  >>> animaMapa (Mapa 4 [ (Estrada 1, [Carro, Carro, Nenhum, Nenhum]),
                          (Rio (-1), [Tronco, Nenhum, Tronco, Tronco]) ])
  Mapa 4 [ (Estrada 1, [Nenhum,Carro,Carro,Nenhum]),
           (Rio (-1), [Nenhum,Tronco,Tronco,Tronco] )]
-}
animaMapa :: Mapa
          -> Mapa
animaMapa (Mapa l []) = Mapa l []
animaMapa (Mapa l (x:xs)) = Mapa l ((animaLinha l x) : n)
  where (Mapa _ n) = animaMapa (Mapa l xs)

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = undefined
