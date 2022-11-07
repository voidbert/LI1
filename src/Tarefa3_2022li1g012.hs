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
import Data.Maybe

{-|
  Implementação não parcial de '(!!)', que devolve o @n@-ésimo elemento de uma
  lista. Caso o índice esteja fora dos limites da lista, @Nothing@ será
  devolvido.

  === Notas

  O primeiro elemento da lista é de índice 0.

  Complexidade: \( O(n) \)

  === Exemplos

  >>> mIndice [10, 43, 21] 2
  Just 21

  >>> mIndice [10, 43, 21] (-1)
  Nothing

  >>> mIndice [10, 43, 21] 3
  Nothing
-}
mIndice :: [a] -> Int -> Maybe a
mIndice [] n = Nothing
mIndice (x:xs) n
  | n <  0 = Nothing
  | n == 0 = Just x
  | n >  0 = mIndice xs (n - 1)

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

{-|
  Um par de duas linhas, devolvido por 'linhasJogador'. A primeira linha
  corresponde à linha onde o jogador se encontra, enquanto a segunda a para
  qual o jogador se deseja mover. Linhas @Nothing@ não existem porque estão
  fora do mapa (se o jogador está no topo do mapa e se deseja mover para cima,
  o segundo elemento do par será @Nothing@).
-}
type Linhas2 = (Maybe (Terreno, [Obstaculo]), Maybe (Terreno, [Obstaculo]))

{-|
  Um par de dois obstáculos, acompanhados pelo terreno onde se encontram,
  devolvido por 'obstaculosJogador'. O primeiro obstáculo corresponde ao por
  baixo do jogador, e o segundo ao para onde o jogador se deseja mover.
  Obstáculos @Nothing@ correspodem a obstáculos fora do mapa.
-}
type Obstaculos2 = (Maybe (Terreno, Obstaculo), Maybe (Terreno, Obstaculo))

{-|
  'deslocamento' devolve o vetor que o jogador se pretende mover numa jogada.

  === Exemplo

  >>> deslocamento (Move Direita)
  (1, 0)
-}
deslocamento :: Jogada -> Coordenadas
deslocamento Parado          = (0,    0)
deslocamento (Move Cima)     = (0, (-1))
deslocamento (Move Baixo)    = (0,    1)
deslocamento (Move Esquerda) = ((-1), 0)
deslocamento (Move Direita)  = (1,    0)

{-|
  'linhasJogador' devolve duas linhas do mapa, caso existam. Essas são, por
  ordem, a linha onde o jogador se encontra e a linha para onde se deseja
  mover, de acordo com a jogada.
-}
linhasJogador :: Jogador -- ^ Jogador (e a sua posição)
              -> Jogada  -- ^ Movimento (ou ausência dele) do jogador
              -> Mapa    -- ^ Mapa, de onde serão tiradas as linhas
              -> Linhas2 -- ^ As linhas atual e (possível) seguinte do jogador
linhasJogador (Jogador (x, y)) (Move Cima) (Mapa l lns)
  | null lns || y < 0 = (Nothing, Nothing)
  | y == 0 = (Just $ head lns, Nothing)
  | y == 1 = let (h:t) = lns in
      if null t then (Nothing, Just h) else (Just $ head t, Just h)
  | otherwise =
      linhasJogador (Jogador (x, y - 1)) (Move Cima) (Mapa l (tail lns))

linhasJogador (Jogador (x, y)) (Move Baixo) m =
  swap $ linhasJogador (Jogador (x, y + 1)) (Move Cima) m
  where swap (x,y) = (y,x) -- Esta definição evita repetição de (Move Cima)

-- Parado, Move Esquerda, Move Direita (sem movimento vertical)
linhasJogador (Jogador (_,y)) _ (Mapa _ lns) = (ln, ln)
  where ln = lns `mIndice` y

{-|
  'obstaculoLinha' @n@ devolve o @n@-ésimo obstáculo de uma linha, juntamente
  com o terreno da linha onde se encontra, caso o obstáculo exista. Em caso
  contrário, @Nothing@ é devolvido.

  === Exemplos

  >>> obstaculoLinha 3 (Just (Relva, [Nenhum, Arvore, Nenhum, Arvore]))
  Just Arvore

  >>> obstaculoLinha 5 Nothing
  Nothing

  >>> obstaculoLinha 12 (Just (Rio 2, [Nenhum, Tronco, Nenhum]))
  Nothing
-}
obstaculoLinha :: Int -- ^ Posição do obstáculo na linha
               -> Maybe (Terreno, [Obstaculo]) -- ^ Linha onde está o obstáculo
               -> Maybe (Terreno, Obstaculo) -- ^ Obstáculo (caso exista)
obstaculoLinha _ Nothing = Nothing
obstaculoLinha n (Just (ter, obs))
  | isNothing mobs = Nothing
  | otherwise = Just (ter, fromJust mobs)
  where mobs = obs `mIndice` n

{-|
  'obstaculosJogador' devolve um par com os obstáculos (acompanhados pelos
  terrenos em que cada um se encontra), onde o jogador está e onde o jogador
  deseja estar na posição seguinte. Caso um desses obstáculos não exista (fora
  do mapa), @Nothing@ estará nessa posição do par.

  === Exemplos

  @@@
  m = Mapa 4 [ (Relva    , [Nenhum, Arvore, Arvore, Arvore]),
               (Rio 3    , [Tronco, Tronco, Nenhum, Tronco]),
               (Estrada 1, [Nenhum, Carro, Carro, Nenhum])]
  @@@

  >>> obstaculosJogador (Jogador (0, 2)) (Move Cima) m
  (Just (Estrada 1, Nenhum), Just (Rio 3, Tronco))

  >>> obstaculosJogador (Jogador (0, 2)) (Move Esquerda) m
  (Just (Estrada 1, Nenhum), Nothing)
-}
obstaculosJogador :: Jogador -- ^ Jogador (pela sua posição)
                  -> Jogada -- ^ Movimento (ou ausência dele) do jogador
                  -> Mapa -- ^ Mapa onde se encontra o jogador
                  -> Obstaculos2 -- ^ Obstáculos atual e possível seguinte
obstaculosJogador jgd@(Jogador (xi, yi)) j m = (o1, o2)
  where (l1, l2) = linhasJogador jgd j m
        (xf, yf) = let (dx, dy) = deslocamento j in (xi + dx, yi + dy)
        o1       = obstaculoLinha xi l1
        o2       = obstaculoLinha xf l2

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = undefined
