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

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23. Animação de um jogo (jogador e
mapa) de acordo com os critérios em:

[Especificações](https://gitlab.com/uminho-di/li1/2223/projetos/2022li1g012/-/blob/main/spec/Fase1.pdf)
-}
module Tarefa3_2022li1g012 (
  -- * Funções expostas
  animaJogo, animaMapa, animaMapa', animaLinha, animaLinha', animaJogador,
  -- * Funções e tipos de dados auxiliares
  -- ** Gerais
  mIndice, velocidadeTerreno, deslocamento,
  -- ** Obstáculos do jogador
  Obstaculos2, obstaculosJogador, obstaculoLinha, Linhas2, linhasJogador
  ) where

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
mIndice :: [a]     -- ^ A lista onde encontrar o elemento
        -> Int     -- ^ O índice do elemento desejado
        -> Maybe a -- ^ O elemento caso exista
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
  Obstáculos em terrenos com velocidade \(v\), movem-se \(|v|\) posições para a
  direita ou para a esquerda, caso \(v\) seja positivo ou negativo,
  respetivamente. Quando, no seu movimento, um obstáculo sai das bordas da
  linha é colocado no início da mesma (mapa circular). Ver 'animaLinha'' para a
  versão utilizada na implementação prática.

  === Notas

  Esta função suporta velocidades superiores ao comprimento da linha, graças a
  aritmética modular (como os elementos ressurgem na linha, por exemplo, mover
  uma linha com 5 obstáculos 5 vezes para a direita é equivalente a ficar-se
  com a mesma linha).

  Esta função pode apresentar comportamento inesperado caso a largura do mapa
  não seja igual à da linha.

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
animaLinha 0 l = l
animaLinha l (t, os)
  | vt <  0 = let (a, d) = splitAt v       os in (t, d ++ a)
  | vt == 0 = (t, os)
  | vt >  0 = let (a, d) = splitAt (l - v) os in (t, d ++ a)
  where vt = velocidadeTerreno t
        v = mod (abs vt) l
{-|
  'animaLinha'' apresenta um comportamento semelhante a 'animaLinha'. No
  entanto, para o movimento de uma @Estrada@ quando o jogador é atropelado. Tal
  é necessário para 'Tarefa4_2022li1g012.jogoTerminou' funcionar perfeitamente
  em @Estrada@s de alta velocidade (assim é impossível o @Carro@ atravessar o
  jogador sem o atropelar).

  === Implementação

  'animaLinha'' recorre a 'animaLinha' para animar linhas onde o jogador não
  se encontra, e nas em que se encontra mas que não são estradas. Para um
  jogador se encontrar numa linha, a sua coordenada vertical é nula. Quando o
  jogador se encontra numa estrada, essa é animada várias vezes com velocidade
  de apenas 1 ou -1, até um dos seguintes:

  1. Um carro atropela o jogador (estão na mesma posição);
  2. A @Estrada@ move-se todas as unidades desejadas (@Estrada 5@ move-se uma
      unidade para a direita 5 vezes).

  === Exemplos:

  A @Estrada@ apenas se move uma unidade para parar o @Carro@ quando atropela o
  jogador:

  >>> animaLinha' 4 (Jogador (1, 0)) (Estrada 2, [Carro, Nenhum, Nenhum, Carro])
  (Estrada 2, [Carro, Carro, Nenhum, Nenhum])

  Não há atropelamento:

  >>> animaLinha' 4 (Jogador (0, 0)) (Estrada 1, [Nenhum, Nenhum, Carro, Nenhum])
  (Estrada 1, [Nenhum, Nenhum, Nenhum, Carro])

  Uso de 'animaLinha', dado que o jogador não está na @Estrada@ em questão.

  >>> animaLinha' 4 (Jogador (3, 5)) (Estrada 3, [Carro, Carro, Nenhum, Nenhum])
  (Estrada 3, [Carro, Nenhum, Nenhum, Carro])
-}
animaLinha' :: Largura                -- ^ Largura do mapa
            -> Jogador                -- ^ Posição do jogador
            -> (Terreno, [Obstaculo]) -- ^ Linha a ser animada
            -> (Terreno, [Obstaculo]) -- ^ Linha no seu estado seguinte
animaLinha' l (Jogador (x, 0)) (Estrada n, os)
  -- Evitar função parcial (fora do mapa, o que não deve acontecer)
  | x < 0 || x >= l = animaLinha l (Estrada n, os)
  | n == 0    = (Estrada n, os)
  -- zip [0..] é usado para se saber o índice da linha em predicado (quantas
  -- movimentações elementares)
  | otherwise = (Estrada n, snd $ last $ takeWhilePlusOne predicado $
      zip [0..] $ iterate (step n) os)
  where takeWhilePlusOne p = foldr (\ x r -> if p x then (x:r) else [x]) []
        step n o
          | n > 0 = snd (animaLinha l (Estrada    1, o))
          | n < 0 = snd (animaLinha l (Estrada (-1), o))
        predicado (i, ls) = (ls !! x) /= Carro && i < abs n
animaLinha' l _ (t, os) = animaLinha l (t, os)

{-|
  'animaMapa' move todas as linhas de um mapa __válido__ com recurso a
  'animaLinha'. Ver 'animaMapa'' para a versão utilizada na implementação
  prática.

  === Notas

  Complexidade: \( O(n \times m) \), \(n\) linhas de largura \(m\)

  === Exemplo

  >>> animaMapa (Mapa 4 [ (Estrada 1, [Carro, Carro, Nenhum, Nenhum]),
                          (Rio (-1), [Tronco, Nenhum, Tronco, Tronco]) ])
  Mapa 4 [ (Estrada 1, [Nenhum,Carro,Carro,Nenhum]),
           (Rio (-1), [Nenhum,Tronco,Tronco,Tronco] )]
-}
animaMapa :: Mapa -- ^ Atual mapa
          -> Mapa -- ^ Mapa para a jogada seguinte
animaMapa (Mapa l lns) = Mapa l $ map (animaLinha l) lns

{-|
  'animaMapa'' apresenta um comportamento semelhante a 'animaMapa', mas que
  para @Carro@s que tentem atropelar o jogador (ver 'animaLinha'').

  === Exemplo

  >>> animaMapa' (Mapa 3 [ (Rio (-1) , [Tronco, Tronco, Nenhum]),
                         (Relva    , [Nenhum, Nenhum, Arvore]),
                         (Estrada 2, [Carro, Nenhum, Nenhum])])
                 (Jogador (1, 2))
  Mapa 3 [ (Rio (-1) , [Tronco, Nenhum, Tronco]),
           (Relva    , [Nenhum, Nenhum, Arvore]),
	   (Estrada 2, [Nenhum, Carro, Nenhum])]
-}
animaMapa' :: Mapa    -- ^ Atual mapa
           -> Jogador -- ^ Jogador (para situações de atropelamento)
           -> Mapa    -- ^ Mapa para a jogada seguinte
animaMapa' (Mapa l [])       _                   = Mapa l []
animaMapa' (Mapa l (ln:lns)) jgd@(Jogador (x,y)) =
  Mapa l (animaLinha' l jgd ln : n)
    where (Mapa _ n) = animaMapa' (Mapa l lns) (Jogador (x, y - 1))

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
deslocamento :: Jogada      -- ^ Movimento desejado do jogador
             -> Coordenadas -- ^ Vetor deslocamento
deslocamento Parado          = (0,    0)
deslocamento (Move Cima)     = (0, (-1))
deslocamento (Move Baixo)    = (0,    1)
deslocamento (Move Esquerda) = ((-1), 0)
deslocamento (Move Direita)  = (1,    0)

{-|
  'linhasJogador' devolve duas linhas do mapa, caso existam. Essas são, por
  ordem, a linha onde o jogador se encontra e a linha para onde se deseja
  mover, de acordo com a jogada.

  === Notas

  Complexidade: \( O(n) \)

  === Exemplos

  @
  m = Mapa 30 [ l0, l1, l2, l3 ] -- suponha-se que estas linhas estão definidas
  @

  >>> linhasJogador (Jogador (0, 0)) Parado m
  (Just l0, Just l1)

  >>> linhasJogador (Jogador (12, 3)) (Move Baixo) m
  (Just l3, Nothing)
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
               -> Maybe (Terreno, Obstaculo)   -- ^ Obstáculo (caso exista)
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

  @
  m = Mapa 4 [ (Relva    , [Nenhum, Arvore, Arvore, Arvore]),
               (Rio 3    , [Tronco, Tronco, Nenhum, Tronco]),
               (Estrada 1, [Nenhum, Carro, Carro, Nenhum])]
  @

  >>> obstaculosJogador (Jogador (0, 2)) (Move Cima) m
  (Just (Estrada 1, Nenhum), Just (Rio 3, Tronco))

  >>> obstaculosJogador (Jogador (0, 2)) (Move Esquerda) m
  (Just (Estrada 1, Nenhum), Nothing)
-}
obstaculosJogador :: Jogador     -- ^ Jogador (pela sua posição)
                  -> Jogada      -- ^ Movimento (ou ausência dele) do jogador
                  -> Mapa        -- ^ Mapa onde se encontra o jogador
                  -> Obstaculos2 -- ^ Obstáculos atual e possível seguinte
obstaculosJogador jgd@(Jogador (xi, yi)) j m = (o1, o2)
  where (l1, l2) = linhasJogador jgd j m
        (xf, yf) = let (dx, dy) = deslocamento j in (xi + dx, yi + dy)
        o1       = obstaculoLinha xi l1
        o2       = obstaculoLinha xf l2

{-|
  'animaJogador' devolve a posição do jogador após o seu movimento numa jogada,
  tomando em conta fatores como para onde se deseja mover, o obstáculo onde se
  encontra, e o obstáculo para onde se deseja mover (ver critérios em
  'animaJogo').

  === Notas

  Complexidade: \( O(n \times m) \), num mapa de \(n\) linhas de largura \(m\)

  === Exemplos

  @
  m = Mapa 4 [ (Relva    , [Nenhum, Arvore, Arvore, Arvore]),
               (Rio     1, [Tronco, Tronco, Nenhum, Tronco]),
               (Rio  (-1), [Nenhum, Tronco, Nenhum, Nenhum]),
               (Estrada 1, [Nenhum, Carro, Carro, Nenhum])]
  @

  Colisão com árvores:

  >>> animaJogador (Jogador (0, 0)) (Move Direita) m
  Jogador (0, 0)

  O jogador é incapaz de sair do mapa __pelos seus movimentos__ (apesar de o
  conseguir fazer se estiver num tronco, por exemplo):

  >>> animaJogador (Jogador (0, 0)) (Move Esquerda) m
  Jogador (0, 0)

  >>> animaJogador (Jogador (3, 1)) Parado m
  Jogador (4, 1)

  Movimentos laterais em rios (a corrente afeta o movimento do jogador):

  >>> animaJogador (Jogador (0, 1)) Parado m
  Jogador (1, 1)

  >>> animaJogador (Jogador (1, 1)) (Move Esquerda) m
  Jogador (1, 1)

  Movimentos verticais envolvendo rios (o rio não afeta o movimento lateral):

  >>> animaJogador (Jogador (1, 1)) (Move Baixo) m
  Jogador (1, 2)

  Neste exemplo, o jogador tenta mover-se para onde há uma árvore, não
  consegue, e é movido pela corrente do rio para a direita.

  >>> animaJogador (Jogador (1, 1)) (Move Cima) m
  Jogador (2, 1)
-}
animaJogador :: Jogador -- ^ Jogador
             -> Jogada  -- ^ Intenção de movimento do jogador
             -> Mapa    -- ^ Mapa, para verificação de obstáculos e correntes de rios
             -> Jogador -- ^ Jogador com posição atualizada
animaJogador jgd@(Jogador (x,y)) j m = Jogador (x + dx, y + dy)
  where (to1, to2) = obstaculosJogador jgd j m
        (dx1, dy1) = case to2 of
                       Nothing          -> (0, 0)
                       Just (_, Arvore) -> (0, 0)
                       _                -> deslocamento j
        dx2        = case to1 of
                       Just (t, Tronco) -> velocidadeTerreno t
                       _                -> 0
        -- Caso o jogador se mova para cima ou para baixo, não é movido pelo rio
        (dx, dy)   = if dy1 == 0 then (dx1 + dx2, 0) else (0, dy1)

{-|
  'animaJogo' parte de um 'Jogo', e atualiza-o após uma 'Jogada'. Tanto o mapa
  é modificado (carros e troncos movem-se) como o jogador pode mudar de
  posição (por vontade própria ou por movimento do tronco em que possa estar):
  ver 'animaMapa' e 'animaJogador'.

  === Critérios

  1. Numa @Estrada@ ou @Rio@ com velocidade \(v\), os obstáculos devem mover-se
  \(|v|\) unidades na direcção determinada.
  2. As jogadas @Move Cima@, @Move Baixo@, etc. fazem com que o jogador se mova
  1 unidade para cima, baixo, etc., respetivamente.
  3. Mesmo quando o jogador não efetua qualquer movimento (i.e. a sua jogada é
  @Parado@), se o personagem se encontrar em cima de um @Tronco@, o jogador
  acompanha o movimento do @Tronco@.
  4. O jogador não consegue escapar do mapa através dos seus movimentos. Por
  exemplo, se o jogador se encontrar na linha de topo do mapa, então mover-se
  para cima não tem qualquer efeito, uma vez que já se encontra no limite do
  mapa.
  5. Ao deslocar os obstáculos de uma linha, assim que desaparecerem por um dos
  lados do mapa, devem reaparecer no lado oposto.
  6. O jogador não pode ficar sobreposto a uma árvore.
  7. No movimento entre rios e outros terrenos, não há qualquer movimento
  lateral do jogador.
  8. Na animação de uma estrada, a linha pode parar antes da sua animação total
  caso um carro atropele o jogador.
  9. O efeito de deslize do mapa não é para ser implementado nesta função. Por
  outras palavras, as dimensões do mapa não devem sofrer alterações após
  invocar esta função.

  === Notas

  Complexidade: \( O(n \times m) \), mapa de \(n\) linhas de largura \(m\).
  Para simplicidade do código, duas iterações do mapa são feitas.

  === Exemplos

  Movimento de um rio e o seu efeito sobre o jogador:

  >>> animaJogo (Jogo (Jogador (1, 0)) (Mapa 3 [ (Rio 1, [Nenhum, Tronco, Tronco]) ])) Parado
  (Jogo (Jogador (2, 0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Tronco])] ))

  O jogador é incapaz de sair do mapa pelos seus movimentos:

  >>> animaJogo (Jogo (Jogador (0, 0)) (Mapa 1 [ (Relva, [Nenhum])])) (Move Cima)
  (Jogo (Jogador (0, 0)) (Mapa 1 [(Relva, [Nenhum])]))

  Para outros exemplos, consulte como o mapa e o jogador são modificados em
  'animaMapa' e 'animaJogador', respetivamente.
-}
animaJogo :: Jogo   -- ^ Estado atual do jogo
          -> Jogada -- ^ Jogada efetuada pelo jogador
          -> Jogo   -- ^ Jogo atualizado
animaJogo (Jogo jgd m) j = (Jogo jgd2 (animaMapa' m jgd2))
  where jgd2 = animaJogador jgd j m

