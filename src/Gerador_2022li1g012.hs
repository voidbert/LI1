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

{-|
Module      : Gerador_2022li1g012
Description : Gerador de mapas mais versátil que a tarefa 2
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

  À 'Tarefa2_2022li1g012' falta variabilidade de parâmetros, e também gera
  mapas válidos de acordo com critérios que não são os nossos desejados, daí
  um segundo gerador de mapas.
-}
module Gerador_2022li1g012 (
  -- * Dificuldade do jogo
  Dificuldade(..), facil,
  -- * Geração de mapas
  -- ** Geração de terrenos
  gerarTerreno, terrenosExceto, contarTerrenos, gerarVelocidade,
  -- ** Geração de linhas
  gerarLinha, intervaloObstaculos,
  -- ** Continuação de mapas
  estendeMapa', deslizaJogo'
  ) where

import Data.List
import System.Random
import System.Random.Shuffle

import LI12223
import Tarefa5_2022li1g012

-- A 'Dificuldade' do jogo define os parâmetros de geração do mapa.
data Dificuldade = Dif
  (Terreno -> Int)            -- ^ Terrenos consecutivos máximos permitidos
  (Terreno -> Int)            -- ^ Velocidade máxima positiva permitida
  (Terreno -> (Float, Float)) -- ^ Intervalo (0 a 1) de quantidade de obstáculos

facil :: Dificuldade
facil = Dif csc vt ro
  where csc _ = 3
        vt  _ = 2
        ro Relva = (0.2, 0.5)
        ro _ = (0.4, 0.6)

{-|
  'gerarVelocidade' gera a velocidade de um terreno de acordo com as restrições
  da 'Dificuldade' e é uma função auxiliar de 'gerarTerreno'. O requisito do
  sinal da velocidade, necessário para o movimento alternado dos rios, será o
  sinal da velocidade devolvida. Caso o inteiro do requisito seja nulo, a
  velocidade poderá ser positiva ou negativa.
-}
gerarVelocidade :: RandomGen gen
                => gen           -- ^ Seed para números aleatórios
                -> Dificuldade   -- ^ Dificuldade do jogo
                -> Terreno       -- ^ Terreno com a velocidade
                -> Int           -- ^ Requisito do sinal da velocidade (0 - nenhum)
                -> Velocidade    -- ^ Velocidade gerada
gerarVelocidade sd (Dif _ vt _) ter r
  | r > 0 = absv
  | r == 0 = sgn * absv
  | r < 0 = -absv
  where (absv, sd') = randomR (1, vt ter) sd
        sgn = [-1, 1] !! (fst $ randomR (0, 1) sd') -- velocidade não nula

{-|
  'contarTerrenos' é uma função auxiliar de 'gerarTerreno' que conta os
  terrenos do mesmo tipo no início (topo) do mapa, devolvendo o 1.º terreno
  (com a sua velocidade) e o número de terrenos desse tipo (que podem ter
  diferentes velocidades do inicial).

  === Exemplos

  >>> contarTerrenos (Mapa 1 [(Relva, [Nenhum]), (Relva, [Arvore]), (Rio 1, [Nenhum])])
  (Relva, 2)

  >>> contarTerrenos (Mapa 1 [(Rio 1, [Nenhum]), (Rio 2, [Tronco]), (Estrada 0, [Nenhum])])
  (Rio 1, 2)
-}
contarTerrenos :: Mapa           -- ^ Mapa em análise
               -> (Terreno, Int) -- ^ 1.º terreno e número de terrenos do mesmo tipo
contarTerrenos (Mapa _ lns) = (fst $ head $ lns, length $ head $ groupBy cmp lns)
  where cmp (Relva, _)       (Relva, _)       = True
        cmp ((Rio _), _)     ((Rio _), _)     = True
        cmp ((Estrada _), _) ((Estrada _), _) = True
        cmp _                _                = False

{-|
  'terrenosExceto' é uma função auxiliar de 'gerarTerreno' que gera uma lista
  de tipos de terreno à exceção do que foi providenciado. As velocidades
  devolvidas serão sempre nulas: o relevante é o __tipo__ de terreno.

  === Exemplos

  >>> terrenosExceto (Rio 2)
  [Relva, Estrada 0]

  >>> terrenosExceto Relva
  [Rio 0, Estrada 0]
-}
terrenosExceto :: Terreno -> [Terreno]
terrenosExceto Relva       = [Rio 0, Estrada 0]
terrenosExceto (Rio _)     = [Relva, Estrada 0]
terrenosExceto (Estrada _) = [Relva, Rio 0]

{-|
  'gerarTerreno' devolve o terreno (tipo e velocidade) que deve usado na
  geração de uma linha para continuar o mapa, obedecendo às restrições da
  'Dificuldade'.
-}
gerarTerreno :: RandomGen gen
             => gen           -- ^ Seed para números aleatórios
             -> Dificuldade   -- ^ Dificuldade do jogo
             -> Mapa          -- ^ Mapa ao qual aplicar as restrições
             -> Terreno       -- ^ Terreno escolhido
-- Mapa vazio. Não há restrições. Escolher um terreno e, se aplicável, uma
-- velocidade.
gerarTerreno sd d (Mapa _ []) =
  let (n, sd') = randomR (0, 2) sd
      tt = [Relva, Rio 0, Estrada 0] !! n
  in case tt of Relva     -> Relva
                Rio     _ -> Rio $ gerarVelocidade sd' d tt 0
                Estrada _ -> Estrada $ gerarVelocidade sd' d tt 0
-- Mapa já tem linhas. Determinar se o terreno seguinte é igual ao anterior. Se
-- sim, gerar velocidade caso aplicável. Senão, escolher outro terreno e outra
-- velocidade.
gerarTerreno sd d@(Dif csc _ _) m =
  let (t, c) = contarTerrenos m
      (r, sd') = randomR (1, csc t) sd
      igs = r <= csc t - c -- se o terreno seguinte é (ou não) do mesmo tipo
  in if igs then case t of Relva -> Relva
                           Rio n -> Rio $ gerarVelocidade sd' d t (- signum n)
                           Estrada n -> Estrada $ gerarVelocidade sd' d t 0
     else let ts = (terrenosExceto t)
              (r', sd'') = randomR (0, 1) sd'
              t' = ts !! r'
          in case t' of Relva     -> Relva
                        Rio _     -> Rio $ gerarVelocidade sd'' d t' 0
                        Estrada _ -> Estrada $ gerarVelocidade sd'' d t' 0

{-|
  'intervaloObstaculos' é uma função auxiliar de 'gerarLinha' que transforma um
  intervalo entre 0 a 1 num intervalo do número de obstáculos mínimo e máximo.
  É claro que a largura da linha é necessária. Os extremos resultantes são
  arredondados ao inteiro mais próximo.

  === Exemplo

  >>> intervaloObstaculos 20 (0.2, 0.6)
  (4, 12)
-}
intervaloObstaculos :: Largura -> (Float, Float) -> (Int, Int)
intervaloObstaculos l (a, b) = let l' = fromIntegral l :: Float in
  (round (l' * a), round (l' * b))

{-|
  'gerarLinha' gera os obstáculos de uma linha, dado o tipo de terreno e
  seguindo as restrições da 'Dificuldade'.
-}
gerarLinha :: RandomGen gen
           => gen                    -- ^ Seed para números aleatórios
           -> Dificuldade            -- ^ Dificuldade do jogo
           -> Terreno                -- ^ Tipo de terreno da linha
           -> Largura                -- ^ Largura da linha
           -> (Terreno, [Obstaculo]) -- ^ Linha produzida
gerarLinha sd (Dif _ vs ro) ter l =
  let (mi, ma) = intervaloObstaculos l $ ro ter
      (no, sd') = randomR (mi, ma) sd
      obs = case ter of Relva     -> Arvore
                        Rio _     -> Tronco
                        Estrada _ -> Carro
      unsh = replicate (l - no) Nenhum ++ replicate no obs
  in (ter, shuffle' unsh l sd')

{-|
  'estendeMapa'' funciona como 'Tarefa2_2022li1g012.estendeMapa' mas segue os
  critérios de uma 'Dificuldade'.
-}
estendeMapa' :: RandomGen gen
             => gen           -- ^ Seed para números aleatórios
             -> Dificuldade   -- ^ Dificuldade do jogo
             -> Mapa          -- ^ Mapa a ser estendido
             -> Mapa          -- ^ Mapa estendido
estendeMapa' sd d m@(Mapa l lns) = Mapa l
  ((gerarLinha sd d (gerarTerreno sd d m) l) : lns)

{-|
  'deslizaJogo'' comporta-se como 'Tarefa5_2022li1g012.deslizaJogo' mas segue
  os critérios de uma 'Dificuldade' para a geração de novas linhas do mapa.
-}
deslizaJogo' :: RandomGen gen
             => gen
             -> Dificuldade
             -> Jogo
             -> Jogo
deslizaJogo' sd d (Jogo j m@(Mapa l lns)) =
  let (Mapa _ lns') = estendeMapa' sd d m
  in Jogo (deslizaJogador j) (Mapa l $ init lns')
