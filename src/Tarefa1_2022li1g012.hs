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
Module      : Tarefa1_2022li1g012
Description : Validação de um mapa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g012 where
import LI12223


{-|
 Informação recolhida sobre uma linha até a um dado momento / elemento da sua
 análise. Contém os dados necessários para a avaliação dos critérios
 especificados em 'linhaValida'.
-}
data HistoricoLinha =
  HL
    Bool       -- ^ Se a linha é válida até ao momento
    Largura    -- ^ Número de elementos contados na linha
    Bool       -- ^ Existência de pelo menos um obstáculo Nenhum
    Obstaculo  -- ^ O obstáculo anterior
    Int        -- ^ O número de obstáculos anteriores do mesmo tipo
    deriving Show -- ^ Para propósitos de /debugging/

historicoInvalido = HL False  0 False Nenhum 0

{-|
  'obstaculoValido' @t o@ avalia se o obstáculo @o@ é adequado para o tipo de
  terreno @t@.

  Função utilizada para cumprir os critérios:

  * Não existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou
    relvas, árvores em rios ou estradas, etc.

  === Exemplos

  >>> obstaculoValido Rio Tronco
  True

  >>> obstaculoValido Rio Carro
  False
-}
obstaculoValido :: Terreno   -- ^ Tipo de terreno da linha
                -> Obstaculo -- ^ Obstáculo em análise
                -> Bool
obstaculoValido Relva       o = o == Nenhum || o == Arvore
obstaculoValido (Rio     _) o = o == Nenhum || o == Tronco
obstaculoValido (Estrada _) o = o == Nenhum || o == Carro

{-|
  'consecutivosValidos' @o n@ afirma se é ou não possível a existência de @n@
  elementos consecutivos do tipo @o@.

  Função utilizada para cumprir os critérios:

  * Troncos têm, no máximo, 5 unidades de comprimento.
  * Carros têm, no máximo, 3 unidades de comprimento.

  === Exemplos

  >>> consecutivosValidos Tronco 6
  False

  >>> consecutivosValidos Carro 3
  True
-}
consecutivosValidos :: Obstaculo -- ^ Obstáculo em análise
                    -> Int -- ^ Número de obstáculos do mesmo tipo consecutivos
                    -> Bool
consecutivosValidos Tronco n = n <= 5
consecutivosValidos Carro  n = n <= 3
consecutivosValidos _      _ = True

{-|
  'historicoLinhaValido' verifica se o estado de análise de uma linha ainda é
  válido. Em caso negativo, é desnecessário desperdiçar poder de processamento
  na validação do resto da linha.

  === Exemplo

  >>> historicoLinhaValido 20 (HL True 21 False Nenhum 3)
  False

  Neste exemplo, já se contaram 21 elementos na linha, apesar do limite ser 20.
  A linha é automática invalidada, não se continuando a análise até ao fim.
-}
historicoLinhaValido :: Largura -- ^ Largura desejada da linha (a do mapa)
                     -> HistoricoLinha -- ^ Estado a ser validado
                     -> Bool
historicoLinhaValido l (HL b lc _ _ _) = b && lc <= l

{-|
  'historicoLinhaSeguinte' analisa um elemento no atual contexto da linha,
  devolvendo o contexto após se processar este elemento. É responsável por
  contar o número total de elementos na linha, o número consecutivo de um dado
  elemento

  === Exemplos

  Contagem do número total de elementos e de elementos consecutivos:

  >>> historicoLinhaSeguinte Relva Nenhum 30 (HL True 12 True Nenhum 3)
  HL True 13 True Nenhum 4

  Contagem do número total de elementos e fim de elementos consecutivos:

  >>> historicoLinhaSeguinte Relva Arvore 30 (HL True 12 True Nenhum 3)
  HL True 13 True Arvore 1

  Elementos consecutivos em excesso:

  >>> historicoLinhaSeguinte (Rio 1) Tronco 10 (HL True 7 True Tronco 5)
  HL False 0 False Nenhum 0 -- um estado inválido

  Elemento inválido:

  >>> historicoLinhaSeguinte (Rio 1) Carro 10 (HL True 4 True Tronco 2)
  HL False 0 False Nenhum 0 -- um estado inválido

  Quando um obstáculo @Nenhum@ é encontrado:

  >>> historicoLinhaSeguinte Relva Nenhum 30 (HL True 5 False Arvore 2)
  HL True 6 True Nenhum 1

  Esta função não verfica se o histórico fornecido é inválido (recorrendo a
  'historicoLinhaValido'), observando-se comportamento não documentado nesses
  casos.
-}
historicoLinhaSeguinte :: Terreno -- ^ O tipo de terreno da linha
                       -> Obstaculo -- ^ Obstáculo a ser analisado
                       -> Largura -- ^ Largura desejada da linha (a do mapa)
                       -> HistoricoLinha -- ^ O atual estado da análise
                       -> HistoricoLinha
historicoLinhaSeguinte ter o l hl@(HL b lc n u c)
  | o == u = if obstaculoValido ter o && consecutivosValidos u (c + 1) then
      HL b (lc + 1) (n || o == Nenhum) o (c + 1) else historicoInvalido

  | obstaculoValido ter o = HL b (lc + 1) (n || o == Nenhum) o 1

  | otherwise = historicoInvalido



{-
  TODO - é possível definir esta função com outras funções (foldl, ou takeWhile
  juntamente com scanl). Analisar essa possibilidade.
-}
linhaValidaAcc :: (Terreno, [Obstaculo]) -- ^ Linha a ser analisada
               -> Largura                -- ^ Largura desejada do mapa
               -> HistoricoLinha         -- ^ Acumulador
               -> HistoricoLinha
linhaValidaAcc (_, []) _ acc = acc
linhaValidaAcc (ter, (h:t)) l acc
  | historicoLinhaValido l acc =
      linhaValidaAcc (ter, t) l (historicoLinhaSeguinte ter h l acc)
  | otherwise = historicoInvalido

{-|
  'linhaValida' verifica se uma linha do mapa é válida, i.e., cumpre os
  seguintes critérios de verificação do mapa:

  * Não existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou
  relvas, árvores em rios ou estradas, etc.

  * Troncos têm, no máximo, 5 unidades de comprimento.

  * Carros têm, no máximo, 3 unidades de comprimento.

  * Em qualquer linha existe, no mínimo, um “obstáculo” @Nenhum@. Ou seja, uma
  linha não pode ser composta exclusivamente por obstáculos, precisando de
  haver pelo menos um espaço livre.

  * O comprimento da lista de obstáculos de cada linha corresponde exatamente à
  largura do mapa.

  === Exemplos

  Largura incorreta da linha:

  >>> linhaValida 4 (Relva, [Nenhum, Nenhum, Nenhum])
  False

  Linha sem obstáculos @Nenhum@:

  >>> linhaValida 3 (Rio (-2), [Tronco, Tronco, Tronco])
  False

  Carros / troncos demasiados longos:

  >>> linhaValida 5 (Estrada 1, [Nenhum, Carro, Carro, Carro, Carro])
  False

  Acidente automóvel!:

  >>> linhaValida 3 (Rio 2, [Nenhum, Carro, Nenhum])
  False

  Exemplos de linha válidas:

  >>> linhaValida 6 (Estrada 1, [Nenhum, Carro, Carro, Nenhum, Carro, Nenhum])
  True

  >>> linhaValida 5 (Relva, [Nenhum, Arvore, Arvore, Nenhum, Arvore])
  True
-}
linhaValida :: Largura -- ^ Largura desejada da linha (a do mapa)
            -> (Terreno, [Obstaculo]) -- ^ Linha a ser validada
            -> Bool
linhaValida lg ln = b && lg == lc && n
  where (HL b lc n _ _) = linhaValidaAcc ln lg (HL True 0 False Nenhum 0)



mapaValido :: Mapa -> Bool
mapaValido = undefined
