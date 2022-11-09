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

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23. Verificação
da validade de um mapa de acordo com os critérios em:

[Especificações](https://gitlab.com/uminho-di/li1/2223/projetos/2022li1g012/-/blob/main/spec/Fase1.pdf)

-}
module Tarefa1_2022li1g012 (
  -- * Funções expostas
  linhaValida, mapaValido,
  -- * Funções e tipos de dados auxiliares
  -- ** Para a análise de linhas
  HistoricoLinha(HL), historicoLinhaInvalido, historicoLinhaValido,
  obstaculoValido, obstaculosConsecutivosValidos, historicoLinhaSeguinte,
  -- ** Para a análise de mapas
  HistoricoMapa(HM), historicoMapaInvalido, historicoMapaValido,
  terrenosConsecutivosValidos, sentidoRios, tipologiaTerreno,
  historicoMapaSeguinte,
  -- ** Gerais
  foldlWhile, sgn, contaConsecutivos
  ) where

import LI12223

{-|
  Função semelhante a 'Prelude.foldl', mas com uma condição de saída, para que
  não seja necessário processar a totalidade de uma lista caso o resultado do
  fold já for sabido após o processamento de apenas alguns elementos.

  === Exemplos

  O fold não chega ao fim:

  >>> foldlWhile (\ acc x -> acc < 24) (*) 1 (\ _ _ -> (-5)) [1..5]
  -5


  __O fold chega ao fim!__:

  >>> foldlWhile (\acc x -> acc < 24) (*) 1 (-5) [1..4]
  24

  @4!@ é calculado mas, como 4 é o último elemento da lista, não se dá
  verificação do predicado!


  O fold chega ao fim:

  >>> foldlWhile (\acc x -> acc < 200) (*) 1 (-5) [1..5]
  120

-}
foldlWhile :: (b -> a -> Bool) -- ^ Predicado que dita se o @fold@ para ou não
           -> (b -> a -> b) -- ^ Função geradora do acumulador seguinte
           -> b -- ^ Valor inicial do acumulador
           -> (b -> a -> b) -- ^ Função que dá o que será devolvido quando o predicado é falso
           -> [a] -- ^ Lista a ser analisada
           -> b -- ^ Valor devolvido (último acumulador)
foldlWhile _ _ acc _ [] = acc
foldlWhile p f acc d (x:xs)
  | p acc x   = foldlWhile p f (f acc x) d xs
  | otherwise = d acc x


{-|
  'sgn' @x@ devolve o sinal de x.

  === Exemplos

  >>> sgn 12
  1

  >>> sgn 0
  0

  >>> sgn (-3)
  -1
-}
sgn :: Real a => a -> a
sgn x
  | x > 0  = 1
  | x == 0 = 0
  | x < 0  = -1

{-|
  'contaConsecutivos' @x@ conta o número de ocorrências consecutivas de @x@ no
  início de uma lista.

  === Notas

  Apesar desta função poder ser útil para implementaões de 'linhaValida' e
  'mapaValido', não é usada várias vezes, à medida que se analisa a linha, para
  um menor número de iterações da lista. No entanto, é usada no final de cada
  linha, para contar o número de elementos no início da linha, para analisar se
  esta é (ou não) válida num contexto de mapa circular (ver 'linhaValida').

  === Exemplo

  >>> contaConsecutivos 3 [3, 3, 3, 3, 4, 12, 5, 3, 1, 3]
  4
-}
contaConsecutivos :: Eq a => a   -- ^ Elemento a ser contado
                          -> [a] -- ^ Lista onde procurar o elemento
                          -> Int -- ^ Número de instâncias no início da linha
contaConsecutivos o = length . fst . span (== o)



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

-- | Exemplo de histório de linha inválido, devolvido quando algo é descoberto
-- que invalide uma linha, durante a sua análise.
historicoLinhaInvalido = HL False 0 False Nenhum 0

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
  'obstaculosConsecutivosValidos' @o n@ afirma se é ou não possível a
  existência de @n@ elementos consecutivos do tipo @o@.

  Função utilizada para cumprir os critérios:

  * Troncos têm, no máximo, 5 unidades de comprimento.
  * Carros têm, no máximo, 3 unidades de comprimento.

  === Exemplos

  >>> obstaculosConsecutivosValidos Tronco 6
  False

  >>> obstaculosConsecutivosValidos Carro 3
  True
-}
obstaculosConsecutivosValidos :: Obstaculo -- ^ Obstáculo em análise
                              -> Int -- ^ Número de obstáculos do mesmo tipo consecutivos
                              -> Bool
obstaculosConsecutivosValidos Tronco n = n <= 5
obstaculosConsecutivosValidos Carro  n = n <= 3
obstaculosConsecutivosValidos _      _ = True

{-|
  'historicoLinhaValido' verifica se o estado de análise de uma linha ainda é
  válido. Em caso negativo, é desnecessário desperdiçar poder de processamento
  na validação do resto da linha (ver 'foldlWhile').

  === Notas

  Esta função não deve ser utilizada para verificar se o __último__ estado de
  uma linha é ou não válido, mas apenas se se pode continuar a sua análise. Por
  exemplo, caso nenhum obstáculo @Nenhum@ tivesse sido encontrado, esta função
  permitiria a continuação da análise da linha (para tentar encontrar
  @Nenhum@s). No entanto, no final da linha, pelo menos um @Nenhum@ teria de
  ter sido encontrado.

  >>> historicoLinhaValido 20 (HL True 20 False Arvore 20)
  True

  === Exemplo

  >>> historicoLinhaValido 20 (HL True 21 False Nenhum 3)
  False

  Neste exemplo, já se contaram 21 elementos na linha, apesar do limite ser 20.
  A linha é automaticamente invalidada, não se continuando a análise até ao
  fim.
-}
historicoLinhaValido :: Largura -- ^ Largura desejada da linha (a do mapa)
                     -> HistoricoLinha -- ^ Estado a ser validado
                     -> Bool
historicoLinhaValido l (HL b lc _ _ _) = b && lc <= l

{-|
  'historicoLinhaSeguinte' analisa um elemento no atual contexto da linha,
  devolvendo o contexto após se processar este elemento. É responsável por
  contar o número total de elementos na linha, o número elementos consecutivos,
  etc.

  === Exemplos

  Contagem do número total de elementos e de elementos consecutivos:

  >>> historicoLinhaSeguinte Relva 30 (HL True 12 True Nenhum 3) Nenhum
  HL True 13 True Nenhum 4

  Contagem do número total de elementos e fim de elementos consecutivos:

  >>> historicoLinhaSeguinte Relva 30 (HL True 12 True Nenhum 3) Arvore
  HL True 13 True Arvore 1

  Elementos consecutivos em excesso:

  >>> historicoLinhaSeguinte (Rio 1) 10 (HL True 7 True Tronco 5) Tronco
  HL False 0 False Nenhum 0 -- um estado inválido

  Elemento inválido:

  >>> historicoLinhaSeguinte (Rio 1) 10 (HL True 4 True Tronco 2) Carro
  HL False 0 False Nenhum 0 -- um estado inválido

  Quando um obstáculo @Nenhum@ é encontrado:

  >>> historicoLinhaSeguinte Relva 30 (HL True 5 False Arvore 2) Nenhum
  HL True 6 True Nenhum 1

  Esta função não verfica se o histórico fornecido é inválido (recorrendo a
  'historicoLinhaValido'), observando-se comportamento não documentado nesses
  casos.
-}
historicoLinhaSeguinte :: Terreno -- ^ O tipo de terreno da linha
                       -> Largura -- ^ Largura desejada da linha (a do mapa)
                       -> HistoricoLinha -- ^ O atual estado da análise
                       -> Obstaculo -- ^ Obstáculo a ser analisado
                       -> HistoricoLinha
historicoLinhaSeguinte ter l hl@(HL b lc n u c) o
  | not (obstaculoValido ter o) = historicoLinhaInvalido
  | o == u && not (obstaculosConsecutivosValidos u (c + 1)) =
      historicoLinhaInvalido
  | o == u    = HL b (lc + 1) (n || o == Nenhum) o (c + 1)
  | otherwise = HL b (lc + 1) (n || o == Nenhum) o 1

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

  === Notas

  Complexidade: \( O(n) \)

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

  Exemplos de linhas válidas:

  >>> linhaValida 6 (Estrada 1, [Nenhum, Carro, Carro, Nenhum, Carro, Nenhum])
  True

  >>> linhaValida 5 (Relva, [Nenhum, Arvore, Arvore, Nenhum, Arvore])
  True
-}
linhaValida :: Largura -- ^ Largura desejada da linha (a do mapa)
            -> (Terreno, [Obstaculo]) -- ^ Linha a ser validada
            -> Bool -- ^ Se a linha é ou não válida
linhaValida lg (ter, ln)
  | b && lg == lc && n = -- Verificar circularidade do mapa
      obstaculosConsecutivosValidos u (c + contaConsecutivos u ln)
  | otherwise = False -- Algo inválido na linha
  where (HL b lc n u c) = foldlWhile (\ hl _ -> historicoLinhaValido lg hl)
                                     (historicoLinhaSeguinte ter lg)
                                     (HL True 0 False Nenhum 0)
                                     (\ _ _ -> historicoLinhaInvalido) ln

{-|
  Informação recolhida sobre um mapa até um dado momento / linha da sua
  análise. Contém os dados necessários para a avaliação dos critérios
  especificados em 'mapaValido'.
-}
data HistoricoMapa =
  {-|
    === Nota

    @(HM True (Rio 3) 2)@ não significa que os últimos 2 terrenos foram @Rio@s
    de velocidade 3, mas apenas que os últimos 2 terrenos foram @Rio@s e o
    último deles tinha velocidade 3.
  -}
  HM
    Bool -- ^ Se o mapa é válido até ao momento
    Terreno -- ^ O tipo de terreno anterior
    Int -- ^ Número de terrenos anteriores do mesmo tipo
    deriving Show -- ^ Para propósitos de /debugging/

-- | Exemplo de histórico de mapa inválido, devolvido quando algo é descoberto
-- que invalide um mapa, durante a sua análise.
historicoMapaInvalido = HM False Relva 0

{-|
  'terrenosConsecutivosValidos' @t n@ afirma se é ou não possível a existência
  de @n@ elementos consecutivos do tipo @t@.

  Função utilizada para cumprir o critério:

  * Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou
  relvas.

  === Exemplos

  Note-se que é impossível haver quatro rios seguidos com a mesma velocidade.
  'terrenosConsecutivosValidos' apenas verifica se podem existir quatro rios
  seguidos, desde que eles cumpram os critérios de velocidade exigidos.

  >>> terrenosConsecutivosValidos (Rio 1) 4
  True

  >>> terrenosConsecutivosValidos Relva 6
  False
-}
terrenosConsecutivosValidos :: Terreno -- ^ Tipo de terreno (independentemente da velocidade)
                            -> Int -- ^ Número de instâncias consecutivas
                            -> Bool -- ^ Possibilidade ou impossibilidade
terrenosConsecutivosValidos Relva       n = n <= 5
terrenosConsecutivosValidos (Estrada _) n = n <= 5
terrenosConsecutivosValidos (Rio _)     n = n <= 4

{-|
  'sentidoRios' verifica se dois terrenos podem estar lado a lado, servindo
  para a verificação do seguinte critério:

  * Rios contíguos têm direcções opostas.

  === Exemplos

  >>> sentidoRios (Rio 1) (Rio 3)
  False

  >>> sentidoRios (Rio 2) (Rio (-4))
  True

  Terrenos que não @Rio@s não são afetados:

  >>> sentidoRios Relva (Rio 0)
  True
-}
sentidoRios :: Terreno -- ^ Rio anterior (na análise do mapa)
            -> Terreno -- ^ Rio atual (na análise do mapa)
            -> Bool
sentidoRios (Rio n1) (Rio n2) = sgn n1 == (-1) * sgn n2
sentidoRios _        _        = True

{-|
  Compara se dois terrenos são do mesmo tipo, independentemente de fatores como
  velocidade de rios ou estradas (i.e., os dois objetos partilham o mesmo
  construtor).

  === Exemplos

  >>> tipologiaTerreno (Rio 0) (Rio 4)
  True

  >>> tipologiaTerreno (Estrada 4) Relva
  False
-}
tipologiaTerreno :: Terreno -> Terreno -> Bool
tipologiaTerreno (Rio _)     (Rio _)     = True
tipologiaTerreno (Estrada _) (Estrada _) = True
tipologiaTerreno Relva       Relva       = True
tipologiaTerreno _           _           = False

{-|
  'historicoMapaValido' verifica se o estado de análise de um mapa ainda é
  válido. Em caso negativo, é desnecessário desperdiçar poder de processamento
  na validação do resto do mapa.

  === Exemplo

  >>> historicoMapaValido (HM False Relva 0)
  False
-}
historicoMapaValido :: HistoricoMapa -- ^ O histórico atual
                    -> Bool -- ^ Se é ou não válido
historicoMapaValido (HM b _ _) = b

{-|
  'historicoMapaSeguinte' analisa uma linha no atual contexto de análise de um
  mapa, devolvendo o contexto após se processar a linha. É responsável por
  garantir que todas as linhas têm a mesma largura, não há rios consecutivos
  com o mesmo sentido, e que não há demasiadas linhas com o mesmo tipo de
  terreno em sequência.

  === Exemplos

  Contagem de elementos sucessivos:

  >>> historicoMapaSeguinte 3 (HM True Relva 2) (Relva, [Nenhum, Arvore, Nenhum])
  HM True Relva 3

  >>> historicoMapaSeguinte 2 (HM True (Rio 2) 1) (Rio (-1), [Nenhum, Tronco])
  HM True (Rio -1) 2

  Note-se que, apesar dos rios consecutivos terem velocidades distintas, a
  contagem de rios consecutivos aumenta. Ver 'HM'.


  >>> historicoMapaSeguinte 3 (HM True Relva 5) (Relva, [Nenhum, Nenhum, Nenhum])
  HM False Relva 0

  Histórico (mapa) inválido, devido a demasiadas relvas sucessivas


  Sentido dos rios:

  >>> historicoMapaSeguinte 2 (HM True (Rio 2) 2) (Rio (-1), [Nenhum, Tronco])
  HM True (Rio (-1)) 3

  >>> historicoMapaSeguinte 2 (HM True (Rio 2) 2) (Rio 1, [Nenhum, Tronco])
  HM False Relva 0

  Linha inválida:

  >>> historicoMapaSeguinte 2 (HM True (Estrada 1) 3) (Estrada 2, [Carro, Carro])
  False
-}
historicoMapaSeguinte :: Largura -- ^ Largura desejada do mapa
                      -> HistoricoMapa -- ^  O atual estado da análise
                      -> (Terreno, [Obstaculo]) -- ^ Linha a ser analisada
                      -> HistoricoMapa
historicoMapaSeguinte l hm@(HM b tu c) ln@(t, _)
  | not $ linhaValida l ln = historicoMapaInvalido -- linha inválida

  | not (sentidoRios tu t) = historicoMapaInvalido -- sentido dos rios

  | tipologiaTerreno t tu = if terrenosConsecutivosValidos t (c+1) then
      HM b t (c+1) else historicoMapaInvalido

  | otherwise = HM b t 1 -- novo tipo de terreno

{-|
  'mapaValido' verifica se um mapa é válido, de acordo com os seguintes
  critérios:

  * Todas as linhas são válidas e têm o mesmo comprimento (consultar
  'linhaValida');

  * Rios contíguos têm sentidos opostos;

  * Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou
  relvas.

  === Notas

  Complexidade: \( O(n \times m) \), \(n\) linhas de largura \(m\)

  === Exemplos

  Linha inválida:

  >>> mapaValido (Mapa 5 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco, Tronco]),
                          (Relva, [Arvore, Arvore, Arvore, Arvore, Arvore])])
  False

  Linha de largura incorreta:

  >>> mapaValido (Mapa 3 [(Estrada 1, [Nenhum, Carro, Carro, Carro])])
  False

  Sentidos dos rios:

  >>> mapaValido (Mapa 2 [(Rio 1, [Nenhum, Tronco]), (Rio (-2), [Nenhum, Nenhum])])
  True

  >>> mapaValido (Mapa 2 [(Rio 1, [Nenhum, Tronco]), ((Rio 2), [Nenhum, Nenhum])])
  False

  Elementos sucessivos em excesso:

  >>> mapaValido (Mapa 1 [(Relva, [Nenhum]),
                          (Relva, [Nenhum]),
                          (Relva, [Nenhum]),
                          (Relva, [Nenhum]),
                          (Relva, [Nenhum]),
                          (Relva, [Nenhum])])
  False
-}
mapaValido :: Mapa -- ^ Mapa a ser validado
           -> Bool -- ^ Se o mapa é ou não válido
mapaValido (Mapa l lns) = historicoMapaValido hm
  where hm = foldlWhile (\ acc x -> historicoMapaValido acc)
                       (historicoMapaSeguinte l)
                       (HM True Relva 0)
                       (\ _ _ -> historicoMapaInvalido)
                       lns
