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
Module      : Tarefa2_2022li1g012
Description : Geração contínua de um mapa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g012 where

import LI12223

{- |
  'estendeMapa' acrescenta uma linha pseudoaleatória a um mapa dado. Este deve
  ser válido, já que esta função só valida e acrescenta uma possivel linha
  seguinte. O inteiro @i@ serve como uma forma de tornar o póximo terreno 
  pseudoaleatório. 
  Critérios para a formação de uma nova linha :

  * Troncos têm, no máximo, 5 unidades de comprimento.

  * Carro têm, no máximo, 3 unidades de comprimento.

  * O mapa é circular.

  * No máximo existem 4 terrenos @Rios _@ consecutivos.

  * No máximo existem 5 terrenos @Estrada _@ e Relva consecutivos.

  * A quantidade de obstáculos de uma linha têm de coincidir com a largura
  dessa mesma linha.

  * Rios contiguos têm direções opostas.

  * Os obstáculos devem ser apropriados ao terreno.

  === Exemplos 
 
  Quando o mapa fornecido é vazio :

  >>> estendeMapa (Mapa 2 []) 3
  Mapa 2 [(Rio 3,[Tronco,Nenhum])]

  Quando o mapa fornecido apresenta um Rio :

  >>> estendeMapa (Mapa 1 [(Rio 3, [Nenhum,Tronco])]) 3
  Mapa 1 [(Rio (-3),[Tronco,Nenhum]),(Rio 3,[Nenhum,Tronco])]

  Quando o mapa fornecido apresenta 4 terrenos @Rio_@ seguidos :

  >>> estendeMapa (Mapa 1 [(Rio 2, [Nenhum]),(Rio (-1), [Nenhum]),
                          (Rio 5, [Nenhum]),(Rio (-1), [Nenhum])]) 1
  Mapa 1 [(Relva,[Nenhum]),(Rio 2,[Nenhum]),(Rio (-1),[Nenhum]),
         (Rio 5,[Nenhum]),(Rio (-1),[Nenhum])]
-}

estendeMapa :: Mapa 
            -> Int -- ^ Inteiro de 0 a 100, fornece pseudoaleatóriedade na geração da linha  
            -> Mapa -- ^ Mapa com a nova linha adicionada
estendeMapa m@(Mapa l []) i
  = Mapa l [(adicionaTerreno m i, adicionaObstaculos l i (t,[]))]
  where t = adicionaTerreno m i
estendeMapa m@(Mapa l lt@((terr, _):trs)) i 
  = Mapa l ((adicionaTerreno m i, adicionaObstaculos l i (t,[])) : lt)
  where t = adicionaTerreno m i

{- |
  'adicionaTerreno' é a função auxiliar de 'estendeMapa' que escolhe um terreno
  de uma lista de terrenos válidos para adicionar ao topo do mapa e lhe 
  atribuí, se desejado, uma velocidade /= 0. 
  Critérios usados nesta função:

  * Rios contiguos têm direções opostas.
 
  === Exemplos 
  
  >>> adicionaTerreno (Mapa 2 [(Rio (-1), [Nenhum,Tronco])]) 3
  Rio 3
  
  Quando o mapa fornecido apresenta 5 terrenos @Estrada _@ seguidos :
  >>> adicionaTerreno (Mapa 2 [(Estrada 0, [Nenhum,Tronco]),(Estrada (-2), [Nenhum,Tronco]),
                               (Estrada (-1), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Tronco]),
                               (Estrada 1, [Nenhum,Tronco])]) 3
  Relva
-}

adicionaTerreno :: Mapa 
                -> Int {- ^ Inteiro de 0 a 100, fornece pseudoaleatóriedade na geração dos
                            terrenos.
                       -}  
                -> Terreno
adicionaTerreno m@(Mapa l ((Rio v, _):_)) i
  | v > 0 && verificarRio (ltp !! mod i r) vel = velocidadeTerreno (ltp !! mod i r) (-vel)
  | otherwise = velocidadeTerreno (ltp !! mod i r) vel
  where ltp = proximosTerrenosValidos m 
        r = length ltp 
        verificarRio t2 i = tipologiaTerreno t2 (Rio i)
        vel = mod (length ltp) i   
adicionaTerreno m i = velocidadeTerreno (ltp !! mod i r) vel
  where ltp = proximosTerrenosValidos m 
        r = length ltp
        vel = mod (length ltp) i   

{- |
  Fornece uma lista de terrenos possíveis segundo os seguintes críterios:
 
  * No máximo existem 4 terrenos @Rios _@ consecutivos.

  * No máximo existem 5 terrenos @Estrada _@ e Relva consecutivos.

  === Exemplos 

  >>> proximosTerrenosValidos (Mapa 3 [(Estrada 2, [])])
  [Rio 0,Estrada 0,Relva]

  >>> proximosTerrenosValidos (Mapa 3 [(Rio 0,[]),(Rio 0,[]),(Rio 1,[]),
                                       (Rio (-1),[])])
  [Estrada 0,Relva]
-}

proximosTerrenosValidos :: Mapa 
                        -> [Terreno]
proximosTerrenosValidos (Mapa l [])    = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Rio v , _):_))
  | contarTerrenos (Rio v) lns     < 4 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Estrada v , _):_))
  | contarTerrenos (Estrada v) lns < 5 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Rio 0, Relva]
proximosTerrenosValidos (Mapa l lns@((Relva, _):_))
  | contarTerrenos (Relva) lns     < 5 = [Rio 0, Estrada 0, Relva]
  | otherwise                          = [Rio 0, Estrada 0]

{- |
  'velocidadeTerreno' atribuí uma nova velocidade a um Terreno com uma 
  velocidade já existente. Serve para dar novos valores de velocidade
  a Terrenos provenientes da função 'proximosTerrenosValidos', no caso
  do Terreno @Relva@ este não atribuí nenhum valor já que é um terreno
  que não aceita valores de velocidade.

  === Exemplos

  >>> velocidadeTerreno (Rio 0) 8
  Rio 8

  >>> velocidadeTerreno Relva 2
  Relva
-}

velocidadeTerreno :: Terreno 
                  -> Int 
                  -> Terreno 
velocidadeTerreno (Rio _) i     = Rio i
velocidadeTerreno (Estrada _) i = Estrada i
velocidadeTerreno (Relva) i     = Relva

{- |
  'contaTerrenos' conta o número de terrenos consecutivos de um certo tipo
  num dado mapa.

  === Exemplo 

  >>>  contarTerrenos Relva [(Relva , []),(Relva , []),(Rio 2, []),(Relva, [])]
  2
-}

contarTerrenos :: Terreno 
               -> [(Terreno,[Obstaculo])] 
               -> Int
contarTerrenos t ts = contaConsecutivos (tipologiaTerreno t) $ map (\ (t,o) -> t) ts

{- |
  'tipologiaTerreno' ajuda na comparação de terrenos indepente da sua 
  velocidade. Se ambos os terrenos forem to mesmo tipo, indepente da sua
  velocidade, a função devolve True.

  === Exemplo

  >>> tipologiaTerreno (Rio 2) (Rio 3)
  True

  >>> tipologiaTerreno (Rio 2) (Estrada 4)
  False
-}

tipologiaTerreno :: Terreno 
                 -> Terreno
                 -> Bool
tipologiaTerreno (Rio _)     (Rio _)     = True
tipologiaTerreno (Estrada _) (Estrada _) = True
tipologiaTerreno Relva       Relva       = True
tipologiaTerreno _           _           = False

{- |
  'contConsecutivos' conta os primeiros elementos que validam uma certa função
  dentro de uma lista.

  === Exemplos 

  >>> contaConsecutivos (==2) [1,2,2]
  0

  >>> contaConsecutivos (==2) [2,2,1,2]
  2
-}

contaConsecutivos :: (a -> Bool) 
                  -> [a] 
                  -> Int
contaConsecutivos f xs = length $ fst $ span f xs

{- |
  'adicionaObstaculos' é a função auxiliar de 'estendeMapa', que escolhe um
  terreno de uma lista de possiveis obstáculos repetidamente, até que a
  lista de obstáculos final tenha nº de elementos igual ao valor da largura
  do mapa. Esta lista é depois adicionada ao topo do mapa na função principal
  'estendeMapa'.
  Critérios usados nesta função:

  * Os obstaculos devem pertencer ao terreno adequado.

  === Exemplos 
  
  >>> adicionaObstaculos 3 3 (Estrada 2, [Nenhum])
  [Nenhum,Carro,Carro]

  >>> adicionaObstaculos 3 3 (Estrada 2, [])
  [Nenhum,Carro,Carro]

  >>> adicionaObstaculos 3 0 (Rio 1, [])
  [Nenhum,Nenhum,Nenhum]
-}
  
adicionaObstaculos :: Int -- ^ largura do terreno
                   -> Int {- ^ Inteiro de 0 a 100, fornece pseudoaleatóriedade na geração dos
                               obstáculos e garante que pertencem ao terreno adequado.
                          -}  
                   -> (Terreno, [Obstaculo]) 
                   -> [Obstaculo]
adicionaObstaculos l i t@(terr, o)
  | l > (lgt * 2) = adicionaObstaculos l i (terr, (o ++ [lto !! (mod i r)]))
  | l > lgt       = adicionaObstaculos l i (terr, (o ++ [lto !! (mod (i*2) r)]))
  | otherwise = o
  where lto = proximosObstaculosValidos l t
        lgt = length o
        r   = length lto

{- |
  'proximosObstaculosValidos' devolve uma lista de possíveis obstáculos para
  uma única posição num dado terreno; seguindo os seguintes critérios:

  * Troncos têm, no máximo, 5 unidades de comprimento.

  * Carro têm, no máximo, 3 unidades de comprimento.

  * O mapa é circular.

  * Existe pelo menos um obstáculo @Nenhum@ em cada linha.

  === Exemplos
  
  Restrições por circularidade:
  >>> proximosObstaculosValidos 5 (Estrada 2, [Carro,Nenhum,Carro,Carro])
  [Nenhum]
  
  Nenhum obstáculo @Nenhum@ ainda presentes na linha:
  >>> proximosObstaculosValidos 1 (Relva, [])
  [Nenhum]

  >>> proximosObstaculosValidos 3 (Relva, [Arvore, Arvore])
  [Nenhum]
-}

proximosObstaculosValidos :: Int  -- ^ Comprimento final da lista
            -> (Terreno, [Obstaculo]) 
            -> [Obstaculo]
proximosObstaculosValidos l (Rio _, [])                             
  | l > 1                                                           = [Nenhum, Tronco]
  | otherwise                                                       = [Nenhum]
proximosObstaculosValidos l (Rio _, o)
  | l       == lgt                                                  = []  
  | l - lgt == 1 && not (elem Nenhum o)                             = [Nenhum]
  | l - lgt == 1 && obstaculosCirculares o                      < 5 = [Nenhum, Tronco] 
  | l - lgt /= 1 && contaConsecutivos (== Tronco) (reverse o)   < 5 = [Nenhum, Tronco]
  | otherwise                                                       = [Nenhum]
  where lgt = length o
proximosObstaculosValidos l (Estrada _, [])   
  | l > 1                                                           = [Nenhum, Carro]
  | otherwise                                                       = [Nenhum]                      
proximosObstaculosValidos l (Estrada _, o)
  | l       == lgt                                                  = []       
  | l - lgt == 1 && not (elem Nenhum o)                             = [Nenhum]
  | l - lgt == 1 && obstaculosCirculares o                      < 3 = [Nenhum, Carro] 
  | l - lgt /= 1 && contaConsecutivos (== Tronco) (reverse o)   < 5 = [Nenhum, Carro]
  | otherwise                                                       = [Nenhum]
  where lgt = length o  
proximosObstaculosValidos l (Relva, [])   
  | l > 1                                                           = [Nenhum, Arvore]
  | otherwise                                                       = [Nenhum]                                                                   
proximosObstaculosValidos l (Relva, o)
  | l       == lgt                                                  = []  
  | l - lgt == 1 && not (elem Nenhum o)                             = [Nenhum]
  | otherwise                                                       = [Nenhum, Arvore]
  
  where lgt = length o

{- |
  'obstaculosCirculares' devolve o número de obstáculos consecutivos do ínicio
  e do fim de uma lista, se estes forem todos iguais. Caso o obstáculo final
  seja diferente do incial, apenas devolve a contagem dos obstáculos iniciais.

  === Exemplos

  >>> obstaculosCirculares [Carro,Nenhum,Carro,Nenhum,Carro,Carro]
  3

  >>> obstaculosCirculares [Carro,Carro,Nenhum,Carro,Nenhum]
  2

-}

obstaculosCirculares :: [Obstaculo] 
                     -> Int
obstaculosCirculares [] = 0
obstaculosCirculares l@(o:obs)
  | head r == o = (contaConsecutivos (== o) l) + (contaConsecutivos (==o) r)
  | otherwise   = (contaConsecutivos (== o) l)
  where r = reverse l






