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
Module      : FicheiroMapa_2022li1g012
Description : Armazenamento e leitura de mapas finitos no disco
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module FicheiroMapa_2022li1g012 (
  -- * Listagem e gestão de mapas
  listarMapas, diretoriaMapas, nomeMapa, apagarMapa,
  -- * Exportação e importação de mapas
  mapaStr, parseMapa, lerFicheiroMapa, guardarFicheiroMapa,
  -- * Funções auxiliares
  -- ** Exportação de mapas
  terrenoStr, obstaculoC, linhaStr,
  -- ** Importação de mapas
  parseTerreno, parseObstaculo, parseObstaculos, parseLinha, medirMapa
  ) where

import System.FilePath
import System.Directory
import System.IO.Error
import Data.List
import Data.Maybe
import Text.Read

import LI12223

-- | A diretoria onde se encontram os mapas. Caso não exista, será criada.
diretoriaMapas :: IO FilePath
diretoriaMapas = do
  path <- getXdgDirectory XdgData "CrossyRoad/Mapas"
  createDirectoryIfMissing True path
  return path

-- | 'listarMapas' devolve a lista de ficheiros @.map@ em 'diretoriaMapas'
listarMapas :: IO [FilePath]
listarMapas = do
  d <- diretoriaMapas
  l <- listDirectory d
  return $ map (d </>) $ filter (endsWith ".map") l
  where endsWith c s = (drop (length s - length c) s) == c

{-|
  'nomeMapa' devolve o nome de um mapa com base no seu caminho de ficheiro.

  === Exemplo

  >>> nomeMapa "~/.local/share/CrossyRoad/Mapas/dificil.map"
  "dificil"
-}
nomeMapa :: FilePath -> String
nomeMapa = dropExtension . takeFileName

{-|
  'apagarmapa' apaga um ficheiro de mapa e devolve se a operação teve (ou não)
  sucesso.
-}
apagarMapa :: FilePath -> IO Bool
apagarMapa f = do
  s <- tryIOError (removeFile f)
  case s of (Left _)  -> return False
            (Right _) -> return True

{-|
  'terrenoStr' converte o 'Terreno' de uma linha para uma 'String', para ser
  armazenada num ficheiro com o mapa. O resultado será uma letra (G para relva,
  R para rio e E para estrada) seguida de um inteiro para a velocidade, caso
  aplicável.

  === Exemplos

  >>> terrenoStr Relva
  "G"

  >>> terrenoStr (Estrada 3)
  "E3"
-}
terrenoStr :: Terreno -> String
terrenoStr Relva       = "G"
terrenoStr (Rio n)     = "R" ++ show n
terrenoStr (Estrada n) = "E" ++ show n

{-|
  'obstaculoC' devolve um caracter para representar um 'Obstaculo' num ficheiro
  de mapa. A correspondência obstáculo - caracter é a seguinte:

  - 'Nenhum' - @'N'@
  - 'Arvore' - @'A'@
  - 'Carro'  - @'C'@
  - 'Tronco' - @'T'@
-}
obstaculoC :: Obstaculo -> Char
obstaculoC Nenhum = 'N'
obstaculoC Arvore = 'A'
obstaculoC Carro  = 'C'
obstaculoC Tronco = 'T'

{-|
  'linhaStr' converte uma linha de um 'Mapa' para uma 'String', de modo a ser
  armazenada no ficheiro com o mapa. Uma linha consiste no tipo de terreno
  ('terrenoStr'), uma vírgula, e um caracter para cada obstáculo
  ('obstaculoC').

  === Exemplo

  >>> linhaStr (Rio (-1), [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  "R-1,NTTNN"
-}
linhaStr :: (Terreno, [Obstaculo]) -> String
linhaStr (t, os) = terrenoStr t ++ (',' : map obstaculoC os)

{-|
  'mapaStr' converte um 'Mapa' finito para uma 'String', de modo a ser
  armazenado num ficheiro. O recorde do jogador neste mapa também é armazenado.
  O resultado consiste numa linha para o recorde, seguido de uma linha para
  cada linha do mapa ('linhaStr').

  === Exemplo

  >>> mapaStr 36 (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),
                          (Estrada 2, [Nenhum, Carro, Carro])])
  "36\nG,NNA\nE2,NCC\n"
-}
mapaStr :: Int    -- ^ Recorde do jogador
        -> Mapa   -- ^ Mapa a converter
        -> String -- ^ Mapa convertido
mapaStr r (Mapa _ lns) = unlines (show r : map linhaStr lns)

guardarFicheiroMapa :: Int      -- ^ Recorde do jogador
                    -> Mapa     -- ^ Mapa a guardar
                    -> FilePath -- ^ Onde guardar o mapa
                    -> IO Bool  -- ^ Se a operação teve (ou não) sucesso
guardarFicheiroMapa r m fp = do
  e <- tryIOError (writeFile fp $ mapaStr r m)
  case e of (Left _)  -> return False
            (Right _) -> return True

{-|
  'parseTerreno' devolve um tipo de terreno após analisar a 'String' em que
  estava representado (num ficheiro de mapa). Caso o texto seja inválido,
  'Nothing' será devolvido.

  === Exemplos

  >>> parseTerreno "G"
  Just Relva

  >>> parseTerreno "G3" -- Relva não tem velocidade
  Nothing

  >>> parseTerreno "R-1"
  Just (Rio (-1))

  >>> parseTerreno "H" -- tipo de terreno inexistente
  Nothing

  >>> parseTerreno "ENaN" -- velocidade inválida
  Nothing
-}
parseTerreno :: String        -- ^ Terreno no ficheiro do mapa
             -> Maybe Terreno -- ^ Terreno processado
parseTerreno "" = Nothing
parseTerreno (t:v)
  | t == 'G'  = if v == "" then Just Relva else Nothing
  | t == 'R'  = case readMaybe v of Nothing -> Nothing
                                    Just i -> Just $ Rio i
  | t == 'E'  = case readMaybe v of Nothing -> Nothing
                                    Just i -> Just $ Estrada i
  | otherwise = Nothing

{-|
  'parseObstaculo' transforma um caracter (num ficheiro de mapa) num obstáculo.
  Caso o caracter seja inválido, @Nothing@ é devolvido.
-}
parseObstaculo :: Char            -- ^ Obstáculo no ficheiro do mapa
               -> Maybe Obstaculo -- ^ Obstáculo processado
parseObstaculo 'N' = Just Nenhum
parseObstaculo 'A' = Just Arvore
parseObstaculo 'C' = Just Carro
parseObstaculo 'T' = Just Tronco
parseObstaculo _   = Nothing

{-|
  'parseObstaculos' analisa uma lista de caracteres num ficheiro de mapa e
  transforma-a numa lista de 'Obstaculo's com recurso a 'parseObstaculo'. Caso
  algum caracter seja inválido, 'Nothing' será devolvido.

  === Exemplos

  >>> parseObstaculos "NCCCNN"
  Just [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]

  >>> parseObstaculos "NN:)NN" -- obstáculos ':' e ')' inválidos
  Nothing
-}
parseObstaculos :: String            -- ^ Obstáculos no ficheiro do mapa
                -> Maybe [Obstaculo] -- ^ Obstáculos processados
parseObstaculos = foldr aux (Just []) . map parseObstaculo
  where aux (Just c) (Just l) = Just (c : l)
        aux _ _ = Nothing

{-|
  'parseLinha' analisa uma linha do mapa proveniente de um ficheiro. No caso de
  algum erro na análise, 'Nothing' será devolvido. Ver 'parseTerreno' e
  'parseObstaculos', subrotinas desta função.

  === Exemplos

  >>> parseLinha "G,NNAAN"
  Just (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum])

  >>> parseLinha "Y42,AAA" -- não existe terreno Y
  Nothing

  >>> parseLinha "GNN" -- não há vírgula
  Nothing
-}
parseLinha :: String                       -- ^ Linha no ficheiro do mapa
           -> Maybe (Terreno, [Obstaculo]) -- ^ Linha processada
parseLinha ln
  | isNothing v = Nothing
  | otherwise = let (t, o) = splitAt (fromJust v) ln
                    t' = parseTerreno t
                    o' = parseObstaculos $ tail o
                in if isNothing t' || isNothing o' then Nothing
                     else Just (fromJust t', fromJust $ o')
  where v = findIndex (== ',') ln

{-|
  'medirMapa' verifica se todas as linhas têm a mesma largura, devolvendo um
  mapa com essa largura caso esse seja o caso.
-}
medirMapa :: [Maybe (Terreno, [Obstaculo])] -- ^ Linhas de 'parseLinha'
          -> Maybe Mapa -- ^ Mapa exportado
medirMapa [] = Nothing
medirMapa (Nothing:_) = Nothing
medirMapa lns@((Just (_, o)):t) = let l = length o in if all (aux l) t then
                                    Just $ Mapa l $ catMaybes lns else Nothing
  where aux _ Nothing = False
        aux n (Just (_, x)) = length x == n

{-|
  'parseMapa' analisa os conteúdos de um ficheiro de mapa, devolvendo o 'Mapa'
  e o recorde do jogador neles contidos, ou 'Nothing', em caso de erro
  (conteúdos inválidos). Ver 'mapaStr' para uma descrição do formato do
  ficheiro.

  === Exemplos

  >>> parseMapa "43\nG,NNAN\nR2,TTNN"
  Just (43, Mapa 4 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum]),
                    (Rio 2, [Tronco, Tronco, Nenhum, Nenhum])])

  >>> parseMapa "30\nG,NNAN\nR2,TTNNT" -- linhas com diferente largura
  Nothing
-}
parseMapa :: String            -- ^ Conteúdos do ficheiro do mapa
          -> Maybe (Int, Mapa) -- ^ Recorde e mapa
parseMapa s
  | null lns = Nothing
  | otherwise = let rec = readMaybe $ head lns
                    m = medirMapa $ map parseLinha $ tail lns
                in if isNothing rec || isNothing m then Nothing
                     else Just (fromJust rec, fromJust m)
  where lns = lines s

{-|
  'lerFicheiroMapa' lê os conteúdos de um ficheiro e analisa-os, procurando
  extrair um recorde e um mapa (ver 'parseMapa').
-}
lerFicheiroMapa :: FilePath          -- ^ Localização do ficheiro
                -> IO (Maybe (Int, Mapa)) -- ^ Recorde e mapa
lerFicheiroMapa fp = do
  c <- tryIOError (readFile fp)
  case c of (Left _)   -> return Nothing
            (Right c') -> return $ parseMapa c'
