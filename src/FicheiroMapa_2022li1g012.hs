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
module FicheiroMapa_2022li1g012 where

import System.FilePath
import System.Directory
import System.IO.Error

-- | A diretoria onde se encontram os mapas. Caso não exista, será criada.
diretoriaMapas :: IO FilePath
diretoriaMapas = do
  path <- getXdgDirectory XdgData "CrossyRoad/Mapas"
  createDirectoryIfMissing True path
  return path

-- | 'listarMapas' devolve a lista de ficheiros @.map@ em 'diretoriaMapas'
listarMapas :: IO [FilePath]
listarMapas = diretoriaMapas >>=
              listDirectory >>=
              (return . filter (endsWith ".map"))
  where endsWith c s = (drop (length s - length c) s) == c

{-|
  'nomeMapa' devolve o nome de um mapa com base no seu caminho de ficheiro.

  === Exemplo

  >>> nomeMapa "~/.local/share/CrossyRoad/Mapas/dificil.map"
  "dificil"
-}
nomeMapa :: FilePath -> String
nomeMapa = dropExtension . takeFileName

