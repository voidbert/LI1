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
Module      : Main
Description : Ponto de entrada para o programa
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Main (
  -- * Ponto de entrada
  main,
  -- * Gloss
  renderizarGloss, eventosGloss, tempoGloss,
  -- * Leitura de assets
  lerBMP, lerPicture, lerAssets
  ) where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO.Error
import Codec.BMP

import LI12223
import MenuP_2022li1g012
import GameOver_2022li1g012
{-|
  'renderizarGloss', dado um estado de jogo, devolve os conteúdos que devem ser
  desenhados no ecrã.
-}
renderizarGloss :: EstadoJogo -- ^ Atual estado de jogo
                -> IO Picture -- ^ Conteúdos no ecrã
renderizarGloss j@(EJ _ (FJ _ _ r) _) = r j

{-|
  'eventosGloss' reage a ('Graphics.Gloss.Interface.IO.Game.Event')s do gloss.
  Eventos de redimensionamento não são enviados para as funções de atualização
  de um estado de jogo.
-}
eventosGloss :: Event         -- ^ Evento em análise
             -> EstadoJogo    -- ^ Atual estado de jogo
             -> IO EstadoJogo -- ^ Estado de jogo com o novo evento
eventosGloss (EventResize t) j = return j
eventosGloss e j@(EJ _ (FJ _ r _) _) = r e j

{-|
  'tempoGloss' reage à passagem de tempo, atualizando o estado de jogo.
-}
tempoGloss :: Float         -- ^ Tempo passado desde a última atualização
           -> EstadoJogo    -- ^ Estado de jogo e eventos atuais
           -> IO EstadoJogo -- ^ Estado de jogo seguinte
tempoGloss t j@(EJ _ (FJ a _ _) _) = a t j



-- | 'lerBMP' lê um ficheiro do tipo bitmap.
lerBMP :: FilePath -> IO BMP
lerBMP fp = tryIOError (readBMP fp) >>= filterError
  where filterError (Left  _) = error ("Erro a ler bitmap " ++ fp)
        filterError (Right (Left _)) = error ("Erro a ler bitmap " ++ fp)
        filterError (Right (Right i)) = return i

{-|
 'lerPicture' lê um ficheiro do tipo bitmap ('lerBMP') e transforma-o numa
 'Picture' do gloss.
-}
lerPicture :: FilePath -> IO Picture
lerPicture x = lerBMP x >>= (return . bitmapOfBMP)

{-|
  'lerAssets' carrega do disco todos os recursos (imagens, sons, ...)
  necessários para o funcionamento do jogo.
-}
lerAssets :: IO Assets
lerAssets = do
  fnt <- lerBMP "assets/export/Font.bmp"
  bld <- lerPicture "assets/export/Balde.bmp"
  tls <- lerBMP "assets/export/Tiles.bmp"
  return (Assets (bitmapDataOfBMP fnt) (bitmapDataOfBMP tls) bld [])

-- | Ponto de entrada do programa, onde se abre a janela com o jogo.
main :: IO ()
main = do
  let janela = InWindow "Crossy Road" (768, 768) (0, 0)
  assets <- lerAssets
  inicial <- inicializarMenu assets
  playIO janela black 60 inicial renderizarGloss eventosGloss tempoGloss
