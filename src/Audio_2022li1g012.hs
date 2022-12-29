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
Module      : Audio_2022li1g012
Description : Engine de reprodução de áudio
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module Audio_2022li1g012 (
  -- * Funções principais
  pararAudio', comecarAudio', pararAudios,
  -- * Funções auxiliares
  pararAudio, comecarAudio,
  -- ** Armazenamento de processos de som
  ficheiroAudios, gravarAudios, lerAudios, pararGuardados,
  -- * Sons do jogo
  -- | Caminhos de ficheiro para os sons usados
  menuAudio, jogoAudio, goAudio
 ) where

import Data.List
import System.Process
import System.IO.Error
import System.Directory
import System.FilePath
import Text.Read

import LI12223

menuAudio = "assets/export/AudioPlaylist/Menu.wav" 
jogoAudio = "assets/export/AudioPlaylist/Jogo.wav"
goAudio   = "assets/export/AudioPlaylist/GameOver.wav"

{-|
  O caminho de ficheiro do ficheiro onde guardar os PIDs do @mpv@. Ler
  'Main.main' para mais informação.
-}
ficheiroAudios :: IO (Maybe FilePath)
ficheiroAudios = do
  dir <- tryIOError $ getTemporaryDirectory
  case dir of (Left _)  -> return Nothing
              (Right d) -> return $ Just (d </> "AudiosCrossyRoad")

{-|
  'gravarAudios' guarda no disco a lista de PIDs de todos os processos do
  @mpv@. Isto serve para parar os áudios caso a janela seja fechada, um
  workaround explicado em 'Main.main'.
-}
gravarAudios :: Audios -> IO ()
gravarAudios ads = do
  let pids = map (getPid . snd) ads
      catIOMaybes [] = return []
      catIOMaybes (x:xs) = x >>=
        \ x' -> case x' of Nothing    -> catIOMaybes xs
                           (Just x'') -> catIOMaybes xs >>= \ y -> return (x'':y)
  e <- ficheiroAudios
  case e of Nothing   -> return ()
            (Just fp) -> do ms <- catIOMaybes pids -- Lista de PIDs
                            e' <- tryIOError $ writeFile fp (show ms)
                            -- Continuar mesmo se o ficheiro não foi gravado
                            return ()

-- | 'lerAudios' lê os PIDs do @mpv@ guardados em 'ficheiroAudios'.
lerAudios :: IO (Maybe [Pid])
lerAudios = do
  e <- ficheiroAudios
  case e of Nothing   -> return Nothing
            (Just fp) -> do e' <- tryIOError $ readFile fp
                            case e' of (Left _)  -> return Nothing
                                       (Right c) -> return $ readMaybe c
{-|
  'pararGuardados' mata todos os processos do @mpv@ guardados em
  'ficheiroAudios'.
-}
pararGuardados :: IO ()
pararGuardados = do
  let matar []     = return ()
      matar (x:xs) = tryIOError (spawnProcess "kill" [show x]) >> matar xs
  mpids <- lerAudios
  case mpids of Nothing     -> return ()
                (Just pids) -> matar pids

-- | 'pararAudio' para um áudio dado um tipo 'Audios' e um FilePath.

pararAudio :: Audios -> FilePath -> IO Audios
pararAudio ads fp = do
  let ma = find (equalFilePath fp . fst) ads
      del _ []     = [] -- delete e deleteBy não se aplicam devido ao ProcessHandle
      del f (x@(fx,_):xs) = if fx `equalFilePath` f then xs else x : del f xs
  case ma of Nothing  -> return ads -- Áudio não encontrado
             (Just (f, ph)) -> do tryIOError $ terminateProcess ph
                                  let n = del f ads
                                  gravarAudios n
                                  return n

-- | 'comecarAudio' começa um áudio dado um tipo 'Audios' e um FilePath.

comecarAudio :: Audios -> FilePath -> IO Audios
comecarAudio ads fp = do
  ads' <- pararAudio ads fp -- Parar o som antes de o começar
  h <- tryIOError $ spawnProcess "mpv" ["--loop", "--no-terminal", fp]
  case h of (Left _)   -> return ads' -- Falha ao iniciar o áudio
            (Right h') -> do let n = (fp, h') : ads'
                             gravarAudios n
                             return n

{-|
  'comecarAudio'' permite o uso do construtor 'Assets' na função comecarAudio,
  para ser possível a adição simples do áudio nos módulos necessários.
-} 

comecarAudio' :: Assets -> FilePath -> IO Assets 
comecarAudio' (Assets f t b a) fp = comecarAudio a fp >>= 
  \ x -> return (Assets f t b x)

{-|
  'pararAudio'' permite o uso do construtor 'Assets' na função pararAudio,
  para ser possível a remoção simples do áudio nos módulos necessários.
-} 

pararAudio' :: Assets -> FilePath -> IO Assets
pararAudio' (Assets f t b a) fp = pararAudio a fp >>= 
  \ x -> return (Assets f t b x)

-- | 'pararAudios' para todos os audios contidos numa lista de Audios. 

pararAudios :: Audios -> IO ()
pararAudios [] = return ()
pararAudios (x:xs) = pararAudio (x:xs) (fst x) >>= pararAudios 

