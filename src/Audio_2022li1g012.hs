module Audio_2022li1g012 where

import Data.List
import System.Process
import System.IO.Error
import System.FilePath

import LI12223

menuAudio = "assets/export/AudioPlaylist/Menu.wav" 

pararAudio :: Audios -> FilePath -> IO Audios
pararAudio ads fp = do
  let ma = find (equalFilePath fp . fst) ads
      del _ []     = [] -- delete e deleteBy não se aplicam devido ao ProcessHandle
      del f (x@(fx,_):xs) = if fx `equalFilePath` f then xs else x : del f xs
  case ma of Nothing  -> return ads -- Áudio não encontrado
             (Just (f, ph)) -> do tryIOError $ terminateProcess ph
                                  return $ del f ads

comecarAudio :: Audios -> FilePath -> IO Audios
comecarAudio ads fp = do
  ads' <- pararAudio ads fp -- Parar o som antes de o começar
  h <- tryIOError $ spawnProcess "mpv" ["--loop", "--no-terminal", fp]
  case h of (Left _)   -> return ads' -- Falha ao iniciar o áudio
            (Right h') -> return ((fp, h') : ads')

comecarAudio' :: Assets -> FilePath -> IO Assets 
comecarAudio' (Assets f t b a) fp = comecarAudio a fp >>= 
  \ x -> return (Assets f t b x)

pararAudio' :: Assets -> FilePath -> IO Assets
pararAudio' (Assets f t b a) fp = pararAudio a fp >>= 
  \ x -> return (Assets f t b x)

pararAudios :: Audios -> IO ()
pararAudios [] = return ()
pararAudios (x:xs) = pararAudio (x:xs) (fst x) >>= pararAudios 

