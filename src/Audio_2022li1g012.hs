module Audio_2022li1g012 where
import System.Process
import System.IO.Error

import LI12223

data Audios = Audio {
  menu :: Maybe ProcessHandle,
  jogo :: Maybe ProcessHandle,
  gameOver :: Maybe ProcessHandle              
 }


comecarMenu :: Audios -> IO Audios
comecarMenu a = do
  -- pararMenu não fará nada se o audio estiver parado. Mas vai pará-lo se estiver a tocar, para
  -- se recomeçar
  (Audio m j g) <- pararMenu a
  h <- tryIOError $ spawnProcess "mpv" ["--loop", "--no-terminal", "AudioPlaylist/Menu.wav"] -- abrir mpv
  case h of (Left _)  -> return $ Audio Nothing j g -- Falha ao abrir
            (Right m') -> return $ Audio (Just m') j g -- O mpv abriu

pararMenu :: Audios -> IO Audios
pararMenu a@(Audio Nothing j g) = return a -- ^ Não parar o que já está parado
pararMenu (Audio (Just m) j g) = do
  tryIOError $ terminateProcess m -- Tentar parar o mpv
  return $ Audio Nothing j g -- Consiga-se ou não parar, o 1.º argumento é Nothing

comecarJogo :: Audios -> IO Audios
comecarJogo a = do
  -- pararJogo não fará nada se o audio estiver parado. Mas vai pará-lo se estiver a tocar, para
  -- se recomeçar
  (Audio m j g) <- pararJogo a
  h <- tryIOError $ spawnProcess "mpv" ["--loop", "--no-terminal", "AudioPlaylist/Jogo.wav"] -- abrir mpv
  case h of (Left _)  -> return $ Audio m Nothing g -- Falha ao abrir
            (Right j') -> return $ Audio m (Just j') g -- O mpv abriu

pararJogo :: Audios -> IO Audios
pararJogo a@(Audio m Nothing g) = return a -- ^ Não parar o que já está parado
pararJogo (Audio m (Just j) g) = do
  tryIOError $ terminateProcess j -- Tentar parar o mpv
  return $ Audio m Nothing g -- Consiga-se ou não parar, o 1.º argumento é Nothing


comecarGO :: Audios -> IO Audios
comecarGO a = do
  -- pararJogo não fará nada se o audio estiver parado. Mas vai pará-lo se estiver a tocar, para
  -- se recomeçar
  (Audio m j g) <- pararJogo a
  h <- tryIOError $ spawnProcess "mpv" ["--loop", "--no-terminal", "AudioPlaylist/GameOver.wav"] -- abrir mpv
  case h of (Left _)  -> return $ Audio m Nothing g -- Falha ao abrir
            (Right j') -> return $ Audio m (Just j') g -- O mpv abriu

pararGO :: Audios -> IO Audios
pararGO a@(Audio m j Nothing) = return a -- ^ Não parar o que já está parado
pararGO (Audio m j (Just g)) = do
  tryIOError $ terminateProcess g -- Tentar parar o mpv
  return $ Audio m j Nothing -- Consiga-se ou não parar, o 1.º argumento é Nothing

-- Isto é só para testagem. Na prática tens o gloss.
loopInput :: Audios -> IO ()
loopInput a = do
  putStrLn "\n\n\n1) Começar menu\n2) Começar jogo\n3) Parar menu\n4) Parar jogo\n\n\n"
  c <- getChar
  case c of '1' -> comecarMenu a >>= loopInput
            '2' -> comecarJogo a >>= loopInput
            '3' -> pararMenu   a >>= loopInput