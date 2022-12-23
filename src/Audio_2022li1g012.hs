module Audio_2022li1g012 where

import System.Process

pid = spawnProcess "pidof" ["mpv"]

audioMenu :: IO (ProcessHandle)
audioMenu = spawnProcess "mpv" ["--no-input-terminal", "--loop", "--playlist-start=0", "GameAudioPlaylist"]

audioJogo :: IO (ProcessHandle)
audioJogo = spawnProcess "mpv" ["--no-input-terminal", "--loop", "--playlist-start=1", "GameAudioPlaylist"]  

audioGO :: IO (ProcessHandle)
audioGO = spawnProcess "mpv" ["--no-input-terminal", "--loop", "--playlist-start=2", "GameAudioPlaylist"]


mataprocesso :: IO (ProcessHandle)
mataprocesso = spawnCommand "killall mpv" 