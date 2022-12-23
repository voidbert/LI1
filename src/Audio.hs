module Audio where

import System.Process

pid = spawnCommand "pidof mpv"

musicaMenu :: IO (ProcessHandle)
musicaMenu = spawnCommand "mpv --no-input-terminal --loop --playlist-start=0 GameAudioPlaylist"

musicaJogo :: IO (ProcessHandle)
musicaJogo = spawnCommand "mpv --no-input-terminal --loop --playlist-start=1 GameAudioPlaylist"

musicaGO :: IO (ProcessHandle)
musicaGO = spawnCommand "mpv --no-input-terminal --loop --playlist-start=2 GameAudioPlaylist"

mataprocesso :: IO (ProcessHandle)
mataprocesso = spawnCommand "killall mpv" 