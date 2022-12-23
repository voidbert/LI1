module Audio_2022li1g012 where

import System.Process

pid = spawnCommand "pidof mpv"

audioMenu :: IO (ProcessHandle)
audioMenu = spawnCommand "mpv --no-input-terminal --loop --playlist-start=0 src/GameAudioPlaylist"

audioJogo :: IO (ProcessHandle)
audioJogo = spawnCommand "mpv --no-input-terminal --loop --playlist-start=1 src/GameAudioPlaylist"

audioGO :: IO (ProcessHandle)
audioGO = spawnCommand "mpv --no-input-terminal --loop --playlist-start=2 src/GameAudioPlaylist"

mataprocesso :: IO (ProcessHandle)
mataprocesso = spawnCommand "killall mpv" 