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
Module      : RenderMapa_2022lig012
Description : Ferramentas para desenhar o mapa no ecrã
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module RenderMapa_2022li1g012 (
  -- * Funções expostas
  renderizarMapa, renderizarJogo,
  -- * Funções auxiliares
  renderizarLinha, separarEstrada, renderizarObstaculo, coordenadasObstaculo,
  renderizarJogador, imagemBaseJogador, jogadorCima,
  -- ** Funções para testagem
  testarArte
  ) where

import Graphics.Gloss
import Data.List
import Codec.BMP -- TODO - remove

import LI12223

{-|
  'coordenadasObstaculo' é uma função auxiliar de 'renderizarObstaculo'. Dado
  um obstáculo num contexto (terrenos, etc.), esta função devolve a posição da
  imagem correspondente no mapa de tiles.
-}
coordenadasObstaculo :: Terreno    -- ^ Terreno da linha anterior
                     -> Terreno    -- ^ Terreno da linha atual
                     -> Int        -- ^ Variante de animação
                     -> Obstaculo  -- ^ Obstáculo a renderizar
                     -> (Int, Int) -- ^ Posição na bitmap
coordenadasObstaculo (Rio _) Relva       _    Nenhum = (32,  112)
coordenadasObstaculo (Rio _) _           _    Arvore = (64,  112)
coordenadasObstaculo _       Relva       _    Nenhum = (16,  112)
coordenadasObstaculo _       _           _    Arvore = (48,  112)

coordenadasObstaculo _       (Rio _)     0    Nenhum = (96,  112)
coordenadasObstaculo _       (Rio _)     1    Nenhum = (112, 112)
coordenadasObstaculo _       (Rio _)     0    Tronco = (96,  96)
coordenadasObstaculo _       (Rio _)     1    Tronco = (112, 96)

coordenadasObstaculo _       (Estrada _) 0    Nenhum = (80,  112)
coordenadasObstaculo _       (Estrada _) 1    Carro  = (0,   96)
coordenadasObstaculo _       (Estrada _) (-1) Carro  = (16,  96)
coordenadasObstaculo _       (Estrada _) 2    Carro  = (32,  96)
coordenadasObstaculo _       (Estrada _) 3    Carro  = (48,  96)
coordenadasObstaculo _       (Estrada _) (-2) Carro  = (64,  96)
coordenadasObstaculo _       (Estrada _) (-3) Carro  = (80,  96)
coordenadasObstaculo _       (Estrada _) 4    Carro  = (0,   80)
coordenadasObstaculo _       (Estrada _) 5    Carro  = (16,  80)
coordenadasObstaculo _       (Estrada _) 6    Carro  = (32,  80)
coordenadasObstaculo _       (Estrada _) (-4) Carro  = (48,  80)
coordenadasObstaculo _       (Estrada _) (-5) Carro  = (64,  80)
coordenadasObstaculo _       (Estrada _) (-6) Carro  = (80,  80)

coordenadasObstaculo _       _           _    _      = (0, 0)

{-|
  'renderizarObstaculo' devolve a imagem correspondente a um obstáculo num
  mapa, dado o contexto em que se encontra (terreno anterior e atual). A
  variante de animação está relacionada com diferentes imagens associadas ao
  mesmo obstáculo: animação dos rios e diferentes imagens de carros conforme o
  comprimento.
-}
renderizarObstaculo :: BitmapData -- ^ Tilemap
                    -> Terreno    -- ^ Terreno da linha anterior
                    -> Terreno    -- ^ Terreno da linha atual
                    -> Int        -- ^ Variante de animação
                    -> Obstaculo  -- ^ Obstáculo a renderizar
                    -> Picture    -- ^ Imagem do obstáculo
renderizarObstaculo b t t' v o = Scale 2 2 $
  BitmapSection (Rectangle (coordenadasObstaculo t t' v o) (16, 16)) b

{-|
  'separarEstrada' devolve a lista de obstáculos (juntamente com as suas
  variantes de animação) numa linha de terreno 'Estrada'. Isto é necessário
  porque carros de diferentes comprimentos têm diferentes variantes de
  animação, algo que não ocorre em outras linhas.
-}
separarEstrada :: (Terreno, [Obstaculo]) -- ^ Linha de estrada
               -> [(Int, Obstaculo)]     -- ^ Variantes de animação e obstáculos
separarEstrada (Estrada v, l) = grp''
  where lgt = length l
        -- replicate 3 usado para carros nas bordas não usarem imagens dos mais
        -- pequenos
        grp = group $ concat $ replicate 3 l
        -- Limitar carros a 3 de comprimento
        limit l = if length l <= 3 then [l] else let (a, b) = splitAt 3 l in
          a : limit b
        grp' = map (zip [0..]) $ concat $ map limit grp
        -- Associar um obstáculo (carro ou estrada) à sua variante de animação
        -- (relacionada com o comprimento e posição do carro). Nota: signum'
        -- é para determinar o sentido dos carros em estradas sem velocidade.
        signum' n = let s = signum n in if s == 0 then 1 else s
        variante _ (_, Nenhum) = (0, Nenhum)
        variante n (i, Carro)  = (signum' v * (sum [1..(n - 1)] + i + 1), Carro)
        grp'' = take lgt $ drop lgt $ concat $ -- tirar lista triplicada
                  map (\ l -> map (variante (length l)) l) grp'

separarEstrada _ = [] -- evitar função parcial. não deve acontecer

{-|
  'renderizarLinha' devolve a lista de imagens (uma para cada obstáculo) para a
  renderização de uma linha de uma maoa. Estas já vêm com a translação adequada
  para 'renderizarMapa', de modo a reduzir o número de translações necessárias
  e não ultrapassar os limites da stack do OpenGL.

  Parâmetros como o tempo decorrido e o tipo de terreno da linha anterior
  servem para animar os rios do mapa e escolher tiles de transição entre
  terrenos, respetivamente.
-}
renderizarLinha :: BitmapData             -- ^ Tilemap
                -> Float                  -- ^ Tempo decorrido
                -> Terreno                -- ^ Terreno da linha anterior
                -> Float                  -- ^ Número da linha
                -> (Terreno, [Obstaculo]) -- ^ Linha a renderizar
                -> [Picture]              -- ^ Imagem de cada obstáculo
renderizarLinha b _  ta y l@(Estrada _, _) =
  map aux $ zip [0..] $ separarEstrada l
  where aux (x, (v, o)) = Translate (x * 32 + 16) (y * 32 + 16) $
                            renderizarObstaculo b ta (Estrada 0) v o
renderizarLinha b td ta y (t, obs) = map aux $ zip [0..] obs
  where variante x y t = mod (round (x + y + t)) 2
        aux (x, o) = Translate (x * 32 + 16) (y * 32 + 16) $
                       renderizarObstaculo b ta t (variante x y td) o

{-|
  'renderizarMapa' devolve a imagem que deve ser usada para se desenhar um
  mapa. __Esta não está centrada__: na origem está o canto inferior esquerdo do
  mapa!
-}
renderizarMapa :: BitmapData -- ^ Tilemap
               -> Float      -- ^ Tempo decorrido
               -> Mapa       -- ^ Mapa a ser renderizado
               -> Picture    -- ^ Imagem do mapa
renderizarMapa b td (Mapa _ lns) =
  Pictures $ trd $ foldr aux (0, Relva, []) lns
  where aux l@(t', o) (n, t, p) = (n + 1, t', renderizarLinha b td t n l ++ p)
        trd (_, _, x) = x

-- | Imagem do jogador voltado para cima em função do tilemap
jogadorCima :: BitmapData -> Picture
jogadorCima = Scale 2 2 . BitmapSection (Rectangle (0, 112) (16, 16))

{-|
  'imagemBaseJogador' devolve uma transformação de 'BitmapData' em 'Picture'
  conforme o a direção do jogador (usada em 'renderizarJogador').
-}
imagemBaseJogador :: Direcao
                  -> (BitmapData -> Picture)
imagemBaseJogador Baixo    = Rotate 180   . jogadorCima
imagemBaseJogador Esquerda = Rotate (-90) . jogadorCima
imagemBaseJogador Direita  = Rotate 90    . jogadorCima
imagemBaseJogador _        = jogadorCima

{-|
  'renderizarJogador' devolve a imagem que deve ser utilizada para renderizar
  o jogador no ecrã (com as transformações adequadas).
-}
renderizarJogador :: BitmapData -- ^ Tilemap
                  -> Direcao    -- ^ Orientação do jogador
                  -> Int        -- ^ Altura do mapa
                  -> Jogador    -- ^ Posição do jogador
                  -> Picture    -- ^ Imagem do jogador
renderizarJogador b d h (Jogador (x, y)) =
  Translate (fromIntegral x * 32 + 16) (fromIntegral (h - y) * 32 - 16) $
  imagemBaseJogador d b

{-|
  'renderizarJogo' devolve a imagem que deve ser usada para se desenhar um
  jogo (mapa e jogador). __Esta não está centrada__: na origem está o canto
  inferior esquerdo do mapa!
-}
renderizarJogo :: BitmapData -- ^ Tilemap
               -> Float      -- ^ Tempo decorrido
               -> Direcao    -- ^ Orientação do jogador
               -> Jogo       -- ^ Mapa a ser renderizado
               -> Picture    -- ^ Imagem do mapa
renderizarJogo b t d (Jogo j m@(Mapa _ lns)) = Pictures [
  renderizarMapa b t m, renderizarJogador b d (length lns) j]

{-|
  'testarArte' cria uma janela com um mapa de exemplo, para se ver a arte do
  jogo e se testarem alterações à mesma.
-}
testarArte :: IO ()
testarArte = do
  (Right b) <- readBMP "assets/export/Tiles.bmp"
  let i = bitmapDataOfBMP b
  simulate (InWindow "" (500, 500) (0, 0)) black 60 0 (\ t -> Pictures $ [
    color white $ Line [(-250, 0), (250, 0)], color white $ Line [(0, 250), (0, -250)]]
    ++ [renderizarJogo i t Direita $ Jogo (Jogador (0, 0)) $
    Mapa 5 [(Relva,        [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
            (Rio 1,        [Nenhum, Nenhum, Tronco, Nenhum, Tronco]),
            (Relva,        [Arvore, Arvore, Nenhum, Arvore, Arvore]),
            (Estrada 2,    [Carro, Nenhum, Carro, Carro, Carro]),
            (Estrada (-1), [Carro, Carro, Nenhum, Nenhum, Carro]),
            (Estrada (-2), [Carro, Carro, Carro, Carro, Carro]),
            (Relva,        [Nenhum, Arvore, Arvore, Arvore, Nenhum]),
            (Estrada 1,    [Carro, Carro, Nenhum, Nenhum, Nenhum])]])
    (\ _ -> (+))
