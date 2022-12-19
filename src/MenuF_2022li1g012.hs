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
Module      : MenuF_2022li1g012
Description : Menu do modo frogger (escolha de mapa)
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module MenuF_2022li1g012 (
  -- * Funções expostas
  inicializarMenuF,
  -- * Funções internas
  tempoMenuF, eventoMenuF, renderizarMenuF, renderizarLinha,
  linhaMenu, linhasMenu
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO.Error

import LI12223
import UI_2022li1g012
import FicheiroMapa_2022li1g012
import ErroM_2022li1g012
import {-# SOURCE #-} MenuP_2022li1g012 -- módulos mutuamente recursivos

-- | 'tempoMenuF' reage à passagem do tempo (nenhuma)
tempoMenuF :: Float -> EstadoJogo -> IO EstadoJogo
tempoMenuF _ = return

data Acao = Nenhuma | Apagar | Editar | Jogar deriving (Eq, Show)

cliqueLinha :: (Float, Float) -> (FilePath, Picture, [Botao]) -> Acao
cliqueLinha p (_, _, [(r1, _), (r2, _), (r3, _)])
  | dentro r1 p = Apagar
  | dentro r2 p = Editar
  | dentro r3 p = Jogar
  | otherwise = Nenhuma
cliqueLinha _ (_, _, _) = Nenhuma -- não é suposto acontecer

cliqueLinhas :: EstadoJogo -- ^ Estado do jogo
             -> IO EstadoJogo
cliqueLinhas ej@(EJ (MenuF _ _ [] _) _ _) = return ej
cliqueLinhas ej@(EJ (MenuF p v (x@(fp, _, _):xs) b) f as)
  | a == Nenhuma = cliqueLinhas $ EJ (MenuF p v xs b) f as
  | a == Editar  = return ej -- TODO
  | a == Jogar   = return ej -- TODO
  | a == Apagar  = do apg <- apagarMapa fp
                      if apg then inicializarMenuF as v -- Reler pasta
                        else inicializarErroM as "Erro ao apagar\n\no mapa :("
  where a = cliqueLinha p x

{-|
  'eventoMenuF' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoMenuF :: Event -> EstadoJogo -> IO EstadoJogo
eventoMenuF (EventMotion (x, y)) (EJ (MenuF _ v l bts) f b) =
  return $ EJ (MenuF (x, y) v l bts) f b
eventoMenuF (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (MenuF _ v l bts) _ b)
  -- TODO - quando scroll for implementado, não esquecer de não carregar em
  -- botões escondidos
  | dentro (fst (bts !! 1)) (x, y) = inicializarMenu b
  | otherwise = cliqueLinhas ej
eventoMenuF _ e = return e

-- | 'renderizarLinha' gera a imagem para renderizar um mapa da lista
renderizarLinha :: (Float, Float)               -- ^ Posição do rato
                -> (FilePath, Picture, [Botao]) -- ^ Linha
                -> Picture                      -- ^ Imagem produzida
renderizarLinha p (_, t, bts) = Pictures (t : map (imagemBotao p) bts)

-- | 'renderizarMenuF' é responsável por desenhar o menu no ecrã.
renderizarMenuF :: EstadoJogo -> IO Picture
renderizarMenuF (EJ (MenuF p v l bts) _ b) = return $ Pictures (
  t : map (imagemBotao p) bts ++ linhas)
  where t = let ((_, h), t') = mrTexto (fonte b) TCentro "Escolha um mapa"
            in Translate 0 (384 - 16 - h * 2.5) $ Scale 5 5 t'
        linhas = if null l then [Scale 2 2 $ snd $
                   mrTexto (fonte b) TCentro "Sem mapas instalados"]
                   else map (renderizarLinha p) l

{-|
  'linhaMenu' gera uma linha deste menu, com o nome de um mapa e opções como
  jogar, editar e eliminar. O número da linha é necessário para se calcular a
  translação vertical.
-}
linhaMenu :: BitmapData                   -- ^ Imagem da fonte
          -> Float                        -- ^ Número da linha
          -> FilePath                     -- ^ Caminho de ficheiro do mapa
          -> (FilePath, Picture, [Botao]) -- ^ Linha do menu
linhaMenu b v fp = (fp, t', bts')
  where ((w, h), t) = mrTexto b TCentro $ nomeMapa fp
        t' = Translate (-384 + w + 16) (264 - h / 2 - v * 64) $ Scale 2 2 t
        (bw, bts) = gerarBotoesEspH b 8 2 [ "Apagar", "Editar", "Jogar" ]
        bts' = map (translateBt (384 - bw / 2 - 16) (264 - h / 2 - v * 64)) bts

{-|
  'linhasMenu' gera todas as linhas deste menu, uma para cada mapa (ver
  'linhaMenu').
-}
linhasMenu :: BitmapData -> [FilePath] -> [(FilePath, Picture, [Botao])]
linhasMenu b = map (uncurry $ linhaMenu b) . zip [0..]

-- | 'inicializarMenuF' devolve o estado inicial deste menu.
inicializarMenuF :: Assets        -- ^ Assets do jogo
                 -> Float         -- ^ Posição vertical (scroll)
                 -> IO EstadoJogo -- ^ Estado de jogo do menu
inicializarMenuF a v = do
  let funcoesMenuF = FJ tempoMenuF eventoMenuF renderizarMenuF
      b1@((_, (w1, h1)), _) = scaleBt 2 $ mrBotao (fonte a) "Criar mapa"
      b2@((_, (w2, h2)), _) = scaleBt 2 $ mrBotao (fonte a) "Voltar"
      b1' = translateBt (384 - w1 / 2 - 16)  (-384 + h1 / 2 + 16) b1
      b2' = translateBt (-384 + w2 / 2 + 16) (-384 + h2 / 2 + 16) b2
  ms <- tryIOError $ listarMapas
  case ms of (Left _) -> inicializarErroM a
                           "Ocorreu um erro a\n\nlistar os mapas :("
             (Right m) -> let lns = linhasMenu (fonte a) m
                              bts = [b1', b2']
                          in return $ EJ (MenuF (0, 0) v lns bts) funcoesMenuF a
