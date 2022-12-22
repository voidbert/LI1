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
  -- ** Geração do menu
  linhaMenu, linhasMenu,
  -- ** Gloss
  tempoMenuF, eventoMenuF, renderizarMenuF,
  -- ** Renderização
  renderizarLinha,
  -- ** Reação a eventos
  Acao(..), cliqueLinha, cliqueLinhas, limitarScroll,
  -- ** Comuns
  translateLinha, dentroRegiaoLinhas
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO.Error
import Data.List

import LI12223
import UI_2022li1g012
import FicheiroMapa_2022li1g012
import ErroM_2022li1g012
import MPreEdit_2022li1g012
import Editor_2022li1g012
import {-# SOURCE #-} MenuP_2022li1g012 -- módulos mutuamente recursivos

-- | 'tempoMenuF' reage à passagem do tempo (nenhuma)
tempoMenuF :: Float -> EstadoJogo -> IO EstadoJogo
tempoMenuF _ = return

{-|
  'translateLinha' faz uma translação vertical de uma linha do menu conforme
  o deslocamento vertical (scroll).
-}
translateLinha :: Float                        -- ^ Scroll
               -> (FilePath, Picture, [Botao]) -- ^ Linha
               -> (FilePath, Picture, [Botao]) -- ^ Linha com translação
translateLinha s (fp, p, bts) = let y = s * 16 in
  (fp, Translate 0 y p, map (translateBt 0 y) bts)

{-|
  Uma ação consequente de um clique do rato numa linha (conforme o botão
  pressionado). Ver 'cliqueLinha'.
-}
data Acao = Nenhuma | Apagar | Editar | Jogar deriving (Eq, Show)

{-|
  'cliqueLinha' verifica se o utilizador clicou em algum botão de uma linha,
  devolvendo a 'Acao' correspondente que deve ser executada.
-}
cliqueLinha :: (Float, Float)               -- ^ Posição do cursor
            -> (FilePath, Picture, [Botao]) -- ^ Linha em análise
            -> Acao                         -- ^ Consequência do clique
cliqueLinha p (_, _, [(r1, _), (r2, _), (r3, _)])
  | dentro r1 p = Apagar
  | dentro r2 p = Editar
  | dentro r3 p = Jogar
  | otherwise = Nenhuma
cliqueLinha _ (_, _, _) = Nenhuma -- não é suposto acontecer

{-|
  'cliqueLinhas' verifica se o utilizador carregou num botão das várias linhas
  no menu, alterando o estado de jogo conforme aplicável (não fazer nada,
  apagar mapas, abrir editor ou abrir jogo). Também é devolvida a ação
  efetuada, caso seja necessário.
-}
cliqueLinhas :: EstadoJogo            -- ^ Estado do jogo
             -> IO (Acao, EstadoJogo) -- ^ Estado seguinte e ação executada
cliqueLinhas ej@(EJ (MenuF _ _ [] _) _ _) = return (Nenhuma, ej)
cliqueLinhas ej@(EJ (MenuF p v (x@(fp, _, _):xs) b) f as)
  | a == Nenhuma = (cliqueLinhas $ EJ (MenuF p v xs b) f as) >>= (seguintes x a)
  | a == Editar  = inicializarEditor as True fp >>= (\ x -> return (Editar, x))
  | a == Jogar   = return (Jogar,  ej) -- TODO
  | a == Apagar  = do apg <- apagarMapa fp
                      if apg then inicializarMenuF as v >>=
                                  (\ e -> return (Apagar, e))
                        else inicializarErroM as "Erro ao apagar\n\no mapa :("
                             >>= (\ e -> return (Apagar, e))
  where a = cliqueLinha p $ translateLinha v x
        -- Analisar a linha seguinte. Caso alguma ação seja executada, ignorar
        -- as linhas anteriores e executar essa ação. Senão, continuar com
        -- todas as linhas.
        seguintes x' ac (Nenhuma, (EJ (MenuF p' v' xs' b') f' a')) =
          return $ (ac, EJ (MenuF p' v' (x':xs') b') f' a')
        seguintes _ _ (ac, f) = return (ac, f)

{-|
  'limitarScroll' limita a posição vertical das linhas a valores aceitáveis
  (não passar dos limites e nenhuma linha ser mostrada, por exemplo).
-}
limitarScroll :: [(FilePath, Picture, [Botao])] -- ^ Linhas do menu
              -> Float                          -- ^ Deslocamento vertical
              -> Float                          -- ^ Deslocamento adequado
limitarScroll lns v
  | v < 0     = 0
  | v > maxv  = maxv
  | otherwise = v
  where l = fromIntegral $ length lns
        maxv = if l > 9 then (4 * l - 37) else 0

{-|
  'dentroRegiaoLinhas' verifica se o cursor se encontra numa região onde pode
  clicar nos botões das linhas dos mapas (para não carregar em botões
  escondidos). Também é útil para determinar se se pode mostrar um botão com ou
  sem highlight.
-}
dentroRegiaoLinhas :: EstadoJogo -- ^ Estado de jogo (posição do rato e botões)
                   -> Bool       -- ^ Verdadeiro se na região das linhas
dentroRegiaoLinhas (EJ (MenuF (_, y) _ _ bts) _ _) =
  y >= 32 + (snd $ snd $ fst (bts !! 0)) - 384 && y <= 384 - 72

{-|
  'eventoMenuF' reage ao input do utilizador (movimento do rato e cliques em
  botões).
-}
eventoMenuF :: Event -> EstadoJogo -> IO EstadoJogo
eventoMenuF (EventMotion (x, y)) (EJ (MenuF _ v l bts) f b) =
  return $ EJ (MenuF (x, y) v l bts) f b
eventoMenuF (EventKey (MouseButton LeftButton) Up _ (x, y))
            ej@(EJ (MenuF _ v l bts) _ b)
  | dentro (fst (bts !! 0)) (x, y) = inicializarMPE b
  | dentro (fst (bts !! 1)) (x, y) = inicializarMenu b
  | not $ dentroRegiaoLinhas ej = return ej
  | otherwise = cliqueLinhas ej >>= (return . snd)
eventoMenuF (EventKey (MouseButton WheelUp) Down _ _)
  (EJ (MenuF p v l bts) f b) = return $ EJ (MenuF p v' l bts) f b
  where v' = limitarScroll l (v - 1)
eventoMenuF (EventKey (MouseButton WheelDown) Down _ _)
  (EJ (MenuF p v l bts) f b) = return $ EJ (MenuF p v' l bts) f b
  where v' = limitarScroll l (v + 1)
eventoMenuF _ e = return e

-- | 'renderizarLinha' gera a imagem para renderizar um mapa da lista
renderizarLinha :: EstadoJogo                   -- ^ Estado do jogo
                -> (FilePath, Picture, [Botao]) -- ^ Linha
                -> Picture                      -- ^ Imagem produzida
renderizarLinha e (_, t, bts) = Pictures (t : map (imagemBotao' e) bts)
  -- Não mostrar um botão com highlight se estiver obstruído
  where imagemBotao' e@(EJ (MenuF p _ _ _) _ _) b@(_, (p1,_)) =
          if dentroRegiaoLinhas e then imagemBotao p b else p1

-- | 'renderizarMenuF' é responsável por desenhar o menu no ecrã.
renderizarMenuF :: EstadoJogo -> IO Picture
renderizarMenuF ej@(EJ (MenuF p v l bts) _ b) = return $ Pictures
  (linhas ++ botoes ++ t)
  where t = let ((_, h), t') = mrTexto (fonte b) TCentro "Escolha um mapa"
            in [ Translate 0 (384 - 16 - h * 2.5) $ Color black $
                 rectangleSolid 768 (h * 5 + 32),
                 Translate 0 (384 - 16 - h * 2.5) $ Scale 5 5 t' ]
        botoes = let ((_, (_, h)), _) = bts !! 0
                 in (Translate 0 (-384 + h / 2 + 16) $ Color black $
                    rectangleSolid 768 (h + 32)) : map (imagemBotao p) bts
        linhas = if null l then [Scale 2 2 $ snd $
                   mrTexto (fonte b) TCentro "Sem mapas instalados"]
                   else map (renderizarLinha ej . translateLinha v) l

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
linhasMenu :: BitmapData                     -- ^ Imagem da fonte
           -> [FilePath]                     -- ^ Lista de mapas
           -> [(FilePath, Picture, [Botao])] -- ^ Lista de linhas
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
             (Right m) -> let lns = linhasMenu (fonte a) $ sort m
                              bts = [b1', b2']
                              v' = limitarScroll lns v
                          in return $ EJ (MenuF (0, 0) v' lns bts) funcoesMenuF a
