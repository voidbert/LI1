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
Module      : MPreEdit_2022li1g012
Description : Menu pre-editor do modo frogger
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module MPreEdit_2022li1g012 where

import System.IO.Error
import System.FilePath
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12223
import UI_2022li1g012
import ErroM_2022li1g012
import FicheiroMapa_2022li1g012
import Editor_2022li1g012
import {-# SOURCE #-} MenuF_2022li1g012 -- módulos mutuamente recursivos

-- A passagem do tempo não importa para este menu
tempoPE :: Float 
        -> EstadoJogo 
        -> IO EstadoJogo
tempoPE _ = return

{- |
  'adicionarTexto' adiciona um caracter à caixa de texto, se esta não estiver
  ainda cheia.
-}
adicionarTexto :: EstadoJogo
               -> Char
               -> EstadoJogo
adicionarTexto ej@(EJ (MenuPE p bts s) fj a) c
  | length s < 20 = EJ (MenuPE p bts (s ++ [c])) fj a
  | otherwise = ej

{- |
  Esta função muda o estado quando uma tecla é pressionada. Ao pressionar
  qualquer tecla com caracteres, o estado muda para representar o caracter 
  apropriado na tela, construindo uma lista. Se o backspace for pressionado,
  é excluido o ultimo caracter da lista criada até então. 
-}
eventoTeclado :: Event 
              -> EstadoJogo 
              -> IO (EstadoJogo)
eventoTeclado (EventMotion (x, y)) (EJ (MenuPE _ bts s) fj a) 
  = return $ EJ (MenuPE (x, y) bts s) fj a
eventoTeclado (EventKey (MouseButton LeftButton) Up _ (x, y))
  (EJ (MenuPE _ bts s) _ b)
  -- Criar mapa
  | dentro (fst (bts !! 0)) (x, y) = tryIOError diretoriaMapas >>=
       \ z -> case z of (Left _)  ->
                          inicializarErroM b "Falha ao criar\n\no mapa :("
                        (Right d) ->
                          inicializarEditor b False (d </> (s ++ ".map"))
  -- Voltar
  | dentro (fst (bts !! 1)) (x, y) = inicializarMenuF b 0

eventoTeclado (EventKey (Char c) Down _ _) ej@(EJ (MenuPE m bts s) fj a) 
  | c == '\b' = let s' = if null s then s else init s in
      return $ EJ (MenuPE m bts s') fj a
  | otherwise = return $ adicionarTexto ej c
eventoTeclado (EventKey (SpecialKey KeySpace) Down _ _) ej =
  return $ adicionarTexto ej ' '
eventoTeclado _ e = return e 

{- |
  Esta função renderiza o menu com as seguintes Pictures: a lista @s@ que é 
  modificada com a função 'eventoTeclado'; o texto no topo para indicar uma ação
  ao jogador e um conjunto de pictures de dois botões.
-}
renderizarPE :: EstadoJogo 
             -> IO Picture
renderizarPE (EJ (MenuPE p bts s) fj a) = return $ Pictures [
  Color white $ rectangleWire 500 64,
  Scale 3 3 $ snd $ mrTexto (fonte a) TCentro s,
  Translate 0 (384 - h * 2.5 - 16) $ Scale 5 5 $ ndm,
  Pictures $ map (imagemBotao p) bts]
  where ((_, h), ndm) = mrTexto (fonte a) TCentro "Nome do Mapa"

{- |
  O estado inicial do Menu, apresenta as translações dos botões a ser 
  renderizados pela função 'renderizarPE' e todos os parametros iniciais
  do estado de jogo.
-}
inicializarMPE :: Assets 
                -> IO EstadoJogo
inicializarMPE a = return $ EJ (MenuPE (0,0) bts "") funcoesJogoPE a
  where b1@((_, (w1, h1)), _) = scaleBt 2 $ mrBotao (fonte a) "Continuar"
        b2@((_, (w2, h2)), _) = scaleBt 2 $ mrBotao (fonte a) "Voltar"
        b1' = translateBt (384 - w1 / 2 - 16)  (-384 + h1 / 2 + 16) b1
        b2' = translateBt (-384 + w2 / 2 + 16) (-384 + h2 / 2 + 16) b2
        bts = [b1', b2']
        funcoesJogoPE = FJ tempoPE eventoTeclado renderizarPE 

