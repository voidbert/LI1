{- |
Module      : LI12223
Description : Módulo auxiliar para LI1 22/23.
Copyright   : Manuel Barros <d13242@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>
              Humberto Gomes <a104348@alunos.uminho.pt>
              José Lopes <a104541@alunos.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2022/23.
 -}
module LI12223 (
  -- * Tipos de dados
  -- ** Básicos
  Coordenadas , Largura , Velocidade, 
  -- ** Mapas
  Mapa(..), Terreno(..), Obstaculo(..),
  -- ** Jogo
  Jogo(..), Jogador(..), Direcao(..), Jogada(..),
  -- ** Estados de jogo
  Assets(..), DadosJogo(..), FuncoesJogo(..), EstadoJogo(..), Audios
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.BMP
import System.Process

import UI_2022li1g012

-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Show, Read, Eq)

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Show, Read, Eq)

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | O Jogador define o personagem controlado no 'Jogo'.
newtype Jogador =
  Jogador Coordenadas
  deriving (Show, Read, Eq)

-- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)


{-|
  Lista de imagens / sons / outros necessários ao longo do jogo.
-}
data Assets = Assets {
  fonte  :: BitmapData,
  tiles  :: BitmapData,
  balde  :: Picture,
  musica :: Audios
 }

-- | Tipo para audios do jogo.

type Audios = [(FilePath, ProcessHandle)]

{-|
  Dados associados a um 'EstadoJogo', necessários de serem passados de
  atualização em atualização ou de frame em frame.
-}
data DadosJogo = MenuP
                   (Float, Float) -- ^ Posição do rato
                   [Botao]
               | ErroM
                   (Float, Float) -- ^ Posição do rato
                   Picture        -- ^ Mensagem de erro (renderizada)
                   Botao          -- ^ Botão de regresso ao menu
               | MenuF
                   (Float, Float)                 -- ^ Posição do rato
                   Float                          -- ^ Posição vertical
                   [(FilePath, Picture, [Botao])] -- ^ Lista de mapas
                   [Botao]                        -- ^ Botões de baixo
               | MenuPE
                   (Float, Float)
                   [Botao]
                   String
               | Editor
                   (Float, Float) -- ^ Posição do rato
                   FilePath       -- ^ Onde guardar o mapa
                   Mapa           -- ^ Conteúdos do mapa
                   [Botao]        -- ^ Botões de baixo
               | GameOver
                   (Float, Float)
                   [Botao]
                   Picture
               | Play


{-|
  Funções associadas a um 'EstadoJogo', responsáveis pela sua atualização após
  passagem de tempo e eventos do jogador, e pela representação do jogo no ecrã.
-}
data FuncoesJogo = FJ (Float -> EstadoJogo -> IO EstadoJogo) -- ^ Passagem de tempo
                      (Event -> EstadoJogo -> IO EstadoJogo) -- ^ Reação a eventos
                      (EstadoJogo -> IO Picture) -- ^ Renderizar o jogo
        
{-|
  Um 'EstadoJogo' representa um menu ou o o jogo em si. É constituído por dados
  que são transmitidos de atualização para atualização ('DadosJogo') e funções
  responsáveis pela atualização e representação no ecrã ('FuncoesJogo').
  Ademais, a lista de imagens necessárias é também incluída.
-}
data EstadoJogo = EJ DadosJogo   -- ^ Informação a ser mantida entre atualizações
                     FuncoesJogo -- ^ Funções de atualização e renderização
                     Assets      -- ^ Imagens necessárias para o jogo
