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
Module      : UI_2022li1g012
Description : Ferramentas para User Interface no jogo
Copyright   : José António Fernandes Alves Lopes <a104541@alunos.uminho.pt>
              Humberto Gil Azevedo Sampaio Gomes <a104348@alunos.uminho.pt>
-}
module UI_2022li1g012 (
  -- * Funções expostas
  -- ** Renderização de texto
  Alinhamento(..), mrTexto, dourado,
  -- ** UI
  -- *** Geral
  Retangulo, dentro,
  -- *** Botões
  Botao, ImagensBotao, rBotao, rBotaoT, mrBotao, gerarBotoesEsp,
  translateBt, scaleBt,

  -- * Funções auxiliares
  -- ** Renderização de texto
  renderizarCaracter, adicionarCaracter, mrLinha, alinharLn,
  -- ** Botões
  gerarBotoes
  ) where

import Graphics.Gloss
import Data.Maybe
import Data.List

-- | Alinhamento de texto (à esquerda, centrado, ou à direita)
data Alinhamento = TEsquerda | TCentro | TDireita deriving (Show, Eq, Enum)

{-|
  A função dourado altera todos os dígitos numa string para os seus
  equivalentes fullwidth, que são representados a dourado por
  'renderizarCaracter'.
-}
dourado :: String -> String
dourado = map aux
  where transform c = toEnum (fromEnum c - fromEnum '0' + 65296)
        aux c = if '0' <= c && c <= '9' then transform c else c

{-|
  'renderizarCaracter' devolve as dimensões de um caracter, juntamente com uma
  imagem (caso a fonte contenha o caracter). A imagem é extraída de uma
  'BitmapData', na região correspondente ao caracter, calculada por esta
  função.

  === Nota

  A fonte em uso tem glifos de dimensão 8x8 (px), daí o uso deste número ao
  longo da implementação da função.
-}
renderizarCaracter :: BitmapData             -- ^ Imagem da fonte
                   -> Char                   -- ^ Caracter do qual virá a imagem
                   -> (Float, Maybe Picture) -- ^ Tamanho e imagem do carac.
renderizarCaracter _ ' '  = (8, Nothing)
renderizarCaracter _ '\t' = (8 * 4, Nothing)
renderizarCaracter _ '\n' = (0, Nothing) -- não suportado aqui
renderizarCaracter b c
  -- Caracteres ASCII neste intervalo estão ordenados na imagem
  | '!' <= c && c <= '~' = let (y, x) = (fromEnum c - 33) `divMod` 20
                               rect = Rectangle (x * 8, (9 - y) * 8) (8, 8)
                             in (8, Just $ BitmapSection rect b)
  -- Números dourados
  | '\65296' <= c && c <= '\65305' = let c' = fromEnum c - 65296
                                         rect = Rectangle (c' * 8, 0) (8, 8)
                                       in (8, Just $ BitmapSection rect b)
  -- Caracteres especiais
  | isJust especial = let (Just i) = especial
                          (y, x) = i `divMod` 20
                          rect = Rectangle (x * 8, (3 - y) * 8) (8, 8)
                        in (8, Just $ BitmapSection rect b)
  -- Caracteres não reconhecidos
  | otherwise = (8, Nothing)
  where especial = findIndex (== c) "ÇçÁáÀàÂâÃãÉéÊêÍíÓóÔôÕõÚúºª"

{-|
  'adicionarCaracter' adiciona uma imagem resultante de 'renderizarCaracter'
  a uma lista de imagens. A imagem será sujeita a uma translação horizontal
  (para os caracteres do texto não ficarem sobrepostos). Esta função é
  utilizada em 'mrLinha'.
-}
adicionarCaracter :: Float         -- ^ Translação horizontal (em píxeis)
                  -> Maybe Picture -- ^ Possível imagem de 'renderizarCaracter'
                  -> [Picture]     -- ^ Lista inicial de imagens
                  -> [Picture]     -- ^ Lista final de imagens
adicionarCaracter _ Nothing  ps  = ps
adicionarCaracter x (Just p) ps = (Translate x 0 p) : ps

{-|
  'mrLinha' mede e renderiza uma linha de texto (devolve uma imagem do gloss).
  Texto com várias linhas não é suportado.
-}
mrLinha :: BitmapData       -- ^ Imagem da fonte
        -> String           -- ^ Texto a ser medido e renderizado
        -> (Float, Picture) -- ^ Dimensão horizontal (em px) e imagem da linha
mrLinha b s = let (xy, pcts) = (foldl aux (0, []) s) in (xy, Pictures pcts)
  where aux (x, ps) c = let (dx, p) = renderizarCaracter b c
                               in (x + dx, adicionarCaracter x p ps)

{-|
  'alinharLn' é responsável pela translação horizontal e vertical de cada linha
  no processo de renderização de texto.

  Esta função também corrige a posição do texto em relação à origem do
  referencial. Normalmente, o centro do primeiro caracter e a origem coincidem,
  mas após esta função, a origem coincidirá com o centro do bloco de texto.
-}
alinharLn :: Alinhamento      -- ^ Alinhamento a ser utilizado
          -> Float            -- ^ Largura da linha mais longa
          -> Float            -- ^ Altura do texto
          -> Float            -- ^ Número da atual linha
          -> (Float, Picture) -- ^ Linha a ser processada
          -> Picture          -- ^ Linha com a translação correta
alinharLn a mw mh n (w, ps) = Translate (dx a - mw / 2) (dy + mh / 2) ps
  where dx TEsquerda = 4
        dx TCentro   = ((mw - w) / 2 + 4)
        dx TDireita  = (mw - w + 4)
        dy = (-8) * n - 4

{-|
  'mrTexto' mede e renderiza texto (devolve uma imagem do gloss).
-}
mrTexto :: BitmapData                -- ^ Imagem da fonte
        -> Alinhamento               -- ^ Esquerda, Centro ou Direita
        -> String                    -- ^ Texto a ser medido e renderizado
        -> ((Float, Float), Picture) -- ^ Dimensões e imagem do texto
mrTexto b a s = ((w, h), Pictures final)
  where lns = lines s
        rnd = map (mrLinha b) lns
        w = fst $ maximumBy (\ (x1, _) (x2, _) -> compare x1 x2) rnd
        h = fromIntegral (8 * length lns)
        final = map (uncurry $ alinharLn a w h) $ zip [0..] rnd


{-|
  Um 'Retangulo' é constituído pela posição do seu centro e pelo seu tamanho.
-}
type Retangulo = ((Float, Float), (Float, Float))

-- | 'dentro' verifica se um ponto (como o cursor) se encontra num retângulo.
dentro :: Retangulo
       -> (Float, Float)
       -> Bool
dentro ((xc, yc), (w, h)) (x, y) = xi <= x && x <= xf && yi <= y && y <= yf
  where xi = xc - w / 2
        yi = yc - h / 2
        xf = xc + w / 2
        yf = yc + h / 2

{-|
  Imagens associadas a um 'Botao'. A primeira imagem é a que deve ser usada
  para o estado normal do botão, enquanto a segunda deve ser usada quando o
  ponteiro do rato está dentro da área do botão.
-}
type ImagensBotao = (Picture, Picture)

{-|
  Um 'Botao' é um componente de interface do utilizador (UI). Todos os botões
  seguem o mesmo grafismo: fundo ciano (mais claro quando o rato está por
  cima), contornos brancos e cantos arredondados. Um botão é composto pela
  área que ocupa e pelas imagens usadas na sua renderização.
-}
type Botao = (Retangulo, ImagensBotao)


{-|
  'rBotao' renderiza um botão que contém uma imagem (normalmente proveniente da
  renderização de texto, ver 'mrTexto').

  === Nota

  As imagens do botão têm mais 16 píxeis de largura e altura que o tamanho da
  imagem dada (primeiro argumento).
-}
rBotao :: (Float, Float) -- ^ Dimensões da imagem no botão
       -> Picture        -- ^ Imagem dentro do botão
       -> Botao          -- ^ Botão final
rBotao (w, h) p = (((0, 0), (w + 16, h + 16)),
                   (Pictures [Color corNormal fundo, contornos, p],
                    Pictures [Color corHover  fundo, contornos, p]))
  where corNormal = makeColorI 00 140 140 255
        corHover  = makeColorI 00 190 190 255

        -- Contornos do botão
        arc' n = Color white $ ThickArc (90 * n) (90 * (n + 1)) 8 2
        hl = Color white $ rectangleSolid w 2
        vl = Color white $ rectangleSolid 2 h
        contornos = Pictures [
          Translate (-w / 2) (-h / 2) $ arc' 2,
          Translate (-w / 2) (h / 2)  $ arc' 1,
          Translate (w / 2)  (h / 2)  $ arc' 0,
          Translate (w / 2)  (-h / 2) $ arc' 3,
          Translate 0 (-8 - h / 2) hl, Translate 0 (8 + h / 2) hl,
          Translate (-8 - w / 2) 0 vl, Translate (8 + w / 2) 0 vl
          ]

        -- Fundo do botão (retângulos no centro e círculos nos cantos)
        rc = rectangleSolid w (h + 16)
        rl = rectangleSolid 8 h
        c  = circleSolid    8
        fundo = Pictures [
          rc,
          Translate (-4 - w / 2) 0 rl,
          Translate (4  + w / 2) 0 rl,

          Translate (-w / 2) (-h / 2) c,
          Translate (-w / 2) (h / 2)  c,
          Translate (w / 2)  (h / 2)  c,
          Translate (w / 2)  (-h / 2) c
          ]

{-|
  'rBotaoT' renderiza um botão com texto. No entanto, força que os conteúdos do
  botão tenham um tamanho determinado. Assim, o botão terá mais 16 píxeis de
  largura e de altura que o tamanho providenciado.
-}
rBotaoT :: BitmapData     -- ^ Imagem da fonte
        -> (Float, Float) -- ^ Tamanho dos conteúdos do botão
        -> String         -- ^ Texto do botão
        -> Botao          -- ^ Botão resultante
rBotaoT b s = rBotao s . snd . mrTexto b TCentro

{-|
  'mrBotao' renderiza um botão com o tamanho mínimo necessário para conter um
  dado texto, devolvendo o seu tamanho as imagens para o renderizar.
-}
mrBotao :: BitmapData -- ^ Imagem da fonte
        -> String     -- ^ Texto do botão
        -> Botao      -- ^ Tamanho e imagens do botão
mrBotao b s = rBotao (w, h) t
  where ((w, h), t) = mrTexto b TCentro s

{-|
  'translateBt' faz a translação de um botão por um vetor (do retângulo da sua
  área e das imagens em si).
-}
translateBt :: Float -- ^ Translação x
            -> Float -- ^ Translação y
            -> Botao -- ^ Botão inicial
            -> Botao -- ^ Botão após a transformação
translateBt dx dy (((x, y), s), (p1, p2)) = (((x + dx, y + dy), s),
  (Translate dx dy p1, Translate dx dy p2))

{-|
  'scaleBt' aumenta o tamanho de um botão por uma escala (do retângulo e das
  imagens).
-}
scaleBt :: Float -- ^ Escala horizontal e vertical
        -> Botao -- ^ Botão inicial
        -> Botao -- ^ Botão final
scaleBt s ((p, (w, h)), (p1, p2)) = ((p, (w * s, h * s)),
  (Scale s s p1, Scale s s p2))


{-|
  'gerarBotoes' gera vários botões com texto, todos com o mesmo tamanho, o
  necessário para os conteúdos do maior texto caber no seu botão.
-}
gerarBotoes :: BitmapData -- ^ Imagem da fonte
            -> [String]   -- ^ Texto de cada botão
            -> [Botao]    -- ^ Tamanho e imagens de cada botão
gerarBotoes b txts = map (rBotaoT b (w - 16, h - 16)) txts
  where tamanhos = map (snd . fst) $ map (mrBotao b) txts
        w = maximum $ map fst tamanhos
        h = maximum $ map snd tamanhos

{-|
  'gerarBotoesEsp' gera vários botões com texto, tal como 'gerarBotoes'. No
  entanto, um espaçamento vertical entre os botões é adicionado (por exemplo,
  para uso num menu).
-}
gerarBotoesEsp :: BitmapData -- ^ Imagem da fonte
               -> Float      -- ^ Espaçamento vertical (em px) entre botões
               -> Float      -- ^ Escala dos botões e do espaçamento
               -> [String]   -- ^ Texto de cada botão
               -> [Botao]    -- ^ Botões produzidos
gerarBotoesEsp _ _ _ [] = [] -- evitar função parcial
gerarBotoesEsp b e s txts = bts'
  where bts = map (scaleBt s) $ gerarBotoes b txts
        th = (sum $ map (snd . snd . fst) bts) + e * s * fromIntegral (length bts - 1)
        (w, h) = snd $ fst $ (bts !! 0)
        bts' = map (\ (i, bt) -> translateBt 0 (th / 2 - (h + e * s) * i - h / 2) bt) (zip [0..] bts)

