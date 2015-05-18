module Main where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

import qualified Halogen.Themes.Bootstrap3 as B

foreign import data Timer :: !

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
data State = State Number

-- | Inputs to the state machine
data Input = Tick | Tick100

ui :: forall m eff. (Applicative m) => Component m Input Input
ui = render <$> stateful (State 0) update
  where
  render :: State -> H.HTML (m Input)
  render (State n) =
    H.div [ A.class_ B.container ]
          [ H.h1 [ A.id_ "header" ] [ H.text "counter" ]
          , H.p_ [ H.text (show n) ]
          , H.button [ A.class_ B.btn
                     , A.onClick (A.input_ $ Tick100) ]
                     [ H.text "+100" ]
          ]

  update :: State -> Input -> State
  update (State n) Tick = State (n + 1)
  update (State n) Tick100 = State (n + 100)

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  setInterval globalWindow 1000 $ driver Tick