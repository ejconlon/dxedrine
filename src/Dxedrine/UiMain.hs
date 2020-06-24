module Dxedrine.UiMain (main) where

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  pure w # set title "Buttons"
  UI.addStyleSheet w "buttons.css"

  buttons <- mkButtons
  getBody w #+
    [UI.div #. "wrap" #+ (greet ++ map element buttons)]

greet :: [UI Element]
greet =
  [ UI.h1  #+ [string "Hello, Haskell!"]
  , UI.div #+ [string "Try the buttons below, they hover and click."]
  ]

mkButton :: String -> UI (Element, Element)
mkButton name = do
  button <- UI.button #. "button" #+ [string name]
  view   <- UI.p #+ [element button]
  return (button, view)

mkButtons :: UI [Element]
mkButtons = do
  list <- UI.ul #. "buttons-list"

  (button1, view1) <- mkButton button1Title

  on UI.hover button1 $ \_ -> do
    element button1 # set text (button1Title ++ " [hover]")
  on UI.leave button1 $ \_ -> do
    element button1 # set text button1Title
  on UI.click button1 $ \_ -> do
    element button1 # set text (button1Title ++ " [pressed]")
    liftIO $ threadDelay $ 1000 * 1000 * 1
    element list    #+ [UI.li # set html "<b>Delayed</b> result!"]

  (button2, view2) <- mkButton button2Title

  on UI.hover button2 $ \_ -> do
    element button2 # set text (button2Title ++ " [hover]")
  on UI.leave button2 $ \_ -> do
    element button2 # set text button2Title
  on UI.click button2 $ \_ -> do
    element button2 # set text (button2Title ++ " [pressed]")
    element list    #+ [UI.li # set html "Zap! Quick result!"]

  pure [list, view1, view2]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"
