module LOGL.FRP
(
    registerKeyboard, KeyEvent, filterKeyE,
    registerWindowSize, WindowSizeEvent
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana.Frameworks
import Reactive.Banana

type KeyEvent = (Window, Key, Int, KeyState, ModifierKeys)

registerKeyboard :: Window -> IO (AddHandler KeyEvent)
registerKeyboard w = do
    (addHandler, fire) <- newAddHandler
    let keyCallback w k i s m = fire (w, k, i, s, m)
    setKeyCallback w $ Just keyCallback
    return addHandler

type WindowSizeEvent = (Window, Int, Int)

registerWindowSize :: Window -> IO (AddHandler WindowSizeEvent)
registerWindowSize w = do
    (addHandler, fire) <- newAddHandler
    let winSizeCallback w width height = fire (w, width, height)
    setWindowSizeCallback w $ Just winSizeCallback
    return addHandler

filterKeyE :: Event KeyEvent -> Key -> Event KeyEvent
filterKeyE e key = filterE f e
    where
        f :: KeyEvent -> Bool
        f (w, k, i, s, m) = k == Key'Escape && s == KeyState'Pressed
