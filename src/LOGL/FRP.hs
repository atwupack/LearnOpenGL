module LOGL.FRP
(
    registerKeyboard, KeyEvent, filterKeyPressE, filterKeyReleaseE,
    registerWindowSize, WindowSizeEvent,
    currentTimeB,
    registerCursorPos, CursorPosEvent,
    registerScroll, ScrollEvent
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana.Frameworks
import Reactive.Banana
import Data.Maybe

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

type CursorPosEvent = (Window, Double, Double)

registerCursorPos :: Window -> IO (AddHandler CursorPosEvent)
registerCursorPos w = do
    (addHandler, fire) <- newAddHandler
    let cursorPosCallback w x y = fire (w, x, y)
    setCursorPosCallback w $ Just cursorPosCallback
    return addHandler

type ScrollEvent = (Window, Double, Double)

registerScroll :: Window -> IO (AddHandler ScrollEvent)
registerScroll w = do
    (addHandler, fire) <- newAddHandler
    let scrollCallback w x y = fire (w, x, y)
    setScrollCallback w $ Just scrollCallback
    return addHandler

filterKeyPressE :: Event KeyEvent -> Key -> Event KeyEvent
filterKeyPressE e key = filterE f e
    where
        f :: KeyEvent -> Bool
        f (w, k, i, s, m) = k == key && s == KeyState'Pressed

filterKeyReleaseE :: Event KeyEvent -> Key -> Event KeyEvent
filterKeyReleaseE e key = filterE f e
    where
        f :: KeyEvent -> Bool
        f (w, k, i, s, m) = k == key && s == KeyState'Released

currentTimeB :: MomentIO (Behavior Double)
currentTimeB = fromPoll $ fromMaybe 0.0 <$> getTime
