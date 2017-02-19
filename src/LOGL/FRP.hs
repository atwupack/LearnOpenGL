module LOGL.FRP
(
    registerKeyboard
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana.Frameworks

registerKeyboard :: Window -> IO (AddHandler (Window, Key, Int, KeyState, ModifierKeys))
registerKeyboard w = do
    (addHandler, fire) <- newAddHandler
    let keyCallback w k i s m = fire (w, k, i, s, m)
    setKeyCallback w $ Just keyCallback
    return addHandler
