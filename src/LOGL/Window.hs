module LOGL.Window
(
 createAppWindow, runAppLoop
)
where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.Loops

createAppWindow :: Int -> Int -> String ->IO Window
createAppWindow width height title = do
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 3
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'Resizable False
    mw <- GLFW.createWindow width height title Nothing Nothing
    case mw of
        Nothing -> do
            GLFW.terminate
            error "Could not create GLFW window"
        Just w -> do
            makeContextCurrent mw
            setKeyCallback w $ Just keyCallback
            (width,height) <- getFramebufferSize w
            GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            return w

keyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback w Key'Escape _ KeyState'Pressed _ = setWindowShouldClose w True
keyCallback _ _ _ _ _ = return ()

runAppLoop :: Window -> IO () -> IO ()
runAppLoop win = whileM_ (not <$> windowShouldClose win)
