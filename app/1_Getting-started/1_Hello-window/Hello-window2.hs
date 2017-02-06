module Main where

import Control.Monad.Loops
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL

main :: IO ()
main = do
    GLFW.init
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 3
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'Resizable False
    mw <- createWindow 800 600 "LearnOpenGL" Nothing Nothing
    case mw of
        Nothing -> print "Could not create GLFW window"
        Just w -> do
            makeContextCurrent mw
            setKeyCallback w $ Just keyCallback
            (width,height) <- getFramebufferSize w
            GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            whileM_ (not <$> windowShouldClose w) $ do
                pollEvents
                clearColor $= Color4 0.2 0.3 0.3 1.0
                clear [ColorBuffer]
                swapBuffers w
    terminate

keyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback w Key'Escape _ KeyState'Pressed _ = setWindowShouldClose w True
keyCallback _ _ _ _ _ = return ()
