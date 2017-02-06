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
            (width,height) <- getFramebufferSize w
            GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            whileM_ (not <$> windowShouldClose w) $ do
                pollEvents
                swapBuffers w
    terminate
