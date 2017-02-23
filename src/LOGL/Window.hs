module LOGL.Window
(
 createAppWindow, runAppLoop, AppWindow, swap
)
where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.Loops
import Reactive.Banana.Frameworks
import Reactive.Banana
import LOGL.FRP
import Control.Applicative

data AppWindow = AppWindow { window :: Window, keyEvent :: MomentIO (Event KeyEvent)}

createAppWindow :: Int -> Int -> String ->IO AppWindow
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
            --setKeyCallback w $ Just keyCallback
            (width,height) <- getFramebufferSize w
            GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            keyE <- fromAddHandler <$> registerKeyboard w
            return $ AppWindow w keyE

runAppLoop :: AppWindow -> IO () -> IO ()
runAppLoop win loop = do
    let networkDesc :: MomentIO ()
        networkDesc = do
            keyE <- keyEvent win
            let escE = filterKeyE keyE Key'Escape
            reactimate $ setWindowShouldClose (window win) True <$ escE
    network <- compile networkDesc
    actuate network
    whileM_ (not <$> windowShouldClose (window win)) loop
    pause network

swap :: AppWindow -> IO ()
swap w = swapBuffers $ window w
