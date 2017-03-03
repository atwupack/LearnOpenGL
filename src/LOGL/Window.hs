module LOGL.Window
(
    createAppWindow, runAppLoop, AppWindow, swap, runAppLoopEx, idleEvent, keyEvent, cursorPosEvent,
    window, scrollEvent
)
where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.Loops
import Reactive.Banana.Frameworks
import Reactive.Banana
import LOGL.FRP
import Control.Applicative

data AppWindow = AppWindow {    title :: String,
                                window :: Window,
                                keyEvent :: MomentIO (Event KeyEvent),
                                winSizeEvent :: MomentIO (Event WindowSizeEvent),
                                cursorPosEvent :: MomentIO (Event CursorPosEvent),
                                scrollEvent :: MomentIO (Event ScrollEvent),
                                idleEvent :: MomentIO (Event ()),
                                fireIdle :: Handler ()}

createAppWindow :: Int -> Int -> String ->IO AppWindow
createAppWindow width height t = do
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 3
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'Resizable True
    mw <- GLFW.createWindow width height t Nothing Nothing
    case mw of
        Nothing -> do
            GLFW.terminate
            error "Could not create GLFW window"
        Just w -> do
            makeContextCurrent mw
            (width,height) <- getFramebufferSize w
            GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            keyE <- fromAddHandler <$> registerKeyboard w
            winSizeE <- fromAddHandler <$> registerWindowSize w
            cursorPosE <- fromAddHandler <$> registerCursorPos w
            scrollE <- fromAddHandler <$> registerScroll w
            (addHandler, fire) <- newAddHandler
            return AppWindow {  title = t,
                                window = w,
                                keyEvent = keyE,
                                winSizeEvent = winSizeE,
                                cursorPosEvent = cursorPosE,
                                scrollEvent = scrollE,
                                idleEvent = fromAddHandler addHandler,
                                fireIdle = fire}

runAppLoopEx :: AppWindow -> MomentIO () -> IO ()
runAppLoopEx win net = do
    let networkDesc :: MomentIO ()
        networkDesc = do
            -- close windw on ESC
            keyE <- keyEvent win
            let escE = filterKeyPressE keyE Key'Escape
            reactimate $ setWindowShouldClose (window win) True <$ escE
            -- react on window resize
            winSizeE <- winSizeEvent win
            reactimate $ handleWinResize <$> winSizeE
            net
    network <- compile networkDesc
    actuate network
    whileM_ (not <$> windowShouldClose (window win)) $ do
        Just startTime <- getTime
        fireIdle win ()
        Just endTime <- getTime
        let fps = 1.0 / (endTime - startTime)
        setWindowTitle (window win) (title win ++ "(" ++ show fps ++ " fps)")
    pause network

runAppLoop :: AppWindow -> IO () -> IO ()
runAppLoop win loop = do
    let networkDesc :: MomentIO ()
        networkDesc = do
            idleE <- idleEvent win
            reactimate $ loop <$ idleE
    runAppLoopEx win networkDesc

handleWinResize :: WindowSizeEvent -> IO ()
handleWinResize (win, width, height) = do
    (width,height) <- getFramebufferSize win
    GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

swap :: AppWindow -> IO ()
swap w = swapBuffers $ window w
