module LOGL.Application.Window
(
    createAppWindow, AppWindow(..), swap,
    keyBehavior, Keys, keyPressed, createAppCamera,
    createTexture, initWindowResize
)
where

import LOGL.Application.Context
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import Reactive.Banana.Frameworks
import Reactive.Banana hiding (empty)
import LOGL.FRP
import Linear.V3
import LOGL.Camera
import Control.Applicative hiding (empty)
import Data.Set as Set hiding (unions)
import Graphics.GLUtil
import Control.Monad.Reader
import Control.Monad.IO.Class


data AppWindow = AppWindow {    title :: String,
                                window :: Window,
                                keyEvent :: MomentIO (Event KeyEvent),
                                winSizeEvent :: MomentIO (Event WindowSizeEvent),
                                cursorPosEvent :: MomentIO (Event CursorPosEvent),
                                scrollEvent :: MomentIO (Event ScrollEvent),
                                idleEvent :: MomentIO (Event ()),
                                fireIdle :: Handler (),
                                ctxEvent :: MomentIO (Event AppContext),
                                fireCtx :: Handler AppContext}

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
            (idleAddHandler, fIdle) <- newAddHandler
            (ctxAddHandler, fCtx) <- newAddHandler
            return AppWindow {  title = t,
                                window = w,
                                keyEvent = keyE,
                                winSizeEvent = winSizeE,
                                cursorPosEvent = cursorPosE,
                                scrollEvent = scrollE,
                                idleEvent = fromAddHandler idleAddHandler,
                                fireIdle = fIdle,
                                ctxEvent = fromAddHandler ctxAddHandler,
                                fireCtx = fCtx}

-- | window resizing functions

initWindowResize :: AppWindow -> MomentIO ()
initWindowResize win = do
    -- react on window resize
    winSizeE <- winSizeEvent win
    reactimate $ handleWinResize <$> winSizeE

handleWinResize :: WindowSizeEvent -> IO ()
handleWinResize (win, width, height) = do
    (width,height) <- getFramebufferSize win
    GL.viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

swap :: AppWindow -> IO ()
swap w = swapBuffers $ window w

-- | functions related to tracking keys

type Keys = Set Key

keyBehavior :: AppWindow -> MomentIO (Behavior Keys)
keyBehavior win = do
    keyE <- keyEvent win
    accumB empty $ unions [handleKeyEvent <$> keyE]

handleKeyEvent ::  KeyEvent -> Keys -> Keys
handleKeyEvent  (w, k, i, KeyState'Pressed, m) keys = Set.insert k keys
handleKeyEvent  (w, k, i, KeyState'Released, m) keys = Set.delete k keys
handleKeyEvent  (w, k, i, _, m) keys = keys

keyPressed :: Key -> Keys -> Bool
keyPressed = member


-- | functions to create a camera for an application window

data CamState = CamState { camera :: Camera GLfloat,
                        lastFrame :: Double,
                        lastX :: GLfloat,
                        lastY :: GLfloat,
                        firstMouse :: Bool}
    deriving (Show)

createAppCamera :: AppWindow -> V3 GLfloat -> MomentIO (Behavior (Camera GLfloat))
createAppCamera w camPos = do
    posE <- cursorPosEvent w
    scrollE <- scrollEvent w
    idleE <- idleEvent w
    timeB <- currentTimeB
    keyB <- keyBehavior w
    stateB <- accumB initState $ unions [
                handleScrollEvent <$> scrollE,
                handlePosEvent <$> posE,
                (doMovement <$> keyB ) <@> (timeB <@ idleE)]
    return $ camera <$> stateB
    where
        initState = CamState {  camera = createCamera camPos (V3 0.0 1.0 0.0) (-90.0) 0.0,
                                    firstMouse = True,
                                    lastX = 400.0,
                                    lastY = 300.0,
                                    lastFrame = 0.0}

doMovement :: Keys -> Double -> CamState -> CamState
doMovement keys time state = state { camera = afterMoveRight , lastFrame = time}
    where
        cam = camera state
        deltaTime = realToFrac $ time - lastFrame state
        upPressed = keyPressed Key'W keys
        downPressed = keyPressed Key'S keys
        leftPressed = keyPressed Key'A keys
        rightPressed = keyPressed Key'D keys
        afterZoomIn = if upPressed then processKeyboard cam ForwardM deltaTime else cam
        afterZoomOut = if downPressed then processKeyboard afterZoomIn BackwardM  deltaTime else afterZoomIn
        afterMoveLeft = if leftPressed then processKeyboard afterZoomOut LeftM  deltaTime else afterZoomOut
        afterMoveRight = if rightPressed then processKeyboard afterMoveLeft RightM  deltaTime else afterMoveLeft

handleScrollEvent :: ScrollEvent -> CamState -> CamState
handleScrollEvent (w, xoffset, yoffset) state = state { camera = processMouseScroll (camera state) (realToFrac yoffset) }

handlePosEvent :: CursorPosEvent -> CamState -> CamState
handlePosEvent (w, xpos, ypos) state = state { lastX = realToFrac xpos, lastY = realToFrac ypos, firstMouse = False,
                                                camera = processMouseMovement cam xoffset yoffset True }
    where
        cam = camera state
        lx = if firstMouse state then realToFrac xpos else lastX state
        ly = if firstMouse state then realToFrac ypos else lastY state
        xoffset =  realToFrac xpos - lx
        yoffset = ly - realToFrac ypos

createTexture :: FilePath -> IO TextureObject
createTexture p = do
    result <- readTexture p
    case result of
        Left s -> error s
        Right t -> do
            textureWrapMode Texture2D S $= (Repeated, Repeat)
            textureWrapMode Texture2D T $= (Repeated, Repeat)
            textureFilter Texture2D $= ((Linear', Nothing), Linear')
            generateMipmap Texture2D $= Enabled
            textureBinding Texture2D $= Nothing
            return t
