module LOGL.Application
(
    runAppLoop, runAppLoopEx, runAppLoopEx2,
    reactWithContext,
    module LOGL.Application.Window,
    module LOGL.Application.Context
)
where

import LOGL.Application.Window
import LOGL.Application.Context
import Reactive.Banana.Frameworks
import LOGL.FRP
import Graphics.UI.GLFW as GLFW
import Control.Monad.Loops
import Reactive.Banana hiding (empty)
import Control.Monad.Reader

runAppLoopEx2 :: AppWindow -> AppContext -> MomentIO () -> IO ()
runAppLoopEx2 win context net = do
    let networkDesc :: MomentIO ()
        networkDesc = do
            -- close windw on ESC
            keyE <- keyEvent win
            let escE = filterKeyPressE keyE Key'Escape
            reactimate $ setWindowShouldClose (window win) True <$ escE
            initWindowResize win
            net
    network <- compile networkDesc
    actuate network
    fireCtx win context
    whileM_ (not <$> windowShouldClose (window win)) $ do
        Just startTime <- getTime
        fireIdle win ()
        Just endTime <- getTime
        let fps = 1.0 / (endTime - startTime)
        setWindowTitle (window win) (title win ++ "(" ++ show fps ++ " fps)")
    pause network

runAppLoopEx :: AppWindow -> MomentIO () -> IO ()
runAppLoopEx win = runAppLoopEx2 win emptyContext

runAppLoop :: AppWindow -> IO () -> IO ()
runAppLoop win loop = do
    let networkDesc :: MomentIO ()
        networkDesc = do
            idleE <- idleEvent win
            reactimate $ loop <$ idleE
    runAppLoopEx win networkDesc

-- | functions for the application context
reactWithContext :: AppWindow -> Event (ReaderT AppContext IO ()) -> MomentIO ()
reactWithContext win event = do
    ctxE <- ctxEvent win
    contextB <- stepper emptyContext ctxE
    reactimate $ (doInContext <$> contextB) <@> event
    where
        doInContext ctx action = runReaderT action ctx
