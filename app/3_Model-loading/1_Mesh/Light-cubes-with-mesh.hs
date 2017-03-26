{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Main where

import LOGL.Window
import LOGL.Texture
import LOGL.Resource
import LOGL.Camera as Cam
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL hiding (normalize, position, Texture)
import Graphics.GLUtil hiding (loadTexture)
import System.FilePath
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Linear.Matrix
import Linear.V3
import Linear.Vector
import Linear.Quaternion
import Linear.Projection
import Linear.Metric
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators hiding (empty)
import LOGL.FRP
import LOGL.Objects
import LOGL.Mesh
import LOGL.Shader
import Control.Monad.Trans.State as St

pointLightPositions :: [V3 GLfloat]
pointLightPositions = [
    V3 0.7  0.2  2.0,
    V3 2.3 (-3.3) (-4.0),
    V3 (-4.0) 2.0 (-12.0),
    V3 0.0 0.0 (-3.0)]

cubePositions :: [V3 GLfloat]
cubePositions = [
    V3 0.0  0.0  0.0,
    V3 2.0  5.0 (-15.0),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2.0) (-12.3),
    V3 2.4 (-0.4) (-3.5),
    V3 (-1.7) 3.0 (-7.5),
    V3 1.3 (-2.0) (-2.5),
    V3 1.5  2.0 (-2.5),
    V3 1.5  0.2 (-1.5),
    V3 (-1.3)  1.0 (-1.5)]

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"

    setCursorInputMode (window w) CursorInputMode'Disabled

    depthFunc $= Just Less

    pm <- updateManager newManager $ do
        loadResource ("lampShader",
            "data" </> "2_Lighting" </> "1_Colors" </> "lamp.vs",
            "data" </> "2_Lighting" </> "1_Colors" </> "lamp.frag")
        loadResource ("lightingShader",
            "data" </> "2_Lighting" </> "6_Multiple-lights" </> "multiple-spot.vs",
            "data" </> "2_Lighting" </> "6_Multiple-lights" </> "multiple-spot.frag")

    tm <- updateManager newManager $ do
        loadResource ("data" </> "2_Lighting" </> "4_Lighting-maps" </> "container2.png")
        loadResource ("data" </> "2_Lighting" </> "4_Lighting-maps" </> "container2_specular.png")

    let appContext = AppContext { shaderMgr = pm, textureMgr = tm }

    let lightingShader = getResource pm "lightingShader"

    currentProgram $= Just (program lightingShader)

    setUniform lightingShader "mat.shininess" (32.0 :: GLfloat)

    setUniform lightingShader "dirLight.direction" (V3 (-0.2 :: GLfloat) (-1.0) (-0.3))
    setUniform lightingShader "dirLight.ambient" (V3 (0.05 :: GLfloat) 0.05 0.05)
    setUniform lightingShader "dirLight.diffuse" (V3 (0.4 :: GLfloat) 0.4 0.4)
    setUniform lightingShader "dirLight.specular" (V3 (0.5 :: GLfloat) 0.5 0.5)

    mapM_ (setPointLight lightingShader) [0..3]

    setSpotLight lightingShader

    contMesh <- cubeMesh [
        Texture (getResource tm "container2") DiffuseMap "diffuse",
        Texture (getResource tm "container2_specular") SpecularMap "specular"]
    lightMesh <- cubeMesh []

    --polygonMode $= (Line, Line)
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            idleE <- idleEvent w
            camB <- createAppCamera w (V3 0.0 0.0 3.0)
            reactWithContext w $ drawScene contMesh lightMesh w <$> (camB <@ idleE)
            -- reactimate $ drawScene pm contMesh lightMesh w <$> (camB <@ idleE)
    runAppLoopEx2 w appContext networkDescription

    deleteMesh lightMesh
    deleteMesh contMesh
    updateManager tm deleteAll

    terminate

drawScene :: Mesh -> Mesh -> AppWindow -> Camera GLfloat -> StateT AppContext IO ()
drawScene contMesh lightMesh w cam = do
    appContext <- St.get
    let pm = shaderMgr appContext
    liftIO $ do
        pollEvents
        clearColor $= Color4 0.1 0.1 0.1 1.0
        clear [ColorBuffer, DepthBuffer]

        Just time <- getTime


        let lightingShader = getResource pm "lightingShader"

        -- draw the container cube
        currentProgram $= Just (program lightingShader)

        setUniform lightingShader "viewPos" (Cam.position cam)

        setUniform lightingShader "spotLight.position" (Cam.position cam)
        setUniform lightingShader "spotLight.direction" (Cam.front cam)

        let view = viewMatrix cam
            projection = perspective (radians (zoom cam)) (800.0 / 600.0) 0.1 (100.0 :: GLfloat)
        setUniform lightingShader "view" view
        setUniform lightingShader "projection" projection

        mapM_ (drawCube contMesh lightingShader) [0..9]

        -- draw the lamp
        let lampShader = getResource pm "lampShader"
        currentProgram $= Just (program lampShader)
        setUniform lampShader "view" view
        setUniform lampShader "projection" projection

        mapM_ (drawLight lightMesh lampShader) [0..3]

        swap w

drawLight :: Mesh -> ShaderProgram -> Int -> IO ()
drawLight mesh shader i = do
    let model = mkTransformationMat (0.2 *!! identity) (pointLightPositions !! i)
    setUniform shader "model" model
    drawMesh mesh shader

setSpotLight :: ShaderProgram -> IO ()
setSpotLight shader = do
    setUniform shader "spotLight.ambient" (V3 (0.0 :: GLfloat) 0.0 0.0)
    setUniform shader "spotLight.diffuse" (V3 (0.5 :: GLfloat) 0.5 0.5)
    setUniform shader "spotLight.specular" (V3 (1.0 :: GLfloat) 1.0 1.0)
    setUniform shader "spotLight.constant" (1.0 :: GLfloat)
    setUniform shader "spotLight.linear" (0.09 :: GLfloat)
    setUniform shader "spotLight.quadratic" (0.032 :: GLfloat)
    setUniform shader "spotLight.cutOff" (cos (radians 12.5) :: GLfloat)
    setUniform shader "spotLight.outerCutOff" (cos (radians 15.0) :: GLfloat)

setPointLight :: ShaderProgram -> Int -> IO ()
setPointLight shader i = do
    setUniform shader ("pointLights[" ++ show i ++ "].position") (pointLightPositions !! i)
    setUniform shader ("pointLights[" ++ show i ++ "].ambient") (V3 (0.05 :: GLfloat) 0.05 0.05)
    setUniform shader ("pointLights[" ++ show i ++ "].diffuse") (V3 (0.5 :: GLfloat) 0.5 0.5)
    setUniform shader ("pointLights[" ++ show i ++ "].specular") (V3 (1.0 :: GLfloat) 1.0 1.0)
    setUniform shader ("pointLights[" ++ show i ++ "].constant") (1.0 :: GLfloat)
    setUniform shader ("pointLights[" ++ show i ++ "].linear") (0.09 :: GLfloat)
    setUniform shader ("pointLights[" ++ show i ++ "].quadratic") (0.032 :: GLfloat)

drawCube :: Mesh -> ShaderProgram -> Int -> IO ()
drawCube mesh shader i = do
    let angle = pi / 180.0 * 20.0 * fromIntegral i
        rot = axisAngle (V3 (1.0 :: GLfloat) 0.3 0.5) (realToFrac angle)
        model = mkTransformation rot (cubePositions !! i)
    setUniform shader "model" model
    drawMesh mesh shader
