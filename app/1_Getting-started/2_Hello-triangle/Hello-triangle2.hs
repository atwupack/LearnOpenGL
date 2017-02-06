module Main where

import Data.Array.MArray
import Data.Array.Storable
import Control.Monad.Loops
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL

vertices :: [GLfloat]
vertices = [
    0.5,  0.5, 0.0,  -- Top Right
     0.5, -0.5, 0.0,  -- Bottom Right
    -0.5, -0.5, 0.0,  -- Bottom Left
    -0.5,  0.5, 0.0]   -- Top Left

indices :: [GLuint]
indices = [  -- Note that we start from 0!
        0, 1, 3,   -- First Triangle
        1, 2, 3]    -- Second Triangle



-- Shaders
vertexShaderSource = "#version 330 core\n"
    ++ "layout (location = 0) in vec3 position;\n"
    ++ "void main()\n"
    ++ "{\n"
    ++ "gl_Position = vec4(position.x, position.y, position.z, 1.0);\n"
    ++ "}\n"

fragmentShaderSource = "#version 330 core\n"
    ++ "out vec4 color;\n"
    ++ "void main()\n"
    ++ "{\n"
    ++ "color = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
    ++ "}\n"

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
            prg <- createShaderProgram
            (vao, vbo, ebo) <- createVAO
            --polygonMode $= (Line, Line)
            whileM_ (not <$> windowShouldClose w) $ do
                pollEvents
                clearColor $= Color4 0.2 0.3 0.3 1.0
                clear [ColorBuffer]
                -- Draw our first triangle
                currentProgram $= Just prg
                bindVertexArrayObject $= Just vao
                --drawArrays Triangles 0 3
                drawElements Triangles 6 UnsignedInt nullPtr
                bindVertexArrayObject $= Nothing
                swapBuffers w
            deleteObjectName vao
            deleteObjectName vbo
            deleteObjectName ebo
    terminate

createShaderProgram :: IO Program
createShaderProgram = do
    vShader <- createShader VertexShader
    shaderSourceBS vShader $= packUtf8 vertexShaderSource
    compileShader vShader
    vsuccess <- get $ compileStatus vShader
    print vsuccess
    vlog <- get $ shaderInfoLog vShader
    print vlog
    fShader <- createShader FragmentShader
    shaderSourceBS fShader $= packUtf8 fragmentShaderSource
    compileShader fShader
    fsuccess <- get $ compileStatus fShader
    print fsuccess
    flog <- get $ shaderInfoLog fShader
    print flog
    prg <- createProgram
    attachShader prg vShader
    attachShader prg fShader
    linkProgram prg
    return prg

createVAO :: IO (VertexArrayObject, BufferObject, BufferObject)
createVAO = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <- createVBO
    ebo <- createEBO
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (3*4) nullPtr)
    vertexAttribArray (AttribLocation 0) $= Enabled
    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing
    return (vao, vbo, ebo)

createVBO :: IO BufferObject
createVBO = do
    let ptrsize = toEnum $ size * 4
        size = length vertices
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, size - 1) vertices
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $= (ptrsize, ptr, StaticDraw)
    return vbo

createEBO :: IO BufferObject
createEBO = do
    let ptrsize = toEnum $ size * 4
        size = length indices
    ebo <- genObjectName
    bindBuffer ElementArrayBuffer $= Just ebo
    arr <- newListArray (0, size - 1) indices
    withStorableArray arr $ \ptr -> bufferData ElementArrayBuffer $= (ptrsize, ptr, StaticDraw)
    return ebo


keyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback w Key'Escape _ KeyState'Pressed _ = setWindowShouldClose w True
keyCallback _ _ _ _ _ = return ()
