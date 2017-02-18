module Main where

import Data.Array.MArray
import Data.Array.Storable
import LOGL.Window
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL

vertices :: [GLfloat]
vertices = [
    -- Positions         Colors
     0.5, -0.5, 0.0,  1.0, 0.0, 0.0,   -- Bottom Right
    -0.5, -0.5, 0.0,  0.0, 1.0, 0.0,   -- Bottom Left
     0.0,  0.5, 0.0,  0.0, 0.0, 1.0    -- Top
     ]

-- Shaders
vertexShaderSource = "#version 330 core\n"
    ++ "layout (location = 0) in vec3 position;\n"
    ++ "layout (location = 1) in vec3 color;\n"
    ++ "out vec3 ourColor;\n"
    ++ "void main()\n"
    ++ "{\n"
    ++ "gl_Position = vec4(position, 1.0);\n"
    ++"ourColor = color;\n"
    ++ "}\n"

fragmentShaderSource = "#version 330 core\n"
    ++ "in vec3 ourColor;\n"
    ++ "out vec4 color;\n"
    ++ "void main()\n"
    ++ "{\n"
    ++ "color = vec4(ourColor, 1.0f);\n"
    ++ "}\n"

main :: IO ()
main = do
    GLFW.init
    w <- createAppWindow 800 600 "LearnOpenGL"
    prg <- createShaderProgram
    (vao, vbo) <- createVAO
    runAppLoop w $ do
        pollEvents
        clearColor $= Color4 0.2 0.3 0.3 1.0
        clear [ColorBuffer]
        -- Draw our first triangle
        currentProgram $= Just prg
        bindVertexArrayObject $= Just vao
        drawArrays Triangles 0 3
        bindVertexArrayObject $= Nothing
        swapBuffers w
    deleteObjectName vao
    deleteObjectName vbo
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

createVAO :: IO (VertexArrayObject, BufferObject)
createVAO = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <-createVBO
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) nullPtr)
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float (6*4) (plusPtr nullPtr (3*4)))
    vertexAttribArray (AttribLocation 1) $= Enabled
    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing
    return (vao, vbo)

createVBO :: IO BufferObject
createVBO = do
    let ptrsize = toEnum $ size * 4
        size = length vertices
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, size - 1) vertices
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $= (ptrsize, ptr, StaticDraw)
    return vbo
