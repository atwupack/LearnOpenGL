module Main where

import Data.Array.MArray
import Data.Array.Storable
import Foreign.Ptr
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL as GL
import LOGL.Application

vertices = [-0.5, -0.5, 0.0,
     0.5, -0.5, 0.0,
     0.0,  0.5, 0.0]

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
    w <- createAppWindow 800 600 "LearnOpenGL"
    prg <- createShaderProgram
    (vao, vbo) <- createVAO 9 vertices
    runAppLoop w $ do
        pollEvents
        clearColor $= Color4 0.2 0.3 0.3 1.0
        clear [ColorBuffer]
        -- Draw our first triangle
        currentProgram $= Just prg
        bindVertexArrayObject $= Just vao
        drawArrays Triangles 0 3
        bindVertexArrayObject $= Nothing
        swap w
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

createVAO :: Int -> [GLfloat] -> IO (VertexArrayObject, BufferObject)
createVAO size elems = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <-createVBO size elems
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float (3*4) nullPtr)
    vertexAttribArray (AttribLocation 0) $= Enabled
    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing
    return (vao, vbo)

createVBO :: Int -> [GLfloat] -> IO BufferObject
createVBO size elems = do
    let ptrsize = toEnum $ size * 4
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, size - 1) elems
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $= (ptrsize, ptr, StaticDraw)
    return vbo
