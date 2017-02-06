module Shader
(
    LShader, createLShader, use
)
where

import Graphics.Rendering.OpenGL.GL as GL
import Data.ByteString as BS

data LShader = LShader Program

createLShader :: FilePath -> FilePath -> IO LShader
createLShader vsf fsf = do
    vShader <- createShader VertexShader
    vertexShaderSource <- BS.readFile vsf
    shaderSourceBS vShader $= vertexShaderSource
    compileShader vShader
    vsuccess <- get $ compileStatus vShader
    print vsuccess
    vlog <- get $ shaderInfoLog vShader
    print vlog
    fShader <- createShader FragmentShader
    fragmentShaderSource <- BS.readFile fsf
    shaderSourceBS fShader $= fragmentShaderSource
    compileShader fShader
    fsuccess <- get $ compileStatus fShader
    print fsuccess
    flog <- get $ shaderInfoLog fShader
    print flog
    prg <- createProgram
    attachShader prg vShader
    attachShader prg fShader
    linkProgram prg
    return $ LShader prg

use :: LShader -> IO ()
use (LShader prg) = currentProgram $= Just prg
