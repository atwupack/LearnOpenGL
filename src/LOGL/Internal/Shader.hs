{-# LANGUAGE TypeFamilies #-}
module LOGL.Internal.Shader
(

)
where

import Graphics.GLUtil
import LOGL.Internal.Resource

instance Resource ShaderProgram where
    type LoadParam ShaderProgram = (String, FilePath, FilePath)
    load (name, vfile, ffile) = do
        shader <- simpleShaderProgram vfile ffile
        return (name, shader)
    delete prg = return ()
