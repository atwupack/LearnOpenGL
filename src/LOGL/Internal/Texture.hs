{-# LANGUAGE TypeFamilies #-}
module LOGL.Internal.Texture
(

)
where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil hiding (loadTexture, get)
import System.FilePath
import LOGL.Internal.Resource

instance Resource TextureObject where
    type LoadParam TextureObject = FilePath
    load file = do
        result <- readTexture file
        case result of
            Left s -> error s
            Right t -> do
                textureWrapMode Texture2D S $= (Repeated, Repeat)
                textureWrapMode Texture2D T $= (Repeated, Repeat)
                textureFilter Texture2D $= ((Linear', Nothing), Linear')
                generateMipmap Texture2D $= Enabled
                textureBinding Texture2D $= Nothing
                return (name, t)
        where
            name = takeBaseName file
    delete = deleteObjectName
