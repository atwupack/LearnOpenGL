module LOGL.Texture
(
    createTexture
)
where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil hiding (loadTexture, get)
import System.FilePath
import LOGL.Resource

instance Resource TextureObject where
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
