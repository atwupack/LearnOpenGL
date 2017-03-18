module LOGL.Texture
(
    createTexture, loadTexture, getTexture, deleteTexture, TextureManager, newTextureManager
)
where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.GLUtil hiding (loadTexture, get)
import Data.Map as Map
import System.FilePath
import Control.Monad.Trans.State as St
import Control.Monad.IO.Class

data TextureManager = TextureManager { cache :: Map String TextureData }

data TextureData = TextureData { tobj :: TextureObject, tname :: String } deriving (Eq, Show)

newTextureManager :: TextureManager
newTextureManager = TextureManager { cache = empty }

updateTextureManager :: (MonadIO m) => TextureManager -> StateT TextureManager m () -> m TextureManager
updateTextureManager tm action = execStateT action tm 

deleteTextureState :: (MonadIO m) => String -> StateT TextureManager m ()
deleteTextureState name = do
    tm <- St.get
    newTm <- liftIO $ deleteTexture tm name
    St.put newTm

deleteTexture :: TextureManager -> String -> IO TextureManager
deleteTexture tm name = do
    case md of
        Nothing -> return tm
        Just td -> do
            deleteObjectName $ tobj td
            return $ tm { cache = delete name (cache tm) }
    where
        md = Map.lookup name (cache tm)

loadTextureState :: (MonadIO m) => FilePath -> StateT TextureManager m ()
loadTextureState tfile = do
    tm <- St.get
    newTm <- liftIO $ loadTexture tm tfile
    St.put newTm

loadTexture :: TextureManager -> FilePath -> IO TextureManager
loadTexture tm tfile = do
    result <- readTexture tfile
    case result of
        Left s -> error s
        Right t -> do
            let td = TextureData { tobj = t, tname = name }
            textureWrapMode Texture2D S $= (Repeated, Repeat)
            textureWrapMode Texture2D T $= (Repeated, Repeat)
            textureFilter Texture2D $= ((Linear', Nothing), Linear')
            generateMipmap Texture2D $= Enabled
            textureBinding Texture2D $= Nothing
            return tm { cache = insert name td (cache tm) }
    where
        name = takeBaseName tfile

getTexture :: TextureManager -> String -> TextureObject
getTexture tm name = case md of
    Just td -> tobj td
    Nothing -> error "Invalid texture"
    where
        md = Map.lookup name (cache tm)

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
