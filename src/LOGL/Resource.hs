{-# LANGUAGE TypeFamilies #-}
module LOGL.Resource
(
    Resource(..), Manager, newManager, loadResource, deleteResource, updateManager, getResource, deleteAll
)
where

import qualified Data.Map as Map
import Control.Monad.Trans.State as St
import Control.Monad.IO.Class
import Data.Maybe

data Manager a = Manager { cache :: Map.Map String a }

newManager :: (Resource r) => Manager r
newManager = Manager { cache = Map.empty }

updateManager :: (MonadIO m, Resource r) => Manager r -> StateT (Manager r) m () -> m (Manager r)
updateManager mgr action = execStateT action mgr

getResource :: (Resource r) => Manager r -> String -> r
getResource mgr name = fromMaybe (error "Invalid resource") md
    where
        md = Map.lookup name (cache mgr)

loadResource :: (MonadIO m, Resource r) => LoadParam r -> StateT (Manager r) m ()
loadResource file = do
    tm <- St.get
    (name, res) <- liftIO $ load file
    St.put tm { cache = Map.insert name res (cache tm) }

deleteResource :: (MonadIO m, Resource r) => String -> StateT (Manager r) m ()
deleteResource name = do
    tm <- St.get
    let mr = Map.lookup name (cache tm)
    case mr of
        Nothing -> return ()
        Just res -> do
            liftIO $ delete res
            St.put tm { cache = Map.delete name (cache tm) }

deleteAll :: (MonadIO m, Resource r) => StateT (Manager r) m ()
deleteAll = do
    tm <- St.get
    mapM_ (liftIO . delete) (cache tm)
    St.put newManager

class Resource a where
    type LoadParam a :: *
    load :: LoadParam a -> IO (String, a)
    delete :: a -> IO ()
