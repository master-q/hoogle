
module Hoogle.DataBase.Instances(Instances, saveInstances, loadInstances, hasInstance) where

import System.IO
import Hoogle.TextBase.All
import General.All


data Instances = Instances


saveInstances :: Handle -> TextBase -> IO [Response]
saveInstances hndl tb = return []


loadInstances :: Handle -> IO Instances
loadInstances hndl = return Instances


hasInstance :: Instances -> () -> IO Bool
hasInstance _ _ = return False