
module Web.All(action) where

import CmdLine.All
import General.Base
import General.Web
import Web.Server
import Web.Response
import Web.Page
import Paths_hoogle


action :: CmdLine -> IO ()
action q@Server{} = server q
action q = do
  f <- readFile' =<< getDataFileName ("resources" </> "template" <.> "html")
  let t = loadTemplates f
  cgiResponse =<< response responseArgs{templates=t} q
