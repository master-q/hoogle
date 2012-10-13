
module Web.All(action) where

import CmdLine.All
import General.System
import General.Base
import General.Web
import Web.Server
import Web.Response
import Web.Page
import Network.Wai
import Paths_hoogle


action :: CmdLine -> IO ()
action q@Server{} = server q
action q = do
  f <- readFile' =<< getDataFileName ("resources" </> "template" <.> "html")
  let t = loadTemplates f
  d <- getDataDir
  p <- getEnvVar "PATH_INFO"
  let p' = fromMaybe "" p
  cgiResponse =<< go t d p'
    where
      go t d p | "/res/" `isPrefixOf` p =
        serveFile True $ d </> "resources" </> takeFileName p
      go t d p | "/file/usr/share/doc/" `isPrefixOf` p =
        let p' = if "/" `isSuffixOf` p then p ++ "index.html" else p
        in rewriteRootLinks =<< serveFile False (fromJust (stripPrefix "/file" p'))
      go t _ _ = rewriteRootLinks =<< response responseArgs{templates=t} q

rewriteRootLinks :: Response -> IO Response
rewriteRootLinks = responseRewrite $ foldl1 (.) $ map f p
  where
    p = [("href=\"/", "href=\"/cgi-bin/hoogle/file/") 
        ,("href='file:/", "href='/cgi-bin/hoogle/file/")]
    f (f,t) = lbsReplace (fromString f) (fromString t)
