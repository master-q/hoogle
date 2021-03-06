
module Link(link) where

import Util

link :: [String] -> IO ()
link xs = do
    let ys = xs ++ ["ghc" | "base" `elem` xs]
    deps <- mapM (\x -> do d <- dependencies x; return (x, ys `intersect` d)) ys
    mapM_ (\d -> convert d (fromJust $ lookup d deps)) $ order deps


convert :: String -> [String] -> IO ()
convert hoo dep = hoogle_ $
    ["/convert=result/" ++ hoo, hooFlag "output" hoo] ++
    [hooFlag "data" d | d <- dep]


dependencies :: String -> IO [String]
dependencies x = do
    src <- readFile $ "result/" ++ x ++ ".txt"
    return [d
        | x <- takeWhile (\x -> null x || "--" `isPrefixOf` x || "@" `isPrefixOf` x) $ lines src
        , ("@depends ",d) <- [splitAt 9 x]]


order :: (Show a, Eq a) => [(a,[a])] -> [a]
order = f []
    where
        f done xs | null xs = []
                  | null now = error $ "Link.order, circular: " ++ show (done,xs)
                  | otherwise = now2 ++ f (now2++done) later
            where (now,later) = partition (\x -> null $ snd x \\ done) xs
                  now2 = map fst now
