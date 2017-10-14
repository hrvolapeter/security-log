{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import           Criterion.Main
import qualified Data.Text              as T
import qualified Data.Text.IO           as TO
import           System.Posix.Directory
import           System.Process
import           System.Unix.Directory


main :: IO ()
main = withTemporaryDirectory "security-log-test" test



test :: FilePath -> IO ()
test _ =
  let dir = "/users/peterhrvola/Desktop" in
  do
  _ <- writeFile (dir ++ "/config.yaml") (unlines [ "tag: Config"
                                        , "input:"
                                        , "  tag: File"
                                        , "  contents: " ++ dir ++ "/stream"
                                        , "output:"
                                        , "  tag: Std"
                                        , "asDaemon: false"
                                        , "serverType: Apache"
                                        ])
  _ <- TO.writeFile (dir ++ "/stream") (T.replicate 100000 "10.5.252.1 - - [22/May/2017:16:07:09 +0200] \"GET /pulp/repos/Govcert/Govcert_Stable/Servery_Centos_7/custom/Epel/epel_7_x86_64/repodata/repomd.xml HTTP/1.1\" 200 2178 \"-\" \"urlgrabber/3.10 yum/3.4.3\"\n")

  defaultMain [ bench "1" $ nfIO (rawSystem "stack" ["exec", "security-log", "--", "--configpath=" ++ dir ++ "/config.yaml"] ) ]
  return ()
