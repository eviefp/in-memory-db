module Main
  ( main,
  )
where

import Server qualified
import Prelude (IO)

main :: IO ()
main = Server.run
