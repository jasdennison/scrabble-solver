{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Network.Wreq
import qualified Network.Wreq.Session as S
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let boardDesignFilePath = args !! 0
  let boardFilePath = args !! 1
  let tileHand = args !! 2

  boardDesign <- readFile boardDesignFilePath
  board <- readFile boardFilePath

  solveBoard (T.pack boardDesign) (T.pack board) (T.pack tileHand)

solveBoard :: T.Text -> T.Text -> T.Text -> IO ()
solveBoard boardDesign board tileHand =
  S.withSession $ \sess -> do
    let opts = defaults & param "boardDesign" .~ [boardDesign]
                        & param "board" .~ [board]
                        & param "tileHand" .~ [tileHand]
    r <- S.getWith opts sess "http://localhost:8080/solve/"
    B.putStr (r ^. responseBody)

