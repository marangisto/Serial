{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import Control.Monad
import Data.List (find)
import Data.Maybe
import System.IO

data Options = Options
    { schema_version    :: Bool
    , interrupt         :: Bool
    , files             :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { schema_version = def &= help "Show schema version"
    , interrupt = def &= help "Generate interrupt vector table"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate device descriptions from SVD files" &=
    summary "SVD2CPP v0.0.0, (c) Bengt Marten Agren 2018-2019" &=
    details [ "SVD2CPP generated device header files for ARM-based"
            , "MCUs based on vendor SVD files (see CMSIS)."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    print opts

