{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import System.Console.Haskeline
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.List (find)
import Data.Maybe
import System.IO

data Options = Options
    { port      :: FilePath
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { port = def &= help "serial port"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Serial terminal" &=
    summary "Serial v0.0.0, (c) Bengt Marten Agren 2019" &=
    details [ "This is a simple serial terminal"
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    print opts
    h <- hOpenSerial port defaultSerialSettings { commSpeed = CS115200 }
    forever $ do
        hGetLine h >>= putStrLn
        hFlush stdout

