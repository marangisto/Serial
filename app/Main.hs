{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import System.Console.Haskeline
import System.Process (system)
import System.Directory
import System.Random
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Extra
import Control.Monad
import Data.List (stripPrefix)
import Data.Maybe
import Data.IORef
import System.IO

data Options = Options
    { port      :: FilePath
    , dir       :: Maybe FilePath
    , midi      :: Bool
    , speed     :: Double
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { port = def &= help "serial port"
    , dir = def &= help "working directory"
    , midi = def &= help "midi-mode"
    , speed = 1.0 &= help "relative speed"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Serial terminal" &=
    summary "Serial v0.1.0, (c) Bengt Marten Agren 2019, 2020" &=
    details [ "This is a simple serial terminal"
            ]

type Line = (Int, B.ByteString)
type GCRef = ([Line], [Line])

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    whenJust dir setCurrentDirectory
    -- hSetNewlineMode stdout noNewlineTranslation
    print opts
    let initial = do
            ref <- newIORef ([], [])
            case files of
                []          -> return ref
                (file:[])   -> do { load ref file; return ref }
                _           -> error "multi-file load not yet supported"
    withSerial port defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
--        liftIO $ threadDelay 100000
        getSerial port >>= putStr . B.unpack
        processInput opts port =<< liftIO initial

processInput :: Options -> SerialPort -> IORef GCRef -> IO ()
processInput opts port gcref = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop | midi opts = getInputLine "> " >>= \x -> case x of
              Nothing -> quit
              Just cmd | Just x <- stripPrefix ":" cmd -> if x == "q" then quit else do
                  e <- liftIO $ try $ localCommand opts port gcref x
                  either (\(SomeException x) -> liftIO $ print x) return e
                  loop
              Just cmd -> do
                  liftIO $ send port $ B.pack $ cmd ++ "\n"
                  res <- liftIO $ getSerial port
                  outputStr $ B.unpack res
                  loop
               | otherwise = do
                res <- liftIO $ getSerial port
                outputStr $ B.unpack res
                loop
          quit = do
              outputStr "quitting..."
              --liftIO $ threadDelay 1000000
              return ()

localCommand :: Options -> SerialPort -> IORef GCRef -> String -> IO ()
localCommand opts port gcref cmd = case cmd of
    _ | Just fp <- stripPrefix "l " cmd -> load gcref fp
    _ | Just dir <- stripPrefix "cd " cmd -> setCurrentDirectory dir
    "pwd"       -> getCurrentDirectory >>= putStrLn
    "s"         -> void $ singleStep opts port gcref
    "r"         -> void $ iterateWhile id $ singleStep opts port gcref
    "x"         -> reset port
    "rew"       -> modifyIORef' gcref $ \(h, t) -> (t ++ h, [])
    "a1"        -> void $ autoTune opts port 1
    "a2"        -> void $ autoTune opts port 2
    "a3"        -> void $ autoTune opts port 3
    "a4"        -> void $ autoTune opts port 4
    "a5"        -> void $ autoTune opts port 5
    "a6"        -> void $ autoTune opts port 6
    "a7"        -> void $ autoTune opts port 7
    "a8"        -> void $ autoTune opts port 8
    "c1"        -> void $ calibrate opts port 1
    "c2"        -> void $ calibrate opts port 2
    "c3"        -> void $ calibrate opts port 3
    "c4"        -> void $ calibrate opts port 4
    "c5"        -> void $ calibrate opts port 5
    "c6"        -> void $ calibrate opts port 6
    "c7"        -> void $ calibrate opts port 7
    "c8"        -> void $ calibrate opts port 8
    "c"         -> mapM_ (clear opts port) [1..8]
    ('!':str)   -> void $ system str
    _           -> error "unrecognized gcs command"

getOneLine :: SerialPort -> IO B.ByteString
getOneLine port = loop where
    loop = do
        x <- recv port 1
        case B.unpack x of
           "\n" -> return x
           _ -> B.append x <$> loop

getSerial :: SerialPort -> IO B.ByteString
getSerial port = loop where
    loop = do
        x <- recv port 256
        if B.null x then return B.empty else B.append x `liftM` loop

bang :: String -> IO ()
bang = void . system

load :: IORef GCRef -> FilePath -> IO ()
load gcref fp = do
    gcode <- B.lines . B.filter (/='\r') <$> B.readFile fp
    mapM_ (putStrLn . B.unpack) $ take 10 gcode
    putStrLn $ show (length gcode) ++ " lines"
    writeIORef gcref (zip [1..] gcode, [])

singleStep :: Options -> SerialPort -> IORef GCRef -> IO Bool
singleStep opts port gcref = readIORef gcref >>= \gc -> case gc of
    ([], _) -> return False
    ((x@(i, s):xs), ys) -> do
        let gcode = B.unpack s
        putStr $ show i ++ "\t" ++ gcode
        hFlush stdout
        if midi opts
        then do
            let (ts:ws) = words gcode
                seconds = read ts
            when (seconds > 0) $ liftIO $ threadDelay $ round $ seconds * 1e6 / speed opts
            send port $ B.pack $ unwords ws <> "\n"
        else do
            send port $ B.snoc s '\n'
        res <- liftIO $ getOneLine port
        putStr $ pad 40 (B.length s) ++ B.unpack res
        writeIORef gcref (xs, ys ++ [x])
        return True

reset :: SerialPort -> IO ()
reset port = do
    liftIO $ send port $ B.pack "\^X"
    res <- liftIO $ getSerial port
    putStr $ B.unpack res

pad :: Int -> Int -> String
pad w l = replicate (max (w - l) 0) ' '

calibrate :: Options -> SerialPort -> Int -> IO Bool
calibrate opts port chan = do
    let xs = zip [1..] $ concat $ replicate 10
            [ B.pack $ "0.1 " <> show (90 + chan) <> " 93 127"
            , B.pack $ "0 " <> show (80 + chan) <> " 93 0"
            , B.pack $ "0.1 " <> show (90 + chan) <> " 45 127"
            , B.pack $ "0 " <> show (80 + chan) <> " 45 0"
            ]
    gcref <- newIORef (xs, [])
    iterateWhile id $ singleStep opts port gcref

autoTune :: Options -> SerialPort -> Int -> IO Bool
autoTune opts port chan = do
    rs <- mapM (const $ (+57) . (*12) <$> randomRIO (-2, 5)) [1..200]
    let xs = zip [1..] $ concatMap (play chan) rs
    gcref <- newIORef (xs, [])
    iterateWhile id $ singleStep opts port gcref
    where play :: Int -> Int -> [B.ByteString]
          play chan key = 
            [ B.pack $ "0.05 " <> show (90 + chan) <> " " <> show key <> " 127"
            , B.pack $ "0 " <> show (80 + chan) <> " " <> show key <> " 0"
            ]

clear :: Options -> SerialPort -> Int -> IO Bool
clear opts port chan = do
    let xs = zip [1..] [ B.pack $ "0 " <> show (80 + chan) <> " 57 0" ]
    gcref <- newIORef (xs, [])
    iterateWhile id $ singleStep opts port gcref

