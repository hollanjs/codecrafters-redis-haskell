{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, send, recv, Socket)
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))
import Data.ByteString.Char8 (split, ByteString)


-- RESPsimplestrings		Simple	    +
-- RESPsimpleerrors		    Simple	    -
-- RESPintegers		        Simple	    :
-- RESPbulkstrings		    Aggregate	$
-- RESPnullbulkstrings		Aggregate	$-1\r\n
-- RESParrays		        Aggregate	*
-- RESPnulls		        Simple	    _
-- RESPbooleans		        Simple	    #
-- RESPdoubles		        Simple	    ,
-- RESPbignumbers		    Simple	    (
-- RESPbulkerrors		    Aggregate	!
-- RESPverbatimstrings		Aggregate	=
-- RESPmaps		            Aggregate	%
-- RESPattributes		    Aggregate	|
-- RESPsets		            Aggregate	~
-- RESPpushes		        Aggregate	>




splitAfter :: String -> String -> (String, String)
splitAfter _ "" = ([],[])
splitAfter "" _ = (_,"")
splitAfter delimiter text =
    let (chunk, remaining) = splitAt (length delimiter) text
    in if chunk == delimiter
        then (delimiter, remaining)
        else case text of
            []      -> ([],[])
            (x:xs)  -> let (first, second) = splitAfter delimiter xs
                       in (x:first, second)

splitAfterCRLF = splitAfter '\r\n'

data RArray = RArray
    { len   :: Int
    , txt   :: String
    } deriving (Show)

data RBulkString = BulkString
    { len   :: Int
    , txt   :: String
    } deriving (Show)

breakCRLF :: String a => a -> [a]
breakCRLF "" = []
breakCRLF text = (first, second)
    (first, second) = break (== '\r') text



RESPbulkstring :: string a -> BulkString b




stringBreak :: string -> string -> [string]
stringBreak _ "" = []
stringBreak (x:xs) text do
    () break (== x) xs

RESPbulkstring :: a -> a
RESPbulkstring "0\r\n\r\n" = ""
RESPbulkstring (x:xs) = do
    length <- x
    

RESPparser :: a -> b
RESPparser (x:xs)
    |x=='$'     = RESPbulkstring (map toLower xs)
    |x=='*'     = RESParrays (map toLower xs)
RESPparser _ = []


handleClient :: Socket -> IO ()
handleClient socket = do
    msg <- recv socket 1024
    case msg of
        Just _ -> do
            send socket "+PONG\r\n"
            handleClient socket
        Nothing -> return ()

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    hPutStrLn stderr "Logs from your program will appear here"

    -- Uncomment the code below to pass the first stage stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address
        handleClient socket
        closeSock socket
