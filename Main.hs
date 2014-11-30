{-# LANGUAGE OverloadedStrings, QuasiQuotes#-}

import Web.Scotty

import Data.Aeson.QQ
import Data.Aeson (Value)

import Control.Monad (when)
import Control.Monad.IO.Class

import Text.Read (readMaybe)

import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command,
    argument, longOption)

import Core


main = do
  args <- optionsWithUsageFile "USAGE.txt"
  let (port, message) = if args `isPresent` longOption "port"
                          then parsePortArg (args `getArg` longOption "port")
                          else (3000, "Starting server on 3000 port.")
  putStrLn message
  startScotty port

parsePortArg :: Maybe String -> (Int, String)
parsePortArg maybePortString = 
  let parsePort :: Maybe Int -> (Int, String)
      parsePort Nothing = (3000, "Port was specified incorrectly. Lucifer will use default server.\nStarting server on 3000 port.")
      parsePort (Just port) = (port, "Starting server on " ++ show port ++ " port.")
  in case maybePortString of
    Nothing -> parsePort Nothing
    (Just portString) -> parsePort $ readMaybe portString


startScotty port =  
    scotty port $ do
-- removes all sequences and observations
-- yep, GET request removes all data on server
-- deal with it (⌐■_■)
        get "/clear" $ do
            liftIO (putStrLn "Clearing all data")
            let result = clear
            liftIO (print result)
            json $ [aesonQQ| {status: "ok", response: "ok"} |]
        post "/sequence/create" $ do
            liftIO (putStrLn "Creating sequence")
            let sequenceCode = createSequence
            liftIO (putStrLn ("Created sequence " ++ sequenceCode))
            json $ [aesonQQ| {status: "ok", response: {sequence: #{sequenceCode}}} |]
        post "/observation/add" $ do
            liftIO (putStrLn "Adding observation")
            let guess@(numbers, (broken0, broken1)) = addObservation "GuaranteedUniqueCode4" "green" ("1010101", "1010101")
            liftIO (putStrLn ("Lucifer's guess is " ++ show guess))
            json $ [aesonQQ| {status: "ok", response: {start: #{numbers}, numbers: [#{broken0}, #{broken1}]}} |]
            
        notFound (html "¯\\_(ツ)_/¯")



