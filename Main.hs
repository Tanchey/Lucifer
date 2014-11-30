{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad (when)
import Data.Char (toUpper)
import Text.Read (readMaybe)
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent, command,
    argument, longOption)

main = do
  args <- optionsWithUsageFile "USAGE.txt"
  let portAndMessage = if args `isPresent` longOption "port"
                          then parsePortArg (args `getArg` longOption "port")
                          else (3000, "Starting server on 3000 port.")
  startScotty portAndMessage

parsePortArg :: Maybe String -> (Int, String)
parsePortArg maybePortString = 
  let parsePort :: Maybe Int -> (Int, String)
      parsePort Nothing = (3000, "Port was specified incorrectly. Lucifer will use default server.\nStarting server on 3000 port.")
      parsePort (Just port) = (port, "Starting server on " ++ show port ++ " port.")
  in case maybePortString of
    Nothing -> parsePort Nothing
    (Just portString) -> parsePort $ readMaybe portString


startScotty (port, message) = do 
    putStrLn message
    scotty port $ do
        get "/:word" $ do
            beam <- param "word"
            html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
        notFound (html "¯\\_(ツ)_/¯")



