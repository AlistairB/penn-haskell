{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

parseMessage :: String -> LogMessage
--parseMessage [] = Unknown ""
parseMessage rawMessage
  | rawMessage == "" ||
    (messageTypeLetter /= 'I' &&
     messageTypeLetter /= 'W' &&
     messageTypeLetter /= 'E'
    ) = Unknown rawMessage
  | otherwise = LogMessage messageType timeStamp message
  where
    wordsList = words rawMessage
    messageTypeLetter = head rawMessage
    messageType = deriveMessageType messageTypeLetter wordsList
    timeStamp = deriveTimeStamp messageType wordsList
    message = deriveMessage messageType wordsList

-- nick - is it better to do these inline without a function?
deriveMessageType :: Char -> [String] -> MessageType
deriveMessageType messageTypeLetter wordsList
  | messageTypeLetter == 'I' = Info
  | messageTypeLetter == 'W' = Warning
  | messageTypeLetter == 'E' = Error (read (wordsList !! 1) :: Int)

-- nick - how to use (.) better..
deriveTimeStamp :: MessageType -> [String] -> Int
deriveTimeStamp (Error _) wordsList = (read (wordsList !! 2)) :: TimeStamp
deriveTimeStamp _ wordsList = (read (wordsList !! 1)) :: TimeStamp

deriveMessage :: MessageType -> [String] -> String
deriveMessage (Error _) wordsList = unwords (drop 3 wordsList)
deriveMessage _ wordsList = unwords (drop 2 wordsList)


parse :: String -> [LogMessage]
parse [] = []
parse messages = processLines (lines messages)
  where
    processLines [] = []
    processLines [x] = [parseMessage x]
    processLines (x:xs) = (parseMessage x) : processLines xs



insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert nlm@(LogMessage _ nts _) (Node ltree elm@(LogMessage _ ets _) rtree)
  | nts >= ets = Node ltree elm (Node Leaf nlm Leaf)
  | nts < ets = Node (Node Leaf nlm Leaf) elm rtree

-- Ex 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf
