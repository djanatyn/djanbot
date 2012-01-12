{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Network.SimpleIRC.Messages as M
import Control.Applicative
import Network.SimpleIRC
import Data.Maybe
import Control.Monad
import Data.Time.Clock

getTime :: IO String
getTime = do
  time <- getCurrentTime
  return ("the current time is " ++ (show time))
  
onMessage :: MIrc -> IrcMessage -> IO ()
onMessage server message = case msg of
  "> commands" -> sendToUser "my commands are: hello, time, commands."
  "> hello"    -> sendToOrigin ("hi there, " ++ (B.unpack user))
  "> coi"      -> sendToOrigin ("coi " ++ (B.unpack user))
  "> poke"     -> sendToOrigin "ouch!"
  "> time"     -> sendToOrigin =<< getTime
  _  -> putStr ((show user) ++ " -- ") >> putStrLn (show msg) >> putStr "> "
  where user = fromJust $ mNick message
        msg  = mMsg message
        sendToOrigin = sendMsg server (fromJust $ mOrigin message) . B.pack
        sendToUser   = sendMsg server user . B.pack
  
main :: IO ()
main = do
  connection <- connect defaultConfig {
    cAddr = "irc.freenode.net",
    cPort = 6667,
    cNick = "djanbot",
    cUsername = "djanbot",
    cRealname = "djanbot",
    cChannels = ["#djanbot","#jbopre"], 
    cEvents = [(Privmsg onMessage)]} True False
  case connection of
    Right server  -> forever $ do
      putStr "> "
      input <- getLine
      sendMsg server "#jbopre" (B.pack input)
    Left server -> putStrLn "failure"
  return ()
