{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Network.SimpleIRC
import Data.Maybe
import Data.Time.Clock

onMessage :: MIrc -> IrcMessage -> IO ()
onMessage server message = case msg of
  "> commands" -> sendToUser "my commands are: hello, time, commands."
  "> hello"    -> sendToOrigin ("hi there, " ++ (B.unpack user))
  "> coi"      -> sendToOrigin ("coi " ++ (B.unpack user))
  "> poke"     -> sendToOrigin "ouch!"
  "> time"     -> do
    time <- getCurrentTime
    sendToOrigin ("the time is " ++ (show time))
  _            -> putStrLn (show message)
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
    cChannels = ["#djanbot"], 
    cEvents = [(Privmsg onMessage)]} False True
  return ()