{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Network.SimpleIRC
import Data.Maybe
import Data.Time.Clock

onMessage :: MIrc -> IrcMessage -> IO ()
onMessage server message = case msg of
  "> hello"    -> sendMsg server origin $ B.pack ("hi there, " ++ (B.unpack user))
  "> time"     -> do
    time <- getCurrentTime
    sendMsg server origin $ B.pack ("the time is " ++ (show time))
  "> commands" -> sendMsg server user $ B.pack "my commands are: hello, time, commands."
  _            -> putStrLn (show message)

  where msg = mMsg message
        user = B.drop 1 $ fromJust $ mUser message
        origin = fromJust $ mOrigin message

  
main :: IO ()
main = do
  connection <- connect defaultConfig {
    cAddr = "irc.freenode.net",
    cPort = 6667,
    cNick = "djanbot",
    cUsername = "djanbot",
    cRealname = "djanbot",
    cChannels = ["#djanbot", "#jbopre"], 
    cEvents = [(Privmsg onMessage)]} False True
  return ()