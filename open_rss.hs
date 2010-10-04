{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Console.CmdArgs
import Data.Maybe(isJust)
import Data.Char(isSpace)
import System.IO(stderr)
import System.Exit(exitFailure)
import System.Cmd(system)
import Text.ParserCombinators.Parsec
import System.Log.Logger (rootLoggerName, emergencyM, debugM, setHandlers, updateGlobalLogger, setLevel, Priority(..))
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Handler.Log4jXML(log4jFileHandler')
import Text.HTML.Yuuko(yuuko)
import Network.URI(parseURI)
import qualified Codec.Binary.Base64                    as Base64
import qualified Codec.Binary.UTF8.String               as UTF8
import qualified Text.ParserCombinators.Parsec.Rfc2822  as Rfc2822

-- name of this type is displayed by cmdargs as the app name, so it cannot be Config:/
data Open_rss = Open_rss { verbose :: Bool
                         , logging :: Bool
                         , logFile :: Maybe FilePath
                         , browser :: String
                         } deriving (Data, Typeable, Show)

config = cmdArgsMode $ Open_rss
         { verbose = False     &= help "verbose output, implies logging"
         , logging = False     &= help "enable logging"
         , logFile = Nothing   &= help "log to XML (Log4j format) file (instead of stderr), implies logging" &= typ "PATH"
         , browser = "firefox" &= help "name/path of the browser, defaults to firefox"
         } &= summary open_rss_summary

open_rss_summary =
  unlines [ "open_rss reads email message produced by rss2email from stdin,"
          , "and opens the rss destination link in your web browser."
          , "RSS feeds, encoded in email message, should be piped by your mail reader"
          , "into open_rss."
          ]

loggerName = "open_rss"

debug = debugM loggerName

setUpLogging Open_rss{..} = do
  case logFile of
    Nothing | verbose   -> setUpDefaultLogger DEBUG
            | logging   -> setUpDefaultLogger WARNING
            | otherwise -> disableDefaultLogger
    Just path -> do disableDefaultLogger; setUpXmlLogger path $ if verbose then DEBUG else WARNING

setUpXmlLogger path level = do
  h <- log4jFileHandler' path level
  updateGlobalLogger loggerName $ setLevel level
  updateGlobalLogger loggerName $ setHandlers [h]

setUpDefaultLogger level = do
  h <- streamHandler stderr level
  updateGlobalLogger rootLoggerName $ setLevel level
  updateGlobalLogger rootLoggerName $ setHandlers [h]

disableDefaultLogger = updateGlobalLogger rootLoggerName $ setHandlers ([] :: [GenericHandler a])

getRawContent :: Rfc2822.Message -> String
getRawContent (Rfc2822.Message _ x) = x

-- replace \n with \r\n, careful not to replace already correct newlines
newLinesUnix2EMail :: String -> String
newLinesUnix2EMail ('\r':'\n':xs) = "\r\n" ++ newLinesUnix2EMail xs
newLinesUnix2EMail (     '\n':xs) = "\r\n" ++ newLinesUnix2EMail xs
newLinesUnix2EMail (        x:xs) =       x : newLinesUnix2EMail xs
newLinesUnix2EMail             [] = []

removeNewLines = filter $ \c -> not $ c `elem` "\r\n"

decodeContent rawContent =
    case Base64.decode $ removeNewLines rawContent of
      Nothing -> debug "Message isn't Base64 encoded" >> return rawContent
      Just x  -> debug "Decoding message from Base64" >> return (UTF8.decode x)

exitBecause s = emergencyM loggerName s >> exitFailure

open Open_rss{..} url = system $ browser ++ " " ++ url ++ " > /dev/null 2> /dev/null &"

main = do
  config <- cmdArgsRun config
  setUpLogging config
  -- wanderlust dumps unix newlines, whereas hsemail expects regular, email line endings
  rawMessage <- newLinesUnix2EMail <$> getContents
  case parse Rfc2822.message "" rawMessage of
    Left e -> exitBecause $ show e
    Right m -> do
         let rawContent = getRawContent m
         when (all isSpace rawContent) $ exitBecause "Empty message body"
         content <- decodeContent rawContent
         let results = yuuko "//p[child::text() = 'URL: ']/a/@href" content
         case results of
           [url] -> if isJust $ parseURI url then
                       open config url
                   else
                       exitBecause $ "Found something, that doesn't look like URI: " ++ url
           _ -> do
                debug $ "Message content:\n" ++ content
                case results of
                  [] -> exitBecause "Coudln't find RSS URL"
                  _ ->  exitBecause $ "Found too many urls:\n" ++ unlines results
