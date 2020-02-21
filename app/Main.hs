module Main where


import Control.Monad

import System.Console.CmdArgs
import Control.Concurrent
import System.Exit

import Control.Concurrent.STM

import Arguments.Args
import Files.Wordlist
import Requester.Request


import Data.List.Split

import Parser.HttpParser


  

main :: IO ()
main = do
  args <- get_args
  threadsCount <- newTVarIO 0
  let max_threads = if (threads args) == 0 then 10 else (threads args)
  let hostname = (host args)
  let cookie = if (cookies args) == "" then "Nothing" else (cookies args)
  let add_header = if (headers args) == "" then "" else (headers args) ++ "\r\n"
  let wordlist_file = (wordlist args)
  let delay_time = if (delay args) == 0 then 1500 else (delay args) 
  let proxy_settings = getProxy (proxy args)
  let host =if (fst proxy_settings) == "" then hostname else fst proxy_settings
  let port = if (snd proxy_settings) == "" then "80" else snd proxy_settings
  let uagent = if (useragent args) == "" then "hsbuter/0.0" else useragent args
  let timeoutt = if (timeout args) == 0 then 10 else timeout args
  let valid_status_codes = if (statuscodes args) == "" then ["200","204","301","302","307","401","403"] else splitOn "," (statuscodes args)
 
  let request_args  = RequestArgs host port hostname timeoutt delay_time max_threads add_header [("User-Agent",uagent),("Cookie",cookie)] valid_status_codes 
  j_wordlist <- readWordlist wordlist_file -- TODO CHANGE
  case j_wordlist of
    Nothing -> do
      putStrLn "Fail to open the wordlist file." 
      exitFailure
    Just wl -> do
        printBanner request_args  wordlist_file
        let wordlist_extended = map  (\x -> x ++ (extension args) ++ (if (addslash args) then "/"  else "")) wl
        callOrcherstor wordlist_extended threadsCount request_args 

  threadDelay (1000000 * timeoutt)

getProxy :: String -> (String,String)
getProxy hostname = do
    case (getDomain hostname) of
      Nothing -> ("","")
      Just wd -> wd


printBanner :: RequestArgs -> String -> IO ()
printBanner requestargs wordlist_file = do
  putStrLn "========================================"
  putStrLn "HsBuster v0.0                      @mota"
  putStrLn $ "URL:          " ++ "http://" ++ (ra_Hostname requestargs) 
  putStrLn $ "Threads:      " ++ (show (ra_Threads requestargs))
  putStrLn $ "Wordlist:     " ++  wordlist_file
  putStrLn $ "StatusCodes:  " ++  (show (ra_ValidStatusCode requestargs))
  putStrLn $ "Timeout:      " ++ (show (ra_Timeout requestargs)) ++ "s"
  putStrLn "========================================"

  
