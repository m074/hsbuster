module Requester.Request where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception 
import Network.Simple.TCP
import Data.ByteString.Char8 as BSU 


import Parser.HttpParser

data RequestArgs = RequestArgs{
     ra_Host :: String, -- direccion del server o proxy
     ra_Port :: String,
     ra_Hostname :: String,
     ra_Timeout :: Int,
     ra_Delay :: Int,
     ra_Threads :: Int,
     ra_extraHeader :: String,
     ra_Headers :: [(String, String)],
     ra_ValidStatusCode :: [String]
    } deriving Show


checkThread :: Int -> TVar Int -> Int -> STM Bool
checkThread id tvar_tc  max_threads= do
  tc <- readTVar  tvar_tc
  if (id - tc - max_threads)< 0 then 
    return True
  else
    return False

countThread :: TVar Int -> STM ()
countThread tvar_tc = do
  tc <- readTVar  tvar_tc
  writeTVar tvar_tc (tc + 1)
  

forkThread :: IO () -> IO ((MVar Bool),ThreadId)
forkThread proc = do
    handle <- newMVar True
    thread_id <- forkFinally proc (\_ -> putMVar handle False)
    return (handle,thread_id)

dispatcher :: TVar Int -> String -> RequestArgs -> IO ()
dispatcher  tvar_tc uri request_args = do
  res <- forkThread $ requester tvar_tc uri request_args
  let thread_id = snd res
  let handle = fst res
  threadDelay $ (ra_Timeout request_args) * 1000000
  thread_not_ended <- readMVar handle
  if thread_not_ended then

    atomically $ check False
  else
    atomically $ countThread tvar_tc
  killThread thread_id


requester :: TVar Int -> String -> RequestArgs -> IO ()
requester tvar_tc uri request_args = do
    httpRequest uri request_args 
    threadDelay $ (ra_Delay request_args) * 100 -- en milisegundos
    atomically $ countThread tvar_tc

getBSUString :: Maybe BSU.ByteString -> BSU.ByteString
getBSUString x = case x of
    Nothing -> BSU.pack ""
    Just x -> x


httpRequest :: String -> RequestArgs  ->  IO ()
httpRequest uri request_args = connect (ra_Host request_args) (ra_Port request_args)  $ \(socket, remoteAddr) -> do
  let request = BSU.pack $ buildRequest $ DataRequest "GET" ("http://" ++ (ra_Hostname request_args) ++"/" ++ uri)  "HTTP/1.1" (ra_extraHeader request_args) (Just ([("Host",(ra_Hostname request_args))] ++ (ra_Headers request_args)))
  send socket $ request
  the_header <-receiveHead socket ""
  case (getResponse the_header) of
    Nothing -> do
        return ()
    Just response -> do
          let to_print = "/" ++ uri ++ " (Status: " ++ (d_StatusCode response) ++ ")"
          if Prelude.elem (d_StatusCode response) (ra_ValidStatusCode request_args) then
              case (d_Headers response) of
                Nothing -> do
                  Prelude.putStrLn $ to_print
                Just headers_list -> do
                  Prelude.putStrLn $ to_print ++ (getLength headers_list)
          else
            return ()


getLength :: [(String,String)]  -> String
getLength the_header = do
    case (getHeaderValue the_header "Content-Length") of
      Nothing -> ""
      Just content2 -> " Length:" ++ content2

receiveHead socket remaining_string = do
  receive <-recv socket 1024
  let casted_receive = BSU.unpack $ getBSUString receive
  let the_header = remaining_string ++ casted_receive
  case (getHeader the_header) of
    Nothing -> receiveHead socket the_header
    Just head -> do
      return the_header


callOrcherstor :: [String]  ->  TVar Int -> RequestArgs -> IO ()
callOrcherstor wordlist tvar_tc request_args =  orchestor wordlist 0 tvar_tc request_args

orchestor :: [String] -> Int ->  TVar Int -> RequestArgs -> IO ()
orchestor [] id tvar_tc request_args  = do return ()
orchestor (word:remaining) id tvar_tc request_args = do 
  check_thread <- atomically $ checkThread id tvar_tc (ra_Threads request_args)
  if check_thread then
    do
      forkIO $ dispatcher tvar_tc word request_args
      orchestor remaining (id + 1) tvar_tc request_args
      return ()
  else
    do
      threadDelay $ (ra_Delay  request_args) * 100 
      orchestor (word:remaining) id tvar_tc request_args
      return ()

