module Arguments.Args where

import System.Console.CmdArgs


data HsArgs = HsArgs
    {threads :: Int
    ,delay :: Int
    ,wordlist :: FilePath
    ,host :: String
    -- dir flags
    ,addslash :: Bool
    ,cookies :: String
    ,extension :: String
    ,headers :: String
    ,proxy :: String
    ,useragent :: String
    ,timeout :: Int
    ,statuscodes :: String
    }
    deriving (Data,Typeable,Show,Eq)

hsargs = HsArgs{
    threads = def &= name "c" &= typ "NUM" &= help "Number of concurrent threads",
    delay = def &= name "D" &= help "Time each thread waits between requests",
    wordlist = def &= name "w" &= help "Path to the wordlist",
    host = def &= name "u" &= help "Host",
    -- dir flags 
    addslash = def &= name "f" &= help "Append / to each request",
    cookies = def &= name "C" &= help "Cookies to use for the requests",
    extension = def &= name "x" &= help "File extension to search for",
    headers = def &= name "H" &= help "Specify HTTP headers",
    proxy = def &= name "p" &= help "Proxy to use for requests [http://host:port]",
    useragent = def &= name "a" &= help "Set the User-Agent string (default \"hsbuter/0.0\")",
    timeout = def  &= help "HTTP Timeout (default 10s)",
    statuscodes  = def &= name "s" &= help "Positive status codes (will be overwritten with statuscodesblacklist if set) (default \"200,204,301,302,307,401,403\")"

}&=
    help "Suggest improvements!" &=
    summary "HsBuster (C) mota" &=
    details ["Hsbuter is a tool used to brute-force web directories.",""]


mode = cmdArgsMode hsargs

get_args = cmdArgs hsargs