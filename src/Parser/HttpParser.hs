module Parser.HttpParser where


import Text.ParserCombinators.ReadP as PC
import Control.Applicative
import Data.Char


data DataResponse = DataResponse{
    d_Protocol :: String,
    d_StatusCode :: String,
    d_Description :: String,
    d_Headers :: Maybe [(String, String)]
    } deriving Show

data DataRequest = DataRequest{
    dr_Method :: String,
    dr_URI :: String,
    dr_Protocol :: String,
    dr_extraHeader :: String,
    dr_Headers :: Maybe [(String, String)]
    } deriving Show


buildRequest :: DataRequest -> String
buildRequest datareq = 
    (dr_Method datareq) ++ " " ++ 
    (dr_URI datareq) ++ " " ++ 
    (dr_Protocol datareq) ++ "\r\n" ++
    (dr_extraHeader datareq) ++
    (buildRequestHeader datareq) ++ "\r\n"


buildRequestHeader :: DataRequest -> String
buildRequestHeader datareq = case (dr_Headers datareq) of
    Nothing -> ""
    Just headers -> buildRequestHeaderIntern headers

buildRequestHeaderIntern :: [(String, String)] -> String
buildRequestHeaderIntern [] = ""
buildRequestHeaderIntern (header:remaining) = 
    (fst header) ++ ": " ++(snd header) ++ "\r\n" ++
    (buildRequestHeaderIntern remaining)


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

getResponse  = parseMaybe parserResponse  

getHeader  = parseMaybe parserGetHeader 

getDomain  = parseMaybe parseURL 

parseURL :: ReadP (String,String)
parseURL = do
    retorn<-(do
        string "http://"
        dom <- domain
        satisfy (\char -> (char == ':'))
        port <- text
        satisfy (\char -> (char == '/'))
        return (dom,port)
       )
        
    return retorn


toLowerString :: String -> String
toLowerString str = [ toLower x | x <- str] 

getHeaderValue :: [(String,String)] -> String -> Maybe String
getHeaderValue [] key = Nothing
getHeaderValue ((first,second):remaining)  key = if (toLowerString (first))==(toLowerString key) then
                                            Just $ second
                                        else
                                            getHeaderValue remaining key



parserGetHeader :: ReadP String
parserGetHeader = do
    header <- wildCard
    crlf
    crlf
    return  header


parserResponse :: ReadP DataResponse
parserResponse = do
  protocol <- word
  withSpace
  status_code <- word
  withSpace
  description <- text
  crlf
  headers_list <- parserHeader
  
  let dataresponse =  DataResponse (protocol) (status_code) (description) Nothing 
  return $ dataresponse {d_Headers=(Just headers_list)}

parserHeader :: ReadP ([(String, String)])
parserHeader = do
    headers_list <- (do
        hl1 <- parserHeaderInt
        hl2 <- parserHeader
        return $ [hl1] ++ hl2
        ) <|> (do
        hl1 <- parserHeaderInt
        crlf
        return $ [hl1] 
        )
        <|> (do
        crlf
        crlf
        return $ [] 
        )
    return headers_list

parserHeaderInt :: ReadP (String, String)
parserHeaderInt = do
    header_name <- word
    satisfy (\char -> (char == ':'))
    header_content <- text
    crlf
    return (header_name,header_content) 


crlf :: ReadP String
crlf = string "\r\n"

word :: ReadP String
word = many1(withoutSpace)

wildCard :: ReadP String
wildCard = PC.many (satisfy (\char -> True ))

text :: ReadP String
text = many1 (satisfy (\char -> not (char == '\r') && not (char == '\n')))

domain :: ReadP String
domain = many1 (satisfy (\char -> not (char == ':') && not (char == '/') && not (char == '\r') && not (char == '\n')))

withoutSpace :: ReadP Char
withoutSpace =
    satisfy (\char -> not (char == ' '))

withSpace :: ReadP Char
withSpace =
    satisfy (\char -> char == ' ')