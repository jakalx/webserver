{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Webserver implementation based on the book "Sockets-and-Pipes"
module Book where

import Prelude ()

import Relude

import ASCII qualified as A
import ASCII.Char qualified as A
import ASCII.Decimal (Digit (..))

import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO qualified as IO

import Control.Exception.Safe qualified as Ex
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)

import Data.Aeson ((.=))
import Data.Aeson qualified as J

import Data.Attoparsec.ByteString (Parser, (<?>))
import Data.Attoparsec.ByteString qualified as P

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Conversion qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Map.Strict qualified as Map

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Text.Lazy.Encoding qualified as LT

import Data.Time qualified as Time

import List.Transformer (ListT, runListT)
import List.Transformer qualified as ListT

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as HTML

import Unfork (unforkAsyncIO_)

import Network.Socket (Socket)
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

import Network.Simple.TCP (HostPreference (..), serve)
import Network.Simple.TCP qualified as Net

getDataDir :: MonadIO m => m FilePath
getDataDir = do
    dir <- liftIO $ Dir.getXdgDirectory Dir.XdgData "webserver-snp"
    liftIO $ Dir.createDirectoryIfMissing True dir
    pure dir

fileResource :: MonadIO m => FilePath -> IOMode -> ResourceT m (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

binaryResource :: MonadIO m => FilePath -> IOMode -> ResourceT m (ReleaseKey, Handle)
binaryResource path mode =
    allocate (IO.openBinaryFile path mode) IO.hClose

dataFileResource :: MonadIO m => FilePath -> IOMode -> ResourceT m (ReleaseKey, Handle)
dataFileResource path mode = do
    dir <- getDataDir
    fileResource (dir </> path) mode

dataBinaryResource :: MonadIO m => FilePath -> IOMode -> ResourceT m (ReleaseKey, Handle)
dataBinaryResource path mode = do
    dir <- getDataDir
    binaryResource (dir </> path) mode

tryFileResource :: (MonadIO m, Ex.MonadCatch m) => FilePath -> IOMode -> ResourceT m (Either Ex.IOException (ReleaseKey, Handle))
tryFileResource path mode = Ex.tryIO $ fileResource path mode

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    print $ length hs
  where
    openManyHandles :: ResourceT IO [Handle]
    openManyHandles = do
        result <- Ex.tryIO (dataFileResource "greetings.txt" ReadMode)
        case result of
            Right (_, h) -> do
                hs <- openManyHandles
                pure (h : hs)
            Left _ -> do
                pure []

prettyHandle :: MonadIO m => IO.Handle -> m String
prettyHandle h = do
    info <- liftIO $ IO.hShow h
    pure (show h <> ": " <> info)

writeGreeting :: IO ()
writeGreeting = runResourceT @IO do
    (_releaseKey, h) <- fileResource "greetings.txt" WriteMode
    liftIO do
        T.hPutStrLn h "hello"
        T.hPutStrLn h "world"

repeatUntil :: (Monad m, Monoid b) => m a -> (a -> Bool) -> (a -> m b) -> m b
repeatUntil nextChunk isDone f = loop mempty
  where
    loop !acc = do
        chunk <- nextChunk
        if isDone chunk
            then pure acc
            else do
                v <- f chunk
                loop (acc <> v)

repeatUntilNothing :: (Monad m, Monoid b) => m (Maybe a) -> (a -> m b) -> m b
repeatUntilNothing ma action = repeatUntil ma isNothing (maybe (pure mempty) action)

printFileContentsUppercase :: FilePath -> IO ()
printFileContentsUppercase path = runResourceT @IO do
    (_, h) <- dataFileResource path ReadMode
    liftIO $ printCapitalized h

printCapitalized :: Handle -> IO ()
printCapitalized h = repeatUntil (T.hGetChunk h) T.null (T.putStr . T.toUpper)

characterCount :: FilePath -> IO Int
characterCount path = runResourceT @IO do
    (_, h) <- dataFileResource path ReadMode
    liftIO $ getSum <$> repeatUntil (T.hGetChunk h) T.null (pure . Sum . T.length)

copyDataFile :: FilePath -> FilePath -> IO ()
copyDataFile from to = runResourceT @IO do
    (_, hFrom) <- dataBinaryResource from ReadMode
    (_, hTo) <- dataBinaryResource to WriteMode
    liftIO $ repeatUntil (BS.hGetSome hFrom 1024) BS.null (BS.hPutStr hTo)

makeFriend :: S.AddrInfo -> IO ()
makeFriend addr = runResourceT @IO do
    (_, s) <- openAndConnect addr
    liftIO do
        Net.send s $ T.encodeUtf8 "Hello, will you be my friend?"
        repeatUntilNothing (Net.recv s 1024) BS.putStr
        S.gracefulClose s 1000

queryGopher :: S.AddrInfo -> IO ()
queryGopher addr = runResourceT @IO do
    (_, s) <- openAndConnect addr
    liftIO do
        S.sendAll s $ T.encodeUtf8 "\r\n"
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve service host = do
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just service)
    case addrInfos of
        [] -> fail $ "resolve: could not look up host=" <> show host <> " with service=" <> show service
        addr : _ -> pure addr
  where
    hints = S.defaultHints{S.addrSocketType = S.Stream}

openAndConnect :: MonadIO m => S.AddrInfo -> ResourceT m (ReleaseKey, Socket)
openAndConnect addr = do
    (key, s) <- allocate (S.openSocket addr) S.close
    liftIO do
        S.setSocketOption s S.UserTimeout 1000
        S.connect s (S.addrAddress addr)
    pure (key, s)

-- Chapter 6 - HTTP data types
data Request
    = Request RequestLine Headers (Maybe MessageBody)
    deriving (Eq, Show)

data Response
    = Response StatusLine Headers (Maybe MessageBody)
    deriving (Eq, Show)

data RequestLine = RequestLine Method RequestTarget HttpVersion
    deriving (Eq, Show)

newtype Method = Method BS.ByteString
    deriving (Eq, Show)

newtype RequestTarget = RequestTarget BS.ByteString
    deriving (Eq, Show)

data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase
    deriving (Eq, Show)

data StatusCode = StatusCode A.Digit A.Digit A.Digit
    deriving (Eq, Show)

newtype ReasonPhrase = ReasonPhrase BS.ByteString
    deriving (Eq, Show)

type Headers = [HeaderField]

data HeaderField = HeaderField FieldName FieldValue
    deriving (Eq, Show)

newtype FieldName = FieldName BS.ByteString
    deriving (Eq, Show)

newtype FieldValue = FieldValue BS.ByteString
    deriving (Eq, Show)

newtype MessageBody = MessageBody LBS.ByteString
    deriving (Eq, Show)

data HttpVersion = HttpVersion A.Digit A.Digit
    deriving (Eq, Show)

-- Chapter 7 - Encoding
class Encode a where
    encode :: a -> BSB.Builder

instance Encode Request where
    encode = encodeRequest

instance Encode Response where
    encode = encodeResponse

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

http_1_1 :: HttpVersion
http_1_1 = HttpVersion Digit1 Digit1

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.lift crlf

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request requestLine headerFields optionalBody) =
    mconcat
        [ encodeRequestLine requestLine
        , repeatedlyEncode encodeHeaderField headerFields
        , encodeLineEnd
        , optionallyEncode encodeMessageBody optionalBody
        ]

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version) =
    mconcat
        [ encodeMethod method
        , A.lift [A.Space]
        , encodeRequestTarget target
        , A.lift [A.Space]
        , encodeHttpVersion version
        , encodeLineEnd
        ]

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method method) = BSB.byteString method

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget target) = BSB.byteString target

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion major minor) =
    mconcat
        [ [A.string|HTTP/|]
        , A.lift [major]
        , [A.string|.|]
        , A.lift [minor]
        ]

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody body) = BSB.lazyByteString body

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField (HeaderField (FieldName name) (FieldValue value)) = BSB.byteString name <> A.lift [A.Colon, A.Space] <> BSB.byteString value <> encodeLineEnd

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap

encodeHeaders :: Headers -> BSB.Builder
encodeHeaders = (<> encodeLineEnd) . repeatedlyEncode encodeHeaderField

encodeTrailers :: Headers -> BSB.Builder
encodeTrailers = encodeHeaders

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headerFields optionalBody) =
    mconcat
        [ encodeStatusLine statusLine
        , encodeHeaders headerFields
        , optionallyEncode encodeMessageBody optionalBody
        ]

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version statusCode reasonPhrase) =
    mconcat
        [ encodeHttpVersion version
        , A.lift [A.Space]
        , encodeStatusCode statusCode
        , A.lift [A.Space]
        , encodeReasonPhrase reasonPhrase
        , encodeLineEnd
        ]

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.lift [x, y, z]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase reasonPhrase) = BSB.byteString reasonPhrase

-- Chapter 8 - Responding

countHelloAscii :: Natural -> LBS.ByteString
countHelloAscii count =
    BSB.toLazyByteString $
        mconcat
            [ [A.string|Hello!|]
            , encodeLineEnd
            , [A.string|This page has |]
            , case count of
                0 -> [A.string|never been viewed.|]
                1 -> [A.string|been viewed once.|]
                _ -> [A.string|been viewed |] <> A.showIntegralDecimal count <> [A.string| times.|]
            ]

data Status = Status StatusCode ReasonPhrase
    deriving (Eq, Show)

ok :: Status
ok =
    Status
        (StatusCode Digit2 Digit0 Digit0)
        (ReasonPhrase [A.string|OK|])

badRequest :: Status
badRequest =
    Status
        (StatusCode Digit4 Digit0 Digit0)
        (ReasonPhrase [A.string|Bad request|])

notFound :: Status
notFound =
    Status
        (StatusCode Digit4 Digit0 Digit4)
        (ReasonPhrase [A.string|Not found|])

methodNotAllowed :: Status
methodNotAllowed =
    Status
        (StatusCode Digit4 Digit0 Digit5)
        (ReasonPhrase [A.string|Method not allowed|])

serverError :: Status
serverError =
    Status
        (StatusCode Digit5 Digit0 Digit4)
        (ReasonPhrase [A.string|Server error|])

versionNotSupported :: Status
versionNotSupported =
    Status
        (StatusCode Digit5 Digit0 Digit5)
        (ReasonPhrase [A.string|HTTP version not supported|])

status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

contentType :: FieldValue -> HeaderField
contentType = HeaderField (FieldName [A.string|Content-Type|])

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

contentLength :: Word64 -> HeaderField
contentLength len = HeaderField (FieldName [A.string|Content-Length|]) (FieldValue $ BS.toByteString' len)

asciiOk :: LBS.ByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = contentType plainAscii
    len = contentLength . bodyLength $ body
    body = MessageBody str

bodyLength :: MessageBody -> Word64
bodyLength (MessageBody bytes) = fromIntegral . LBS.length $ bytes

sendResponse :: MonadIO m => Socket -> Response -> m ()
sendResponse s = Net.sendLazy s . BSB.toLazyByteString . encodeResponse

stuckCountingServer :: IO ()
stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s (asciiOk (countHelloAscii count))

-- chapter 9 - UTF-8

plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf-8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

countHelloGreeting :: LT.Text
countHelloGreeting = "Hello! \9835"

countHelloMessage :: Natural -> LT.Text
countHelloMessage count = TB.toLazyText $ case count of
    0 -> "This page has never been viewed."
    1 -> "This page has been viewed once."
    _ -> "This page has been viewed " <> TB.decimal count <> " times."

countHelloText :: Natural -> LT.Text
countHelloText count = countHelloGreeting <> "\r\n" <> countHelloMessage count

textOk :: LT.Text -> Response
textOk = textResponse ok mempty

stuckCountingServerText :: IO ()
stuckCountingServerText = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s $ textOk $ countHelloText count

-- HTML encoding

countHelloHtml :: Natural -> Html
countHelloHtml count = HTML.docType <> document
  where
    document = HTML.html $ documentMetaData <> documentBody

    documentMetaData = HTML.head title
    title = HTML.title "Sockets & Pipes Welcome page"

    documentBody = HTML.body $ greeting <> HTML.hr <> hitCounter

    greeting = HTML.p . toHtml $ countHelloGreeting
    hitCounter = HTML.p . toHtml $ countHelloMessage count

htmlOk :: Html -> Response
htmlOk html = Response (status ok) [typ, len] (Just body)
  where
    typ = contentType htmlUtf8
    len = contentLength . bodyLength $ body
    body = MessageBody $ renderHtml html

stuckCountingServerHtml :: IO ()
stuckCountingServerHtml = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s $ htmlOk $ countHelloHtml count

-- JSON encoding

countHelloJson :: Natural -> J.Value
countHelloJson count =
    J.object
        [ "greeting" .= countHelloGreeting
        , "hits"
            .= J.object
                [ "count" .= count
                , "message" .= countHelloMessage count
                ]
        ]

jsonOk :: J.Value -> Response
jsonOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = contentType json
    len = contentLength . bodyLength $ body
    body = MessageBody $ J.encode str

increment :: TVar Natural -> STM Natural
increment var = do
    val <- readTVar var
    let !val' = val + 1
    writeTVar var val'
    pure val

countingServer :: IO ()
countingServer = do
    hitCounter <- atomically $ newTVar @Natural 0
    serve @IO HostAny "8000" \(s, _) -> do
        count <- atomically $ increment hitCounter
        sendResponse s $ jsonOk $ countHelloJson count

-- exercise 27

updateTime :: TVar (Maybe Time.UTCTime) -> Time.UTCTime -> STM (Maybe Time.NominalDiffTime)
updateTime var timePoint = do
    prevTimePoint <- readTVar var
    writeTVar var (Just timePoint)
    pure $ Time.diffUTCTime timePoint <$> prevTimePoint

timingServer :: IO ()
timingServer = do
    timeOfLastRequest <- atomically $ newTVar @(Maybe Time.UTCTime) Nothing
    serve @IO HostAny "8000" \(s, _) -> do
        now <- Time.getCurrentTime
        delta <- atomically $ updateTime timeOfLastRequest now
        sendResponse s $ jsonOk $ J.toJSON delta

-- chapter 11 - Chunks

data Chunk = Chunk ChunkSize ChunkData
    deriving (Eq, Show)
newtype ChunkSize = ChunkSize Natural
    deriving (Eq, Show)
newtype ChunkData = ChunkData BS.ByteString
    deriving (Eq, Show)

dataChunk :: ChunkData -> Chunk
dataChunk chunk = Chunk (chunkDataSize chunk) chunk

chunkDataSize :: ChunkData -> ChunkSize
chunkDataSize (ChunkData chunk) =
    case toIntegralSized @Int @Natural (BS.length chunk) of
        Just n -> ChunkSize n
        _ -> error "BS.length is always >= 0, i.e. Natural"

encodeChunk :: Chunk -> BSB.Builder
encodeChunk (Chunk chunkSize chunkData) =
    mconcat
        [ encodeChunkSize chunkSize
        , encodeLineEnd
        , encodeChunkData chunkData
        , encodeLineEnd
        ]

encodeChunkSize :: ChunkSize -> BSB.Builder
encodeChunkSize (ChunkSize sz) = A.showIntegralHexadecimal A.LowerCase sz

encodeChunkData :: ChunkData -> BSB.Builder
encodeChunkData (ChunkData bs) = BSB.byteString bs

encodeLastChunk :: BSB.Builder
encodeLastChunk = encodeChunkSize (ChunkSize 0) <> encodeLineEnd

transferEncoding :: FieldValue -> HeaderField
transferEncoding = HeaderField (FieldName [A.string|Transfer-Encoding|])

chunked :: FieldValue
chunked = FieldValue [A.string|chunked|]

streamingFileServer :: IO ()
streamingFileServer = do
    serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
        (_, h) <- dataBinaryResource "stream.txt" ReadMode
        sendBSB s (encodeStatusLine (status ok))
        sendBSB s (encodeHeaders [transferEncoding chunked])
        liftIO $
            repeatUntil
                (BS.hGetSome h 1024)
                BS.null
                (sendBSB s . encodeChunk . dataChunk . ChunkData)
        sendBSB s encodeLastChunk
        sendBSB s (encodeTrailers [])

sendBSB :: MonadIO m => Socket -> BSB.Builder -> m ()
sendBSB s = Net.sendLazy s . BSB.toLazyByteString

-- Chapter 12

data StreamingResponse = StreamingResponse StatusLine Headers (Maybe ChunkedBody)

newtype ChunkedBody = ChunkedBody (ListT IO Chunk)
newtype MaxChunkSize = MaxChunkSize Int

hStreamingResponse :: Handle -> MaxChunkSize -> StreamingResponse
hStreamingResponse h maxChunkSize = StreamingResponse statusLine headers (Just body)
  where
    statusLine = status ok
    headers = [transferEncoding chunked]
    body = chunkedBody (hChunks h maxChunkSize)

hChunks :: Handle -> MaxChunkSize -> ListT IO BS.ByteString
hChunks h (MaxChunkSize maxChunkSize) =
    listUntil (BS.hGetSome h maxChunkSize) BS.null

chunkedBody :: ListT IO BS.ByteString -> ChunkedBody
chunkedBody = ChunkedBody . fmap (dataChunk . ChunkData)

encodeStreamingResponse :: StreamingResponse -> ListT IO BS.ByteString
encodeStreamingResponse (StreamingResponse statusLine headerFields maybeBody) =
    asum
        [ selectChunk $ encodeStatusLine statusLine
        , selectChunk $ encodeHeaders headerFields
        , do
            ChunkedBody chunks <- ListT.select @Maybe maybeBody
            asum
                [ do
                    chunk <- chunks
                    selectChunk $ encodeChunk chunk
                , selectChunk encodeLastChunk
                , selectChunk $ encodeTrailers []
                ]
        ]

-- | convert a Builder into a stream of strings
selectChunk :: BSB.Builder -> ListT IO BS.ByteString
selectChunk = ListT.select @[] . LBS.toChunks . BSB.toLazyByteString

sendStreamingResponse :: Socket -> StreamingResponse -> IO ()
sendStreamingResponse s r = runListT do
    bs <- encodeStreamingResponse r
    Net.send s bs

-- exercise 31 -- listUntilIO

-- ListT.unfold ::
--   Monad m => (b -> m (Maybe (a, b))) -> b -> ListT m a

unfoldChunk :: Monad m => (a -> Bool) -> m a -> m (Maybe (a, m a))
unfoldChunk isDone f = do
    chunk <- f
    pure $
        if isDone chunk
            then Nothing
            else Just (chunk, f)

listUntil :: Monad m => m a -> (a -> Bool) -> ListT m a
listUntil nextChunk isDone = ListT.unfold (unfoldChunk isDone) nextChunk

-- execise 32 -- copy greeting file using streams

copyGreetingStream :: IO ()
copyGreetingStream = runResourceT @IO do
    (_, src) <- dataBinaryResource "greeting.txt" ReadMode
    (_, dst) <- dataBinaryResource "greeting2.txt" WriteMode
    liftIO $ hCopy src dst

hCopy :: Handle -> Handle -> IO ()
hCopy src dst = runListT @IO do
    hChunks src (MaxChunkSize 1024) >>= liftIO . BS.hPutStr dst

fileCopyMany :: FilePath -> [FilePath] -> IO ()
fileCopyMany srcPath dstPaths = runResourceT @IO do
    (_, srcHandle) <- binaryResource srcPath ReadMode
    dstHandles <- forM dstPaths \p -> do
        (_, h) <- binaryResource p WriteMode
        pure h
    forM_ dstHandles \dst -> do
        liftIO $ hCopy srcHandle dst

-- Chapter 13 - Parsing

newtype ResourceMap = ResourceMap (Map T.Text FilePath)

resourceMap :: FilePath -> ResourceMap
resourceMap dir =
    ResourceMap $
        Map.fromList
            [ ("/stream", dir </> "stream.txt")
            , ("/read", dir </> "read.txt")
            ]

-- let's define some basic parsers

pSpace :: Parser BS.ByteString
pSpace = P.string $ A.lift [A.Space]

pLineEnd :: Parser BS.ByteString
pLineEnd = P.string $ A.lift crlf

pRequestLine :: Parser RequestLine
pRequestLine = do
    method <- pMethod <?> "Method"
    _ <- pSpace <|> fail "Method should be followed by a single space"
    target <- pRequestTarget <?> "Target"
    _ <- pSpace <|> fail "Target should be followed by a single space"
    version <- pHttpVersion <?> "Version"
    _ <- pLineEnd <|> fail "Version should be followed by an end-of-line"
    pure $ RequestLine method target version

pMethod :: Parser Method
pMethod = Method <$> pToken

pToken :: Parser BS.ByteString
pToken = do
    token <- P.takeWhile isTchar
    when (BS.null token) (fail "tchar expected")
    pure token

isTchar :: Word8 -> Bool
isTchar c =
    any
        ($ c)
        [ isTcharSymbol
        , A.isLetter
        , A.isDigit
        ]

isTcharSymbol :: Word8 -> Bool
isTcharSymbol c = c `elem` tcharSymbols

tcharSymbols :: [Word8]
tcharSymbols =
    A.lift
        [ A.ExclamationMark
        , A.NumberSign
        , A.DollarSign
        , A.PercentSign
        , A.Ampersand
        , A.Apostrophe
        , A.Asterisk
        , A.PlusSign
        , A.HyphenMinus
        , A.FullStop
        , A.Caret
        , A.Underscore
        , A.GraveAccent
        , A.VerticalLine
        , A.Tilde
        ]

pRequestTarget :: Parser RequestTarget
pRequestTarget = do
    target <- P.takeWhile A.isVisible
    when (BS.null target) (fail "vchar expected")
    pure $ RequestTarget target

pHttpVersion :: Parser HttpVersion
pHttpVersion = do
    _ <- P.string [A.string|HTTP/|] <|> fail "Expected HTTP/"
    major <- pDigit <?> "major"
    _ <- P.string $ A.lift [A.FullStop]
    minor <- pDigit <?> "minor"
    pure $ HttpVersion major minor

pDigit :: Parser A.Digit
pDigit = do
    x <- P.anyWord8
    case A.word8ToDigitMaybe x of
        Just d -> pure d
        Nothing -> fail "0-9 expected"

readRequestLine :: MonadIO m => MaxChunkSize -> Socket -> m (P.Result RequestLine)
readRequestLine (MaxChunkSize mcs) s =
    P.parseWith (liftIO $ S.recv s mcs) pRequestLine BS.empty

resourceServer :: IO ()
resourceServer = do
    dir <- getDataDir
    let resources = resourceMap dir
    let maxChunkSize = MaxChunkSize 1024
    serve @IO HostAny "8000" \(s, _) ->
        serveResourceOnce resources maxChunkSize s

serveResourceOnce :: ResourceMap -> MaxChunkSize -> Socket -> IO ()
serveResourceOnce resources maxChunkSize s = runResourceT @IO do
    result <- P.eitherResult <$> readRequestLine maxChunkSize s
    case result of
        Right (RequestLine _ target _) -> do
            case getTargetFilePath resources target of
                Just fp -> do
                    (_, h) <- binaryResource fp ReadMode
                    liftIO $ sendStreamingResponse s $ hStreamingResponse h maxChunkSize
                Nothing ->
                    putStrLn $ "no such resource: " <> show target
        Left err ->
            putStrLn err

getTargetFilePath :: ResourceMap -> RequestTarget -> Maybe FilePath
getTargetFilePath (ResourceMap r) (RequestTarget t) = do
    resource <- A.convertStringMaybe t
    Map.lookup resource r

-- exercise 37 - parse status line

pStatusLine :: Parser StatusLine
pStatusLine = (StatusLine <$> pHttpVersion <* pSpace <*> pStatusCode <* pSpace <*> pReasonPhrase) <?> "StatusLine"

pStatusCode :: Parser StatusCode
pStatusCode = (StatusCode <$> pDigit <*> pDigit <*> pDigit) <?> "StatusCode"

pReasonPhrase :: Parser ReasonPhrase
pReasonPhrase = (ReasonPhrase <$> (pReason <* pLineEnd) <|> fail "Reason may not contain CR or LF") <?> "ReasonPhrase"
  where
    pReason = P.takeWhile (not . isCRorLF)
    isCRorLF c = c == A.lift A.CarriageReturn || c == A.lift A.LineFeed

-- Chapter 14

textResponse :: Status -> Headers -> LT.Text -> Response
textResponse s headers str = Response (status s) (typ : len : headers) (Just body)
  where
    typ = contentType plainUtf8
    len = contentLength . bodyLength $ body
    body = MessageBody $ LT.encodeUtf8 str

newtype LogEvent = LogEvent LT.Text
    deriving (Show, Eq)

data Error = Error (Maybe Response) [LogEvent]
    deriving (Show, Eq)

requestParseError :: String -> Error
requestParseError parseError = Error (pure response) (pure event)
  where
    response = textResponse badRequest [] message
    event = LogEvent message
    message = TB.toLazyText $ "Malformed request: " <> TB.fromString parseError

notFoundError :: Error
notFoundError = Error (pure response) mempty
  where
    response = textResponse notFound [] message
    message = "The droids you are looking for are not here."

versionError :: [HttpVersion] -> Error
versionError supportedVersions = Error (pure response) mempty
  where
    response = textResponse versionNotSupported [allow] LT.empty
    allow = HeaderField (FieldName [A.string|Allow|]) (FieldValue versions)
    versions = BS.intercalate (A.lift [A.Comma, A.Space]) $ map (\(HttpVersion major minor) -> [A.string|HTTP/|] <> A.lift [A.lift major, A.FullStop, A.lift minor]) supportedVersions

methodError :: [Method] -> Error
methodError supportedMethods = Error (pure response) mempty
  where
    response = textResponse methodNotAllowed [allow] LT.empty
    allow = HeaderField (FieldName [A.string|Allow|]) (FieldValue methods)
    methods = BS.intercalate (A.lift [A.Comma, A.Space]) $ map (\(Method m) -> m) supportedMethods

fileOpenError :: FilePath -> Ex.IOException -> Error
fileOpenError fp ex = Error (pure response) (pure event)
  where
    response = textResponse serverError [] "Something went wrong"
    event =
        LogEvent $
            TB.toLazyText $
                mconcat
                    [ "Failed to open file path "
                    , show fp
                    , ": "
                    , TB.fromString $ displayException ex
                    ]

lateError :: Ex.IOException -> Error
lateError ex = Error Nothing (pure event)
  where
    event = LogEvent $ LT.pack $ displayException ex

printLogEvent :: LogEvent -> IO ()
printLogEvent (LogEvent msg) = putLText $ "???: " <> msg <> "\n"

resourceServerX :: IO ()
resourceServerX = do
    dir <- getDataDir
    let resources = resourceMap dir
    let maxChunkSize = MaxChunkSize 1024
    unforkAsyncIO_ printLogEvent \log ->
        serve @IO HostAny "8000" \(s, _) -> do
            result <- serveResourceOnceX resources maxChunkSize s
            case result of
                Left e -> handleError log s e
                Right _ -> pure ()

handleError :: (LogEvent -> IO ()) -> Socket -> Error -> IO ()
handleError log s (Error response events) = do
    traverse_ log events
    traverse_ (sendResponse s) response

serveResourceOnceX :: ResourceMap -> MaxChunkSize -> Socket -> IO (Either Error ())
serveResourceOnceX resources maxChunkSize s = runResourceT @IO $ runExceptT @Error @(ResourceT IO) do
    RequestLine method target version <- ExceptT $ readRequestLineX maxChunkSize s
    _ <- ExceptT $ pure $ requireVersion http_1_1 version
    _ <- ExceptT $ pure $ requireMethodX [Method "GET"] method
    fp <- ExceptT $ pure $ getTargetFilePathX resources target
    (_, h) <- ExceptT $ binaryResourceX fp ReadMode
    let r = hStreamingResponse h maxChunkSize
    ExceptT $ liftIO $ sendStreamingResponseX s r

readRequestLineX :: MonadIO m => MaxChunkSize -> Socket -> m (Either Error RequestLine)
readRequestLineX (MaxChunkSize mcs) s = do
    result <- P.eitherResult <$> P.parseWith (liftIO $ S.recv s mcs) pRequestLine BS.empty
    pure $ either (Left . requestParseError) Right result

binaryResourceX :: (MonadIO m, Ex.MonadCatch m) => FilePath -> IOMode -> ResourceT m (Either Error (ReleaseKey, Handle))
binaryResourceX fp mode = do
    result <- Ex.tryIO $ binaryResource fp mode
    pure $ either (Left . fileOpenError fp) Right result

requireVersion :: HttpVersion -> HttpVersion -> Either Error ()
requireVersion supportedVersion x =
    if x == supportedVersion
        then Right ()
        else Left (versionError [supportedVersion])

requireMethodX :: [Method] -> Method -> Either Error ()
requireMethodX supportedMethods x =
    if x `elem` supportedMethods
        then Right ()
        else Left (methodError supportedMethods)

getTargetFilePathX :: ResourceMap -> RequestTarget -> Either Error FilePath
getTargetFilePathX (ResourceMap r) (RequestTarget t) = do
    resource <- maybe (Left notFoundError) Right $ A.convertStringMaybe t
    case Map.lookup resource r of
        Just fp -> Right fp
        Nothing -> Left notFoundError

sendStreamingResponseX :: Socket -> StreamingResponse -> IO (Either Error ())
sendStreamingResponseX s r = do
    result <- Ex.tryIO $ runListT do
        bs <- encodeStreamingResponse r
        Net.send s bs
    pure $ either (Left . lateError) Right result
