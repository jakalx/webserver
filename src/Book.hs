{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Conversion qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Text.Lazy.Encoding qualified as LT

import Data.Time qualified as Time

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as HTML

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
    = Request RequestLine [HeaderField] (Maybe MessageBody)
    deriving (Eq, Show)

data Response
    = Response StatusLine [HeaderField] (Maybe MessageBody)
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

-- excercise 16

helloResponse :: Response
helloResponse = Response statusLine [typ, len] (Just (MessageBody "Hello!"))
  where
    statusLine = StatusLine http_1_1 (StatusCode Digit2 Digit0 Digit0) (ReasonPhrase "OK")
    typ = contentType plainAscii
    len = HeaderField (FieldName "Content-Length") (FieldValue "6")

-- Chapter 7 - Encoding

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

encodeHeaders :: [HeaderField] -> BSB.Builder
encodeHeaders = (<> encodeLineEnd) . repeatedlyEncode encodeHeaderField

encodeTrailers :: [HeaderField] -> BSB.Builder
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
textOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = contentType plainUtf8
    len = contentLength . bodyLength $ body
    body = MessageBody $ LT.encodeUtf8 str

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
