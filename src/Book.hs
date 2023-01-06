{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

-- | Webserver implementation based on the book "Sockets-and-Pipes"

module Book where

import Prelude ()
import Relude

import ASCII qualified as A
import ASCII.Char qualified as A
import ASCII.Decimal (Digit(..))

import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO qualified as IO

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Control.Exception.Safe qualified as Ex

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T

import Network.Socket (Socket)
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

import Network.Simple.TCP (serve, HostPreference(..))
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
  where openManyHandles :: ResourceT IO [Handle]
        openManyHandles = do
          result <- Ex.tryIO (dataFileResource "greetings.txt" ReadMode)
          case result of
            Right (_, h) -> do
              hs <- openManyHandles
              pure (h:hs)
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
  where loop !acc = do
          chunk <- nextChunk
          if isDone chunk then
             pure acc
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
    addr:_ -> pure addr
  where hints = S.defaultHints { S.addrSocketType = S.Stream }

openAndConnect :: MonadIO m => S.AddrInfo -> ResourceT m (ReleaseKey, Socket)
openAndConnect addr = do
  (key, s) <- allocate (S.openSocket addr) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1000
    S.connect s (S.addrAddress addr)
  pure (key, s)

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

-- HTTP data types

data Request =
  Request RequestLine [HeaderField] (Maybe MessageBody)
  deriving (Eq, Show)

data Response =
  Response StatusLine [HeaderField] (Maybe MessageBody)
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
httpVersion :: HttpVersion
httpVersion = HttpVersion Digit1 Digit1

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start = RequestLine (Method [A.string|GET|]) (RequestTarget "/hello.txt") httpVersion
    host = HeaderField (FieldName "Host") (FieldValue "www.example.com")
    lang = HeaderField (FieldName "Accept-Language") (FieldValue "en, mi")
