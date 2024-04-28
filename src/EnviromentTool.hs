module EnviromentTool where

import Control.Concurrent.Chan
import Control.Concurrent.Async
import Data.IORef
import Control.Monad.Reader
import Control.Lens
import Data.Aeson.Lens
import Myai
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Text.IO as T
import Control.Monad.Trans.Cont

data TTSItem = Item Text | Done deriving (Eq,Show)
type ChanTTS = (Chan TTSItem,IORef Text)

newChanTTS :: IO ChanTTS
newChanTTS = do
    chan <- newChan
    ref <- newIORef ""
    pure (chan,ref)

use' :: MonadReader e m => Getting a e a -> m a
use' getting = asks (^. getting)

pushToken :: (MonadReader ChanTTS m,MonadIO m) => TTSItem -> m ()
pushToken text = do
    chan <-  use' _1
    liftIO $ writeChan chan text

runTTS :: MonadIO m => ReaderT ChanTTS m a -> m ()
runTTS tts = do
    chanTTS@(chan,ref) <- liftIO $ newChanTTS
    asyncRead <-  liftIO $  async $  evalContT $ do
        liftIO $ putStrLn "asyn read thread"
        (recur,_) <- createLabel ""
        let parseItem Done = pure ()
            parseItem (Item text) = do
                let chunks = T.split (flip T.elem ".?!:\n\r") text
                let lastText = last chunks
                if T.null lastText then do
                    let sentence = T.concat chunks
                    if T.null sentence then pure ()
                    else do
                        liftIO $ T.putStrLn $ T.concat chunks
                else do
                    liftIO $ T.putStrLn $ T.concat $ init $ chunks
                    liftIO $ T.putStrLn $ "last " <> last chunks
                recur ""
        item <- liftIO $ readChan chan
        parseItem item
        liftIO $ putStrLn "asyn read thread end"
    _ <- runReaderT tts chanTTS
    liftIO $ wait asyncRead
    pure ()


mydata = A.object [
        "model" <-- "gpt-3.5-turbo" ,
        "temperature" <-- 0 ,
        "messages" <-- [
            A.object [
                "role" <-- "system",
                "content" <-- "给我出三道英文题"
            ]
        ]
    ]

runAI :: (MonadAI m, MonadIO m) => m ()
runAI = chatStream mydata $ runTTS $  do
    (recur,vals,_) <- lift $ recurValues ""
    forM_ vals $ runReaderT $ do
        val <- ask
        let token = maybe "" id $ val ^? key "choices" . nth 0 . key "delta" . key "content" . _JSON'
        liftIO $ T.putStr token
        lift $ pushToken (Item token)

    lift $ recur ""
    liftIO $ putStrLn ""