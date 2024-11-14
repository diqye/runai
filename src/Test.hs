module Test where
import           Myai                (runAIT, useOllamaRequest, usePost) -- 
import           Myai.Data.Config                                        -- 
import           Myai.Data.Ollama    (Ollama(Ollama, _baseURL))          -- 
import           Control.Monad.Trans (MonadIO(liftIO))                   -- mtl-2.3.1
import qualified Data.Text.IO        as T                                -- text-2.0.2
import           Data.Text           (Text)                              -- text-2.0.2
import           Control.Lens        ((^?!),(.~),(&))                             -- lens-5.2.3
import           Control.Monad       (void,forM)                              -- base-4.18.2.1
import qualified Data.Aeson          as A                                -- aeson-2.1.2.1
import           Data.Aeson          ((.=))                              -- aeson-2.1.2.1
import           Data.Aeson.Lens     (key, _JSON')                       -- lens-aeson-1.2.3
import           Data.Default.Class  (Default(def))                      -- data-default-class-0.1.2.2

config :: Config
config = def {
    _ollama = def {
        _baseURL = "http://124.127.95.156:11434/"
    }
}

translateToEnglish :: (MonadAI m,MonadIO m) => Text -> m Text
translateToEnglish txt = do
    req <- useOllamaRequest "/api/chat"
    block <- usePost data' req
    pure $ (block :: A.Value) ^?! key "message" . key "content" . _JSON'
    where data' = A.object [
                "model" .= "qwen2",
                "stream" .= False,
                "messages" .= [
                    A.object [
                        "role" .= "system",
                        "content" .= "Translate the Chinese to the English without any noise, no more than 50 words"
                    ],
                    A.object [
                        "role" .= "user",
                        "content" .= "需要帮助"
                    ],
                    A.object [
                        "role" .= "assistant",
                        "content" .= "Need help"
                    ],
                    A.object [
                        "role" .= "user",
                        "content" .= ("Please translate \"" <> txt  <> "\", output it without any noise" :: Text)
                    ]
                ]
            ]
test :: IO ()
test = (() <$) $ runAIT config $ do
    (Just value) <- liftIO $ A.decodeFileStrict "graph.json"
    let nodes = (value::A.Value) ^?! key "nodes" . _JSON'
    newNodes <- forM nodes $ \ node -> do 
        let name = (node::A.Value) ^?! key "name" . _JSON'
        r <- translateToEnglish name
        liftIO $ T.putStrLn $ name <> " -> " <> r
        pure $ node & key "name" .~ A.String r & key "id" .~ A.String r
    let newValue = value & key "nodes" .~ A.toJSON newNodes
    liftIO $ A.encodeFile "graph-new.json" newValue