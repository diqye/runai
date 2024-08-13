module EnviromentTool where

import Mydefault
import Myai
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift),runContT,ContT, MonadCont)
import qualified System.Environment as E
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.String.Conversions (cs)
import Data.Aeson (FromJSON, Value (Null),object)
import qualified Data.Yaml as Y
import Data.Either (fromRight)
import Text.Parsec (runParserT, char, notFollowedBy, space, anyChar, many1, many, ParsecT, spaces, try)
import Text.Parsec.Text (Parser)
import Control.Lens((^?), (&), (.~), (%~))
import Data.Aeson.Lens (key, _JSON',_Array, atKey, nth)
import Control.Applicative((<|>))
import qualified Myai.Data.Azure as Az
import System.Environment (getArgs, getProgName)
import Myai.Data.Config (Config(_azure), MonadAI)
import Data.Default.Class (Default(..))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Aeson as A
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import System.Console.Haskeline
    ( runInputT,
      defaultSettings,
      getInputLine,
      Settings(complete),
      setComplete,
      simpleCompletion )
import Control.Monad.Catch(MonadMask)
import Control.Monad (unless,when)
import qualified Data.Vector as V
import Control.Monad.Trans.Cont (evalContT)
import System.Console.Haskeline.Completion (completeFilename)
import Data.List (intersperse)
import Data.Monoid (First(..))
import System.Process (readProcess)
import System.Exit (exitSuccess)

setVars :: MonadIO m => T.Text -> m T.Text
setVars content = liftIO $ do
    key <- getEnvDef' "AZURE_KEY"
    endpoint <- getEnvDef' "AZURE_ENDPOINT"
    let text = T.replace "$AZURE_KEY" (cs key) $ T.replace "$AZURE_ENDPOINT" (cs endpoint) content
    pure text

getEnvDef :: String -> String -> IO String
getEnvDef envName def = do
    val <- E.lookupEnv envName
    pure $ fromMaybe def val

getEnvDef' :: String ->  IO String
getEnvDef' envName  = getEnvDef envName $ "$" <> envName

parseYaml :: T.Text -> Value
parseYaml yamlText = fn $ Y.decodeEither' (cs yamlText) where
    fn (Right a) = a
    fn (Left e) = error $ show e

varparser :: MonadIO m => ParsecT T.Text () m String
varparser =  try varparser2 <|> try varparser1

varparser1 :: MonadIO m => ParsecT T.Text () m String
varparser1 = do
    _ <- char '$'
    var <- many1 (notFollowedBy (space <|> char '$' ) >> anyChar)
    liftIO $ getEnvDef' var

varparser2 :: MonadIO m => ParsecT T.Text () m String
varparser2 = do
    _ <- char '$'
    _ <- char '('
    spaces
    var <- many1 (notFollowedBy space >> anyChar)
    spaces
    _ <- char ')'
    liftIO $ getEnvDef' var

myparser' :: MonadIO m => ParsecT T.Text () m String
myparser' = do
    a <- many $ notFollowedBy varparser >> anyChar
    b <- varparser
    c <- many $  notFollowedBy varparser >> anyChar
    pure $ a <> b <> c

myparser :: MonadIO m => ParsecT T.Text () m T.Text
myparser = do
    strs <- many myparser'
    pure $ cs $ concat strs

parseVars :: MonadIO m => T.Text -> m T.Text
parseVars text = do
    r <- runParserT myparser () "yaml" text
    let decon (Left _) = pure text
        decon (Right a) = pure a
    decon r

readYamlFromFile :: FilePath -> IO (Value,T.Text)
readYamlFromFile filePath = do
    text <- T.readFile filePath
    vtext <- parseVars text
    pure (parseYaml vtext,text)

convertMymsgs :: [Value] -> [Value]
convertMymsgs = map convert where
    convert v = fromMaybe v $ system <|> assistant <|> user
        where
            system = do
                text <- v ^? key "system"
                pure $ object [ "role" =: "system", "content" =: text]
            assistant = do
                text <- v ^? key "assistant"
                pure $ object [ "role" =: "assistant", "content" =: text]
            user = do
                text <- v ^? key "user"
                pure $ object [ "role" =: "user", "content" =: text]

toAzure :: Value -> Maybe (Value,String,Az.Azure)
toAzure yaml = do
    model <- yaml ^? key "model" . _JSON'
    azure <- yaml ^? key "azure"
    msgs <- yaml ^? key "messages" . _JSON'
    endpoint <- azure ^? key "endpoint" . _JSON'
    key <- azure ^? key "key" . _JSON'
    let azure' = azure & atKey "endpoint" .~ Nothing & atKey "key" .~ Nothing
    let config = azure' <> object [
                "stream" =: True,
                "messages" =: convertMymsgs msgs
            ]
    pure (config,model,Az.Azure {Az._key = key, Az._endpoint = endpoint})

runai :: IO ()
runai = do
    arg <- processArgs
    (v,text) <- readYamlFromFile arg
    let (param,model,aconf) = fromJust $ toAzure v
    let config = def {_azure = aconf}
    a <- runAIT config $ myai v param model text
    putStr "\ESC[38;5;196m"
    case a of
        Right _ -> pure ()
        Left (First (Just (A.String e))) -> T.putStrLn e
        Left e -> L.putStrLn $ A.encode e
    putStr "\ESC[0;0m"

processArgs :: IO String
processArgs = do
    args <- getArgs
    process args
    where
        process [] = printHelp
        process [arg] = return arg
        process _ = printHelp
        printHelp = do
            progName <- getProgName
            putStr "\ESC[38;5;169m"
            putStrLn $ "使用: " <> progName <> " your/path.yaml"
            putStr "\ESC[38;5;69m"
            putStrLn "在会话中"
            putStrLn ":export 将会话导出文件为export.yaml"
            putStrLn ":clear  清空会话"
            putStrLn ": 进入/退出多行模式"
            putStr "\ESC[0;0m"
            exitSuccess

myai :: (MonadIO m,MonadAI m,MonadMask m) => Value -> Value -> String -> T.Text -> m ()
myai yaml param model originText = runInputT defaultSettings $ evalContT $ do
    let logger = yaml ^? key "log" == Just  (A.toJSON True)
    req <- lift $ lift $ useAzureRequest model
    (recurConversation,msgs) <- createLabel V.empty
    input <- lift $ do
        let prefix = "\ESC[38;5;69m" <> model
        a <- getInputLine $ prefix <> "> "
        let mutipleInput = do
                a <- getInputLine $ prefix <> "| "
                if a == Just ":" then pure Nothing else do
                    b <- mutipleInput
                    pure $ if isNothing b then a else a <> Just "\n" <> b
        if a == Just ":" then mutipleInput else pure a
    -- 清除会话记录
    when (input == Just ":clear") $ do
        _ <- liftIO $ T.putStr "\ESC[H\ESC[2J\ESC[3J"
        recurConversation V.empty
    -- 导出会话记录
    when (input == Just ":export") $ do
        export msgs originText
        liftIO $ T.putStrLn "Success export.yaml"
        recurConversation msgs
    -- liftIO $ print input
    unless (isNothing input || input == Just ":quit") $ do
        let userInpput = fromJust input
        let msgs' = V.snoc msgs  $ object ["role" =: "user","content" =: userInpput]
        let p = param & key "messages" . _Array %~ (<> msgs')
        assistantOutput <- lift $ lift $ useStream p req $ do
            liftIO $ T.putStr "\ESC[38;5;169m"
            (recur,v,text) <- recurValue ""
            when logger $ liftIO $ putStrLn "" >> printJSON v
            let token = fromMaybe "" $ v  ^? key "choices" . nth 0 . key "delta" . key "content" . _JSON'
            liftIO $ T.putStr token
            recur $ text <> token
            liftIO $ putStrLn "\ESC[0;0m"
            pure $ text <> token
        let msgs = V.snoc msgs'  $ object ["role" =: "assistant","content" =: assistantOutput]
        recurConversation msgs
    liftIO $ putStr "\ESC[0;0m"


export :: MonadIO m => V.Vector Value -> T.Text -> m ()
export msgs text = liftIO $ do
    let yaml = parseYaml text
    Y.encodeFile "export.yaml" $ yaml & key "messages" . _Array %~ (<> msgs)

printJSON :: MonadIO m => Value -> m ()
printJSON v = liftIO $ L.putStrLn $ A.encode v