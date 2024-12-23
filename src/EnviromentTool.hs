{-# LANGUAGE FlexibleContexts #-}
module EnviromentTool where

import           Mydefault                                                                                                                                                                         -- 
import           Myai                                                                                                                                                                              -- 
import qualified Myai.Data.Azure                     as Az                                                                                                                                         -- 
import qualified Myai.Data.Ollama                    as O                                                                                                                                          -- 
import           Myai.Data.Config                    (Config(_azure,_ollama), MonadAI, createError)                                                                                                -- 
import           Control.Monad.Cont                  (runContT, ContT, MonadCont)                                                                               -- mtl-2.3.1
import Control.Monad.Trans.Class (MonadTrans(lift))
import       Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader                (ReaderT)                                                                                                                                     -- mtl-2.3.1
import           Control.Monad.Except                (ExceptT, MonadError(throwError))                                                                                                             -- mtl-2.3.1
import qualified Data.Text                           as T                                                                                                                                          -- text-2.0.2
import qualified Data.Text.IO                        as T                                                                                                                                          -- text-2.0.2
import           Control.Lens                        ((^?), (&), (.~), (%~), (^?!))                                                                                                                -- lens-5.2.3
import qualified System.Environment                  as E                                                                                                                                          -- base-4.18.2.1
import           Data.Maybe                          (fromMaybe, isJust, fromJust, isNothing)                                                                                                      -- base-4.18.2.1
import           Data.Either                         (fromRight)                                                                                                                                   -- base-4.18.2.1
import           Control.Applicative                 ((<|>))                                                                                                                                       -- base-4.18.2.1
import           System.Environment                  (getArgs, getProgName)                                                                                                                        -- base-4.18.2.1
import           Control.Monad                       (unless, when)                                                                                                                                -- base-4.18.2.1
import           Data.List                           (intersperse, isPrefixOf, dropWhileEnd, isInfixOf)                                                                                            -- base-4.18.2.1
import           Data.Monoid                         (First(..))                                                                                                                                   -- base-4.18.2.1
import           System.Exit                         (exitSuccess)                                                                                                                                 -- base-4.18.2.1
import           Data.Char                           (isSpace)                                                                                                                                     -- base-4.18.2.1
import           Data.Aeson                          (FromJSON, Value(Null), object,(.=))                                                                                                               -- aeson-2.1.2.1
import qualified Data.Aeson                          as A                                                                                                                                          -- aeson-2.1.2.1
import qualified Data.Yaml                           as Y                                                                                                                                          -- yaml-0.11.11.2
import           Text.Parsec                         (runParserT, char, notFollowedBy, space, anyChar, many1, many, ParsecT, spaces, try)                                                          -- parsec-3.1.16.1
import           Text.Parsec.Text                    (Parser)                                                                                                                                      -- parsec-3.1.16.1
import qualified Data.Vector                         as V                                                                                                                                          -- vector-0.13.1.0
import           Data.Aeson.Lens                     (key, _JSON', _Array, atKey, nth)                                                                                                             -- lens-aeson-1.2.3
import           System.Process                      (readProcess)                                                                                                                                 -- process-1.6.19.0
import           System.Console.Haskeline            (runInputT, defaultSettings, getInputLine, Settings(complete), setComplete, simpleCompletion, Completion, Settings(Settings), CompletionFunc) -- haskeline-0.8.2.1
import           System.Console.Haskeline.Completion (completeFilename)                                                                                                                            -- haskeline-0.8.2.1
import           Control.Monad.Catch                 (MonadMask)                                                                                                                                   -- exceptions-0.10.7
import           System.Directory                    (doesFileExist, getHomeDirectory)                                                                                                             -- directory-1.3.8.5
import           System.FilePath                     (takeBaseName, (</>))                                                                                                                         -- filepath-1.4.300.1
import qualified Data.ByteString.Lazy.Char8          as L                                                                                                                                          -- bytestring-0.11.5.3
import           Control.Monad.Trans.Cont            (evalContT)                                                                                                                                   -- transformers-0.6.1.0
import           Data.String.Conversions             (cs)                                                                                                                                          -- string-conversions-0.4.0.1
import           Data.Default.Class                  (Default(..))                                                                                                                                 -- data-default-class-0.1.2.2


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

toAzure :: Value -> Maybe (Value,String,Config)
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
    pure (config,model,def { _azure = Az.Azure {Az._key = key, Az._endpoint = endpoint}})

toOllama :: Value -> Maybe (Value,String,Config)
toOllama yaml = do
    model <- yaml ^? key "model" . _JSON'
    ollama <- yaml ^? key "ollama"
    msgs <- yaml ^? key "messages" . _JSON'
    baseURL <- ollama ^? key "baseURL" . _JSON'
    let ollamaConf = ollama & atKey "baseURL" .~ Nothing
    let config = ollamaConf <> object [
                "model" =: model,
                "stream" =: True,
                "messages" =: convertMymsgs msgs
            ]
    pure (config,model,def {_ollama = def {O._baseURL = baseURL}})
runai :: IO ()
runai = processArgs >>= runai' where
    runai' arg = do
        let fileName = takeBaseName arg
        (v,text) <- readYamlFromFile arg
        let (param,model,aconf) = fromJust $ toAzure v <|> toOllama v
        let config = aconf
        a <- runAIT config $ myai v param (fileName,model) text
        case a of
            Right _ -> pure ()
            Left (First (Just obj)) | obj ^? key "command" == Just "reload"  -> do 
                putStr "\ESC[38;5;69m"
                putStrLn $ "Rload file " <> arg
                runai' arg
            Left (First (Just obj )) | obj ^? key "command" == Just "change" -> do
                putStr "\ESC[38;5;69m"
                let path =  obj ^?! key "value" . _JSON'
                putStrLn $ "Change file to " <> path
                runai' path
            Left (First (Just (A.String e))) -> do
                putStr "\ESC[38;5;196m"
                T.putStrLn e
            Left e -> do
                putStr "\ESC[38;5;196m"
                L.putStrLn $ A.encode e
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
            putStr "\ESC[38;5;69m"
            putStrLn $ "使用: " <> progName <> " your/path.yaml"
            putStrLn "在会话中(Tab补全)"
            putStrLn "> :export         将会话导出文件为export.yaml"
            putStrLn "> :start          直接调用LLM"
            putStrLn "> :quit           退出"
            putStrLn "> :clear          清空会话"
            putStrLn "> :               进入/退出多行模式"
            putStrLn "> :reload         从新加载当前配置文件"
            putStrLn "> :change file.yaml 加载新的配置文件"
            putStr "\ESC[0;0m"
            exitSuccess

mySettings :: MonadIO m => Settings m
mySettings = setComplete complete defaultSettings where
    complete :: MonadIO m => CompletionFunc m
    complete p@(a ,_) | ":change " `isPrefixOf` reverse a = completeFilename p
    complete (a ,_) | ":" `isPrefixOf` reverse a = do
        let commonds = [":export",":start", ":clear",":quit", ":reload", ":change"]
        return ("", simpleCompletion <$> filter (isPrefixOf $ reverse a)  commonds)
    complete _ =  return ("",[])


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

myai :: (MonadIO m,MonadAI m,MonadMask m) => Value -> Value -> (String,String) -> T.Text -> m ()
myai yaml param (prompt,model) originText = runInputT mySettings $ evalContT $ do
    let logger = yaml ^? key "log" == Just  (A.toJSON True)
    let noSession = yaml ^? key "noSession" == Just  (A.toJSON True)
    req <- lift $ lift $ useAzureRequest model <|> useOllamaRequest "/api/chat"
    (recurConversation,msgs) <- createLabel V.empty
    input' <- lift $ do
        let prefix = "\ESC[38;5;69m" <> prompt <> "." <> model
        a <- getInputLine $ prefix <> if noSession then ":- " else ":+ "
        let mutipleInput = do
                a <- getInputLine $ prefix <> "| "
                if a == Just ":" then pure Nothing else do
                    b <- mutipleInput
                    pure $ if isNothing b then a else a <> Just "\n" <> b
        
        if a == Just ":" then mutipleInput else pure a
    let input = fmap trim input'
    when (input == Just ":reload") $ lift $ lift $ throwError $ createError $ A.object ["command" =: "reload"]
    when (fmap (isPrefixOf ":change") input == Just True) $ do
        let input' = fromJust input
        let path = trim $ drop (length ":change") input'
        path <- handlePath path
        when (null path) $ do
            liftIO $ putStrLn ":change path\nThe path can not is null"
            recurConversation msgs
        isExist <- liftIO $ doesFileExist path
        unless isExist $ do
            liftIO $ putStrLn $ "The file [" <> path <> "] isn't exist"
            recurConversation msgs

        lift $ lift $ throwError $ createError $ A.object ["command" =: "change", "value" =: path]
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
        let msgs' = if userInpput == ":start" then msgs else V.snoc msgs  $ object ["role" =: "user","content" =: userInpput]
        let p = param & key "messages" . _Array %~ (<> msgs')
        assistantOutput <- lift $ lift $ useStream p req $ do
            liftIO $ T.putStr "\ESC[38;5;169m"
            (recur,v,text) <- recurValue ""
            when logger $ liftIO $ putStrLn "" >> printJSON v
            let token = fromMaybe "" $ v  ^? key "choices" . nth 0 . key "delta" . key "content" . _JSON' <|> v  ^? key "message"  . key "content" . _JSON'
            liftIO $ T.putStr token
            recur $ text <> token
            liftIO $ putStrLn "\ESC[0;0m"
            pure $ text <> token
        let msgs = V.snoc msgs'  $ object ["role" =: "assistant","content" =: assistantOutput]
        if noSession then recurConversation V.empty else recurConversation msgs
    liftIO $ putStr "\ESC[0;0m"

handlePath :: MonadIO m => FilePath -> m FilePath
handlePath path = do
    home <- liftIO getHomeDirectory
    let absolutePath
            | "~" `isPrefixOf` path = home <> tail path
            | otherwise = path
    pure absolutePath

export :: MonadIO m => V.Vector Value -> T.Text -> m ()
export msgs text = liftIO $ do
    let yaml = parseYaml text
    Y.encodeFile "export.yaml" $ yaml & key "messages" . _Array %~ (<> msgs)

printJSON :: MonadIO m => Value -> m ()
printJSON v = liftIO $ L.putStrLn $ A.encode v