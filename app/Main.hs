
import EnviromentTool
import Myai
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Monoid
import Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L


testa :: (MonadReader Int m, MonadReader Char m, MonadIO m) => m ()
testa = do
    i <- ask
    c <- ask
    liftIO $ do
        print (i :: Int)
        print (c :: Char)


main :: IO ()
main = fmap (const ()) $ runEnvAIT $ do
    catchError runAI $ runReaderT $ do 
        (First err) <- ask
        liftIO $ putStrLn "Error"
        liftIO $ L.putStrLn $ A.encode err
    liftIO $ putStrLn "Done"
