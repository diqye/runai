import EnviromentTool (runai)
import System.IO (stdout,hSetBuffering,BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runai