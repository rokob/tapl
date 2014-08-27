import Parser
import System.IO
import System.Environment

main = do
  args <- getArgs
  let filename = head args
  handle <- openFile filename ReadMode
  contents <- hGetContents handle  
  let result = prettyRun contents
  mapM_ print $ result
