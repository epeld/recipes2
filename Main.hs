
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Options.Applicative
import Data.Monoid

--
-- Program Actions
--
data Action = List | Status

list :: Parser Action
list = flag' List (long "list" <> short 'l' <> help "List Available Recipes")

status :: Parser Action
status = flag' Status (long "status" <> short 's' <> help "Print Status Info")

defaultAction :: Parser Action
defaultAction = pure Status

action :: Parser Action
action = list <|> status <|> defaultAction


--
-- Program Options
--
data ProgramOptions = ProgramOptions
  { programAction :: Action
    
  }

programOptions :: Parser ProgramOptions
programOptions = ProgramOptions <$> Main.action

opts :: ParserInfo ProgramOptions
opts = info (programOptions <**> helper)
  ( fullDesc <> progDesc "Query a Database full of Recipes" <> header "recipes - :)" )


--
-- MySQL Setup
--
connectInfo = (defaultConnectInfo { connectPassword = "abcabc", connectDatabase = "recipes" })

setupTables :: Connection -> IO ()
setupTables conn =
  do let q = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, PRIMARY KEY (id))"
     execute_ conn q
     return ()

hello :: IO ()
hello = do
  options <- execParser opts

  --
  -- MySQL Setup
  conn <- connect connectInfo
  setupTables conn

  runAction conn (programAction options)

runAction :: Connection -> Action -> IO ()
runAction _ List = putStrLn "TODO List!"
runAction conn Status = do
  [Only i] <- query_ conn "select count(*) from recipes"
  putStrLn (unwords ["Recipe", "Count", (show (i :: Int))])
