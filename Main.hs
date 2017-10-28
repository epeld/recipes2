
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Options.Applicative

import Data.Monoid
import qualified Data.Text as Text

import Control.Monad


--
-- Program Actions
--
data Action = Insert | List | Status

insert :: Parser Action
insert = flag' Insert (long "insert" <> short 'i' <> help "Insert a new Recipe")

list :: Parser Action
list = flag' List (long "list" <> short 'l' <> help "List Available Recipes")

status :: Parser Action
status = flag' Status (long "status" <> short 's' <> help "Print Status Info")

defaultAction :: Parser Action
defaultAction = pure List

action :: Parser Action
action = list <|> status <|> insert <|> defaultAction


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
  do let q = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, description VARCHAR(300), PRIMARY KEY (id))"
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

runAction conn Insert = do
  putStrLn "INSERT"

runAction conn List = do
  xs <- query_ conn "SELECT id, name, description FROM recipes"
  forM_ xs $ \ (id, name, description) ->
    putStrLn $ show (id :: Int) ++ ", " ++ Text.unpack name ++ ", " ++ Text.unpack description
    
runAction conn Status = do
  [Only i] <- query_ conn "SELECT count(*) FROM recipes"
  putStrLn (unwords [(show (i :: Int)), "recipes", "in", "database"])
