
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Options.Applicative

import Data.Monoid
import qualified Data.Text as Text

import Control.Monad

import Types
import qualified Insert


--
-- Program Actions
--
data ProgramCommand =
  List |
  Status |
  Insert Insert.Options |
  Help
  deriving (Show)


insertCommand :: Mod CommandFields ProgramCommand
insertCommand = command "insert" ( info ( insertOptions ) ( progDesc "Insert a new Recipe with name NAME and (optionally) description DESCRIPTION"))

listCommand :: Mod CommandFields ProgramCommand
listCommand = command "list" ( info (commandOptions List) ( progDesc "List Available Recipes"))

statusCommand :: Mod CommandFields ProgramCommand
statusCommand = command "status" ( info (commandOptions List) ( progDesc "Print Status Info"))

helpCommand :: Mod CommandFields ProgramCommand
helpCommand = command "help" ( info (pure Help) ( progDesc "Print This Message" ) )

programCommand :: Parser ProgramCommand
programCommand = hsubparser
  ( listCommand <> statusCommand <> insertCommand <> helpCommand )

commandOptions :: ProgramCommand -> Parser ProgramCommand
commandOptions x = pure x

insertOptions = Insert <$> (Insert.Options <$> name <*> description)

--
-- Program Options
--

programInfo :: ParserInfo ProgramCommand
programInfo = info (programCommand <**> helper)
  ( fullDesc <> progDesc "Query a recipe database" <> header "recipes - lots of them!" )

description :: Parser (Maybe Description)
description = argument (maybeStr2 Description) (metavar "DESCRIPTION" <> value Nothing)

descriptionOption = option (maybeStr2 Description)
  ( long "description" <>
    short 'd' <>
    metavar "DESCRIPTION" <>
    value Nothing <>
    help "recipe description" )

name :: Parser Name
name = argument (Name <$> str) (metavar "NAME")

nameOption = option (Name <$> str)
  ( long "name" <>
    short 'n' <>
    metavar "NAME" <>
    help "the recipe name" )

maybeStr :: ReadM (Maybe String)
maybeStr = Just <$> str

maybeStr2 f = (Just . f) <$> str

--
-- MySQL Setup
--
connectInfo = (defaultConnectInfo { connectPassword = "abcabc", connectDatabase = "recipes" })

setupTables :: Connection -> IO ()
setupTables conn =
  do let q = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, description VARCHAR(300), PRIMARY KEY (id))"
     execute_ conn q
     return ()

setupDatabase :: IO Connection
setupDatabase = do
  conn <- connect connectInfo
  setupTables conn
  return conn

programPrefs :: ParserPrefs
programPrefs = prefs showHelpOnEmpty

-- Test Fn to try out different args interactively
mainWithArgs :: String -> IO ()
mainWithArgs stringArgs = do
  let args = words stringArgs
  cmd <- handleParseResult $ execParserPure programPrefs programInfo args
  conn <- setupDatabase
  runCommand cmd conn

main :: IO ()
main = do
  cmd <- execParser programInfo
  conn <- setupDatabase

  runCommand cmd conn


printHelp :: IO a
printHelp = handleParseResult . Failure $ parserFailure programPrefs programInfo ShowHelpText mempty

runCommand :: ProgramCommand -> Connection -> IO ()

runCommand (Insert opts) conn = do
  Insert.run opts conn


runCommand List conn = do
  xs <- query_ conn "SELECT id, name, description FROM recipes"
  forM_ xs $ \ (id, name, description) ->
    putStrLn $ show (id :: Int) ++ ", " ++ Text.unpack name ++ ", " ++ Text.unpack description
    
runCommand Status conn = do
  [Only i] <- query_ conn "SELECT count(*) FROM recipes"
  putStrLn (unwords [(show (i :: Int)), "recipes", "in", "database"])

runCommand Help _ = printHelp
