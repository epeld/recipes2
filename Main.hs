
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Options.Applicative

import Data.Monoid
import qualified Data.Text as Text

import Control.Monad


--
-- Program Actions
--
data ProgramCommand = List | Status | Insert | Help


insertCommand :: Mod CommandFields ProgramCommand
insertCommand = command "insert" ( info (commandOptions Insert) ( progDesc "Insert a new Recipe"))

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

--
-- Program Options
--

programInfo :: ParserInfo ProgramCommand
programInfo = info (programCommand <**> helper)
  ( fullDesc <> progDesc "Query a recipe database" <> header "recipes - lots of them!" )

description :: Parser (Maybe String)
description = option maybeStr
  ( long "description" <>
    short 'd' <>
    metavar "DESCRIPTION" <>
    value Nothing <>
    help "name recipe description" )

name :: Parser (Maybe String)
name = option maybeStr
  ( long "name" <>
    short 'n' <>
    metavar "NAME" <>
    value Nothing <>
    help "the recipe name" )

maybeStr :: ReadM (Maybe String)
maybeStr = Just <$> str

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
  runAction conn cmd

hello :: IO ()
hello = do
  cmd <- execParser programInfo
  conn <- setupDatabase

  runAction conn cmd


printHelp :: IO a
printHelp = handleParseResult . Failure $ parserFailure programPrefs programInfo ShowHelpText mempty

runAction :: Connection -> ProgramCommand -> IO ()

runAction conn Insert = do
  putStrLn "INSERT"

runAction conn List = do
  xs <- query_ conn "SELECT id, name, description FROM recipes"
  forM_ xs $ \ (id, name, description) ->
    putStrLn $ show (id :: Int) ++ ", " ++ Text.unpack name ++ ", " ++ Text.unpack description
    
runAction conn Status = do
  [Only i] <- query_ conn "SELECT count(*) FROM recipes"
  putStrLn (unwords [(show (i :: Int)), "recipes", "in", "database"])

runAction _ Help = printHelp
