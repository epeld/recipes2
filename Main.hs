
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Options.Applicative

import Data.Monoid
import qualified Data.Text as Text

import Control.Monad

import Types
import qualified Insert
import qualified Query
import qualified Delete


--
-- Program Actions
--
data ProgramCommand =
  Query Query.Options |
  Insert Insert.Options |
  Delete Delete.Options | 
  Status |
  Help
  deriving (Show)


insertCommand :: Mod CommandFields ProgramCommand
insertCommand = command "insert" ( info ( insertOptions ) ( progDesc "Insert a new Recipe with name NAME and (optionally) description DESCRIPTION"))

deleteCommand :: Mod CommandFields ProgramCommand
deleteCommand = command "delete" ( info ( deleteOptions ) ( progDesc "Delete a Recipe given its RECIPE_ID"))

queryCommand :: Mod CommandFields ProgramCommand
queryCommand = command "query" ( info ( queryOptions ) ( progDesc "Query Available Recipes"))

statusCommand :: Mod CommandFields ProgramCommand
statusCommand = command "status" ( info (commandOptions Status) ( progDesc "Print Status Info"))

helpCommand :: Mod CommandFields ProgramCommand
helpCommand = command "help" ( info (pure Help) ( progDesc "Print This Message" ) )

programCommand :: Parser ProgramCommand
programCommand = hsubparser
  ( queryCommand <> insertCommand <> deleteCommand <> statusCommand <> helpCommand )

commandOptions :: ProgramCommand -> Parser ProgramCommand
commandOptions x = pure x

insertOptions = Insert <$> ( Insert.Options <$> name <*> description )
queryOptions = Query <$> ( Query.Options <$> recipeIdOption <*> nameOption <*> descriptionOption )
deleteOptions = Delete <$> ( Delete.Options <$> recipeId )

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

nameOption = option (maybeStr2 Name)
  ( long "name" <>
    short 'n' <>
    metavar "NAME" <>
    value Nothing <>
    help "recipe name" )


recipeId :: Parser RecipeId
recipeId = argument ( RecipeId <$> auto ) (metavar "RECIPE_ID")

recipeIdOption :: Parser (Maybe RecipeId)
recipeIdOption = option ( maybeAuto RecipeId )
  ( long "id" <>
    short 'i' <>
    metavar "RECIPE_ID" <>
    value Nothing <>
    help "recipe id" )

maybeStr :: ReadM (Maybe String)
maybeStr = Just <$> str

maybeStr2 f = Just <$> (f <$> str)

maybeAuto f = Just <$> (f <$> auto)

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
runCommand (Insert opts) conn = Insert.run opts conn
runCommand (Query opts) conn = Query.run opts conn
runCommand (Delete opts) conn = Delete.run opts conn
    
runCommand Status conn = do
  [Only i] <- query_ conn "SELECT count(*) FROM recipes"
  putStrLn (unwords [(show (i :: Int)), "recipes", "in", "database"])

runCommand Help _ = printHelp
