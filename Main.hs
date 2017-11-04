
import Database.MySQL.Simple
import Options.Applicative

import Data.Monoid
import qualified Data.Text as Text

import Control.Monad

import qualified Recipe
import qualified Ingredient
import qualified Db
import ProgramOptions
import Schema

--
-- Program Actions
--
data ProgramCommand =
  Recipe Recipe.Command | 
  Ingredient Ingredient.Command |
  Database Db.Command | 
  Help
  deriving (Show)


recipeCommand :: Mod CommandFields ProgramCommand
recipeCommand = command "recipe" (info parser desc)
  where
    parser = Recipe <$> Recipe.commandParser
    desc = progDesc "Recipe Commands"


ingredientCommand :: Mod CommandFields ProgramCommand
ingredientCommand = command "ingredient" (info parser desc)
  where
    parser = Ingredient <$> Ingredient.commandParser
    desc = progDesc "Ingredient Commands"


dbCommand :: Mod CommandFields ProgramCommand
dbCommand = command "database" (info parser desc)
  where
    parser = Database <$> Db.commandParser
    desc = progDesc "Database Maintenence Commands"

helpCommand :: Mod CommandFields ProgramCommand
helpCommand = command "help" (info parser desc)
  where
    parser = pure Help
    desc = progDesc "Print This Message"


programCommand :: Parser ProgramCommand
programCommand = hsubparser (
  recipeCommand <> ingredientCommand <> dbCommand <> helpCommand
  )


run :: ProgramCommand -> Connection -> IO ()
run (Recipe cmd) conn = Recipe.run cmd conn
run (Ingredient cmd) conn = Ingredient.run cmd conn
run (Database cmd) conn = Db.run cmd conn
run Help _ = printHelp


--
-- MySQL Setup
--
connectInfo = (defaultConnectInfo { connectPassword = "abcabc", connectDatabase = "recipes" })

setupTables :: Connection -> IO ()
setupTables conn =
  do execute_ conn (Schema.tableCreationQuery Schema.Recipes)
     execute_ conn Schema.traditionalMode
     return ()

setupDatabase :: IO Connection
setupDatabase = do
  conn <- connect connectInfo
  setupTables conn
  return conn

--
-- Program Definition
--

-- Test Fn to try out different args interactively
mainWithArgs :: String -> IO ()
mainWithArgs stringArgs = do
  let args = words stringArgs
  cmd <- handleParseResult $ execParserPure programPrefs programInfo args
  conn <- setupDatabase
  run cmd conn

main :: IO ()
main = do
  cmd <- execParser programInfo
  conn <- setupDatabase
  run cmd conn


printHelp :: IO a
printHelp = handleParseResult . Failure $ parserFailure programPrefs programInfo ShowHelpText mempty


programInfo :: ParserInfo ProgramCommand
programInfo = info (programCommand <**> helper)
  ( fullDesc <> progDesc "Query a recipe database" <> header "recipes - lots of them!" )

programPrefs :: ParserPrefs
programPrefs = prefs showHelpOnEmpty
