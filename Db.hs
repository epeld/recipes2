module Db where
import Data.Monoid
import Control.Monad

import Options.Applicative
import Database.MySQL.Simple

import Schema

type TableSelection = Maybe Schema.Table

data Command =
  Schema |
  Create TableSelection |
  Drop TableSelection
  deriving (Show, Eq)

--
-- Commands
--
commandParser :: Parser Command
commandParser = hsubparser commands


commands :: Mod CommandFields Command
commands = schemaCommand <> createCommand <> dropCommand


run :: Command -> Connection -> IO ()
run Schema _ = forM_ Schema.tables printSchema
run (Create t) conn = withSelectedTables conn t tableCreationQuery
run (Drop t) conn = withSelectedTables conn t dropTableQuery


schemaCommand :: Mod CommandFields Command
schemaCommand = command "schema" ( info opts desc )
  where
    desc = progDesc "Print the Database Schema"
    opts = pure Schema


dropCommand :: Mod CommandFields Command
dropCommand = command "drop" ( info opts desc )
  where
    desc = progDesc "Drop Tables (DESTRUCTIVE OPERATION)"
    opts = Drop <$> tableParser


createCommand :: Mod CommandFields Command
createCommand = command "create" ( info opts desc )
  where
    desc = progDesc "Create Tables"
    opts = Create <$> tableParser


--
-- Table Parsers
--

allTables :: Mod CommandFields TableSelection
allTables = command "all" ( info opts desc )
  where
    desc = progDesc "All tables"
    opts = pure Nothing


specificTable :: Table -> Mod CommandFields TableSelection
specificTable t = command ( tableName t ) ( info opts desc )
  where
    desc = progDesc ( "The " ++ tableName t ++ " table" )
    opts = pure (Just t)


tableParser :: Parser TableSelection
tableParser = hsubparser ( metavar "TABLE" <> allTables <> mconcat (specificTable <$> tables) )


--
-- Helpers
--
tableName :: Table -> String
tableName Recipes = "recipes"
tableName Ingredients = "ingredients"
tableName RecipeIngredients = "recipe_ingredients"


selectedTables :: TableSelection -> [Table]
selectedTables Nothing = Schema.tables
selectedTables (Just t) = [t]


withSelectedTables :: Connection -> TableSelection -> (Table -> Query) -> IO ()
withSelectedTables conn t f =
  forM_ (selectedTables t) $ \table -> do let q = f table
                                          printQuery q
                                          execute_ conn q


--
-- Printing
--
printSchema :: Table -> IO ()
printSchema t = do
  printComment (tableName t)
  printQuery (tableCreationQuery t)


printQuery :: Query -> IO ()
printQuery q = putStrLn ( "Query: " ++ queryToString q )


printComment :: String -> IO ()
printComment comment = putStrLn ("\n%% " ++ comment)
