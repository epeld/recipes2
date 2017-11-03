module Db where
import Data.Monoid
import Control.Monad

import Options.Applicative
import Database.MySQL.Simple

import Schema

data Command =
  Schema |
  Drop (Maybe Schema.Table)
  deriving (Show, Eq)


schemaCommand :: Mod CommandFields Command
schemaCommand = command "schema" ( info opts desc )
  where
    desc = progDesc "Print the Database Schema"
    opts = pure Schema


dropCommand :: Mod CommandFields Command
dropCommand = command "drop" ( info opts desc )
  where
    desc = progDesc "Drop Tables (DESTRUCTIVE OPERATION)"
    opts = Drop <$> hsubparser ( metavar "TABLE" <> dropAll <> mconcat (dropTable <$> tables) )


dropAll :: Mod CommandFields (Maybe Table)
dropAll = command "all" ( info opts desc )
  where
    desc = progDesc "All tables"
    opts = pure Nothing


dropTable :: Table -> Mod CommandFields (Maybe Table)
dropTable t = command ( tableName t ) ( info opts desc )
  where
    desc = progDesc ( "Drop the " ++ tableName t ++ " table" )
    opts = pure Nothing


tableName :: Table -> String
tableName Recipes = "recipes"
tableName Ingredients = "ingredients"
tableName RecipeIngredients = "recipe_ingredients"


commands :: Mod CommandFields Command
commands = schemaCommand <> dropCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run Schema _ = forM_ Schema.tables printSchema
run (Drop Nothing) conn =
  forM_ Schema.tables $ \table -> execute_ conn (tableCreationQuery table)



printSchema :: Table -> IO ()
printSchema t = do
  printComment (tableName t)
  printQuery (tableCreationQuery t)


printQuery :: Query -> IO ()
printQuery = putStrLn . queryToString


printComment :: String -> IO ()
printComment comment = putStrLn ("\n%% " ++ comment)
