
{-# LANGUAGE OverloadedStrings #-}

module Recipe where
import Data.Monoid

import Options.Applicative
import Database.MySQL.Simple

import Types
import qualified Insert
import qualified Query
import qualified Delete
import qualified Ingredient
import ProgramOptions


data Command =
  Query Query.Options |
  Insert Insert.Options |
  Delete Delete.Options |
  Count
  deriving (Show)


commands :: Mod CommandFields Command
commands = insertCommand <> deleteCommand <> queryCommand <> countCommand

commandParser :: Parser Command
commandParser = hsubparser commands

run :: Command -> Connection -> IO ()
run (Insert opts) conn = Insert.run opts conn
run (Query opts) conn = Query.run opts conn
run (Delete opts) conn = Delete.run opts conn
run Count conn = do
  [Only i] <- query_ conn "SELECT count(*) FROM recipes"
  putStrLn (unwords [(show (i :: Int)), "recipes", "in", "database"])


insertCommand :: Mod CommandFields Command
insertCommand = command "insert" (info parser desc)
  where
    parser = Insert <$> insertOptions
    desc = progDesc "Insert a new Recipe with name NAME and (optionally) description DESCRIPTION"


deleteCommand :: Mod CommandFields Command
deleteCommand = command "delete" (info parser desc)
  where
    parser = Delete <$> deleteOptions
    desc = progDesc "Delete a Recipe given its RECIPE_ID"


queryCommand :: Mod CommandFields Command
queryCommand = command "query" (info parser desc)
  where
    parser = Query <$> queryOptions
    desc = progDesc "Query Available Recipes"


countCommand :: Mod CommandFields Command
countCommand = command "count" (info parser desc)
  where
    parser = pure Count
    desc = progDesc "Print Recipe Count"


insertOptions :: Parser Insert.Options
insertOptions = Insert.Options <$> name <*> description


queryOptions :: Parser Query.Options
queryOptions = Query.Options <$> recipeIdOption <*> nameOption <*> descriptionOption


deleteOptions :: Parser Delete.Options
deleteOptions = Delete.Options <$> recipeId
