module Ingredient where
import Data.Monoid

import Options.Applicative
import Database.MySQL.Simple

import Schema
import Types

data Command = Add deriving (Show, Eq)

data IngredientParams = Ingredient
  { name :: String,
    recipeId :: RecipeId,
    amount :: Int,
    unit :: Maybe String
  }
  deriving (Show, Eq)

--
-- Commands
--
addCommand :: Mod CommandFields Command
addCommand = command "add" ( info opts desc )
  where
    desc = progDesc "Add an ingredient to a recipe"
    opts = pure Add


commands :: Mod CommandFields Command
commands = addCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run Add conn = withTransaction conn $ do
  let ingredientName = "foo"
      recipeId = 1
      amount = 1
      unit = "mg"
  execute conn Schema.insertIngredient (Only ingredientName)
  execute conn Schema.insertRecipeIngredient
    (recipeId :: Int, ingredientName :: String, amount :: Int, unit :: String)
  return ()

--
--
--
