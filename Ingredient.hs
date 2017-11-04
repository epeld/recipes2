module Ingredient where
import Data.Monoid

import Options.Applicative
import Database.MySQL.Simple

import Schema
import Types
import ProgramOptions as Opts

data Command = Add IngredientParams deriving (Show, Eq)

data IngredientParams = Ingredient
  { name :: Name,
    recipeId :: RecipeId,
    amount :: Amount,
    unit :: Maybe Unit
  }
  deriving (Show, Eq)

--
-- Commands
--
addCommand :: Mod CommandFields Command
addCommand = command "add" ( info opts desc )
  where
    desc = progDesc "Add an ingredient to a recipe"
    opts = Add <$> ingredientParams


commands :: Mod CommandFields Command
commands = addCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run (Add params) conn = withTransaction conn $ do
  let ingredientName = nameString (Ingredient.name params)
      recipeId = recipeIdInt (Ingredient.recipeId params)
      amount = amountInt (Ingredient.amount params)
      unit = unitString <$> (Ingredient.unit params)
      
  execute conn Schema.insertIngredient (Only ingredientName)
  execute conn Schema.insertRecipeIngredient
    (recipeId :: Int, ingredientName :: String, amount :: Int, unit :: Maybe String)
  return ()

--
-- Bla
--

ingredientParams :: Parser IngredientParams
ingredientParams =
  Ingredient <$> Opts.name <*> Opts.recipeId <*> Opts.amount <*> Opts.unit
