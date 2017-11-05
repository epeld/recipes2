module Ingredient where
import Data.Monoid
import Control.Monad

import Options.Applicative
import Database.MySQL.Simple

import Schema
import Types
import qualified Db
import qualified Query
import ProgramOptions as Opts

data Command =
  Add IngredientParams |
  Query [Name]
  deriving (Show, Eq)

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


queryCommand :: Mod CommandFields Command
queryCommand = command "query" ( info opts desc )
  where
    desc = progDesc "Given a set of ingredients, find matching recipes"
    opts = Query <$> ingredientNames


commands :: Mod CommandFields Command
commands = addCommand <> queryCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run (Add params) conn = withTransaction conn $ do
  let ingredientName = nameString (Ingredient.name params)
      recipeId = recipeIdInt (Ingredient.recipeId params)
      amount = amountInt (Ingredient.amount params)
      unit = unitString <$> (Ingredient.unit params)

  Db.exec conn Schema.insertIngredient (Only ingredientName)
  Db.exec conn Schema.insertRecipeIngredient
    (recipeId :: Int, ingredientName :: String, amount :: Int, unit :: Maybe String)
    
  return ()


run (Query names) conn = do
  let strings = map nameString names
  r <- Db.query conn Schema.selectRecipesWithIngredients (In strings, length strings)
  Query.printHeader
  forM_ r $ \row -> Query.printRow_ row

--
-- Subparsers
--

ingredientParams :: Parser IngredientParams
ingredientParams =
  Ingredient <$> Opts.name <*> Opts.recipeId <*> Opts.amount <*> Opts.unit


ingredientNames :: Parser [Name]
ingredientNames = pure <$> Opts.name -- TODO support multiple names later if possible
