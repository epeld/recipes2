{-# LANGUAGE OverloadedStrings #-}

module Schema where
import Database.MySQL.Simple

data Table =
  Recipes |
  Ingredients |
  RecipeIngredients
  deriving (Show, Eq)


tableCreationQuery :: Table -> Query
tableCreationQuery Recipes = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, description VARCHAR(300), PRIMARY KEY (id))"
tableCreationQuery Ingredients = "CREATE TABLE IF NOT EXISTS ingredients (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, PRIMARY KEY (id));"
tableCreationQuery RecipeIngredients = "CREATE TABLE IF NOT EXISTS recipe_ingredients (id INT NOT NULL AUTO_INCREMENT, recipe_id INT NOT NULL, ingredient_id INT NOT NULL, FOREIGN KEY (recipe_id) REFERENCES recipes(id) ON DELETE CASCADE, FOREIGN KEY (ingredient_id) REFERENCES ingredients(id) ON DELETE CASCADE, PRIMARY_KEY (id));"


dropTableQuery :: Table -> Query
dropTableQuery Recipes = "DROP TABLE IF EXISTS recipes"
dropTableQuery Ingredients = "DROP TABLE IF EXISTS ingredients"
dropTableQuery RecipeIngredients = "DROP TABLE IF EXISTS recipe_ingredients"


tables :: [Table]
tables = [Recipes, Ingredients, RecipeIngredients]


queryToString :: Query -> String
queryToString = read. show
