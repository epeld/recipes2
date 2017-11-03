{-# LANGUAGE OverloadedStrings #-}

module Schema where
import Database.MySQL.Simple

recipeTable :: Query
recipeTable = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT,name VARCHAR(100) NOT NULL,description VARCHAR(300),PRIMARY KEY (id))"

ingredientsTable :: Query
ingredientsTable = "CREATE TABLE IF NOT EXISTS ingredients (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, PRIMARY KEY (id))"

recipeIngredientsTable :: Query
recipeIngredientsTable = "CREATE TABLE IF NOT EXISTS recipe_ingredients (id INT NOT NULL AUTO_INCREMENT, recipe_id INT NOT NULL, ingredient_id INT NOT NULL, FOREIGN KEY (recipe_id) REFERENCES recipes(id) ON DELETE CASCADE, FOREIGN KEY (ingredient_id) REFERENCES ingredients(id) ON DELETE CASCADE, PRIMARY_KEY (id))"

dropTable :: Query
dropTable = "DROP TABLE IF EXISTS ?"


type NamedSchema = (String, Query)

tables :: [NamedSchema]
tables = [("recipes", recipeTable),
          ("ingredients", ingredientsTable),
          ("recipe_ingredients", recipeIngredientsTable)]



queryToString :: Query -> String
queryToString = read. show
