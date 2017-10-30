{-# LANGUAGE OverloadedStrings #-}

module Delete where
import Types
import Database.MySQL.Simple

data Options = Options
  { id :: RecipeId }
  deriving (Show, Eq)


run :: Options -> Connection -> IO ()
run opts conn = do
  let (RecipeId rid) = Delete.id opts
  count <- execute conn "DELETE FROM recipes WHERE id = ? LIMIT 1" (Only rid)
  putStrLn (show count ++ " recipe(s) deleted")
