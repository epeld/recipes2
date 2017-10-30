{-# LANGUAGE OverloadedStrings #-}

module Query where
import Types

import Database.MySQL.Simple

import qualified Data.Text as Text
import Control.Monad
import Text.Printf
import Control.Applicative

data Options = Options
  { id :: Maybe RecipeId
  , name :: Maybe Name
  , description :: Maybe Description }
  deriving (Show, Eq)


type ResultTuple = (Int, Text.Text, Text.Text) -- id, name, description

run :: Options -> Connection -> IO ()
run opts conn = do
  xs <- selectQuery opts conn

  -- Print Header
  printf "%-10s%45s%45s\n" ("RECIPE_ID" :: String) ("NAME" :: String) ("DESCRIPTION" :: String)
  putStrLn (replicate 100 '-')
  
  let fmt = "%-10d%45s%45s\n"
  forM_ xs $ \ (id, name, description) -> do
    printf fmt (id :: Int) (Text.unpack name :: String) (take 45 (Text.unpack description) :: String)
    

--selectQuery :: Options -> (Query, Params)
selectQuery opts =
  case Query.id opts of
    Just i -> selectById i
    Nothing -> selectMultipleQuery opts


selectById :: RecipeId -> Connection -> IO [ResultTuple]
selectById (RecipeId recipeId) conn = do
  query conn "SELECT id, name, description FROM recipes WHERE id = ? LIMIT 1" (Only recipeId)


selectMultipleQuery :: Options -> Connection -> IO [ResultTuple]
selectMultipleQuery opts conn = do
  case (fromName <$> name opts, fromDescription <$> description opts) of
    (Just n, Just d) -> query conn "SELECT id, name, description FROM recipes WHERE name LIKE ? AND description LIKE ? LIMIT 10" [n, d]
    (Just n, Nothing) -> query conn "SELECT id, name, description FROM recipes WHERE name LIKE ? LIMIT 10" [n]
    (Nothing, Just d) -> query conn "SELECT id, name, description FROM recipes WHERE description LIKE ? LIMIT 10" [d]
    (Nothing, Nothing) -> query_ conn "SELECT id, name, description FROM recipes LIMIT 10"

fromName (Name n) = "%" ++ n ++ "%"
fromDescription (Description d) = "%" ++ d ++ "%"
