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

limit opts = 50 -- TODO make into an option later

type ResultTuple = (Int, Text.Text, Text.Text) -- id, name, description

run :: Options -> Connection -> IO ()
run opts conn = do
  printHeader
  withQueryResults opts conn $ \r -> do
    printRow "%-10d%45s%45s\n" r

printHeader = do
  let fmt = "%-10s%45s%45s\n"
  printf fmt ("RECIPE_ID" :: String) ("NAME" :: String) ("DESCRIPTION" :: String)
  putStrLn (replicate 100 '-')

printRow fmt row =
  let (id, name, description) = row in
  printf fmt (id :: Int) (Text.unpack name :: String) (take 45 (Text.unpack description) :: String)


withQueryResults opts =
  case Query.id opts of
    Just i -> foldById i
    Nothing -> foldQuery opts


foldById :: RecipeId -> Connection -> (ResultTuple -> IO ()) -> IO ()
foldById (RecipeId recipeId) conn f = do
  xs <- query conn "SELECT id, name, description FROM recipes WHERE id = ? LIMIT 1" (Only recipeId)
  forM_ xs f


foldQuery :: Options -> Connection -> (ResultTuple -> IO ()) -> IO ()
foldQuery opts conn f = do
  let g _ r = f r
      lim = limit opts :: Int
  case (fromName <$> name opts, fromDescription <$> description opts) of
    (Just n, Just d) -> fold conn "SELECT id, name, description FROM recipes WHERE name LIKE ? AND description LIKE ? LIMIT ?" (n, d, lim) () g
    (Just n, Nothing) -> fold conn "SELECT id, name, description FROM recipes WHERE name LIKE ? LIMIT ?" (n, lim) () g
    (Nothing, Just d) -> fold conn "SELECT id, name, description FROM recipes WHERE description LIKE ? LIMIT ?" (d, lim) () g
    (Nothing, Nothing) -> fold conn "SELECT id, name, description FROM recipes LIMIT ?" (Only lim) () g

fromName (Name n) = "%" ++ n ++ "%"
fromDescription (Description d) = "%" ++ d ++ "%"
