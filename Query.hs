{-# LANGUAGE OverloadedStrings #-}

module Query where
import Types

import Database.MySQL.Simple

import qualified Data.Text as Text
import Control.Monad
import Text.Printf

data Options = Options
  { id :: Maybe RecipeId
  , name :: Maybe Name
  , description :: Maybe Description }
  deriving (Show, Eq)


run :: Options -> Connection -> IO ()
run opts conn = do
  let fmt = "%-10d%45s%45s\n"
  xs <- query_ conn "SELECT id, name, description FROM recipes"

  -- Print Header
  printf "%-10s%45s%45s\n" ("RECIPE_ID" :: String) ("NAME" :: String) ("DESCRIPTION" :: String)
  putStrLn (replicate 100 '-')
  
  forM_ xs $ \ (id, name, description) -> do
    printf fmt (id :: Int) (Text.unpack name :: String) (take 45 (Text.unpack description) :: String)
    
