{-# LANGUAGE OverloadedStrings #-}

module Query where
import Types

import Database.MySQL.Simple

import Data.Text as Text
import Control.Monad

data Options = Options
  { id :: Maybe RecipeId
  , name :: Maybe Name
  , description :: Maybe Description }
  deriving (Show, Eq)


run :: Options -> Connection -> IO ()
run opts conn = do
  xs <- query_ conn "SELECT id, name, description FROM recipes"
  forM_ xs $ \ (id, name, description) ->
    putStrLn $ show (id :: Int) ++ ", " ++ Text.unpack name ++ ", " ++ Text.unpack description
    
