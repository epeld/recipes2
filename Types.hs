module Types where

newtype Name = Name String deriving (Show, Eq)
newtype Description = Description String deriving (Show, Eq)
newtype RecipeId = RecipeId Int deriving (Show, Eq)
