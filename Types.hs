module Types where

newtype Name = Name String deriving (Show, Eq)
newtype Unit = Unit String deriving (Show, Eq)
newtype Amount = Amount Int deriving (Show, Eq)
newtype Description = Description String deriving (Show, Eq)
newtype RecipeId = RecipeId Int deriving (Show, Eq)


recipeIdInt :: RecipeId -> Int
recipeIdInt (RecipeId int) = int
