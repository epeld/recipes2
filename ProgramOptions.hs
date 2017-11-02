module ProgramOptions where
import Options.Applicative

import Data.Monoid

import Types

description :: Parser (Maybe Description)
description = argument (maybeStr2 Description) (metavar "DESCRIPTION" <> value Nothing)

descriptionOption = option (maybeStr2 Description)
  ( long "description" <>
    short 'd' <>
    metavar "DESCRIPTION" <>
    value Nothing <>
    help "recipe description" )

name :: Parser Name
name = argument (Name <$> str) (metavar "NAME")

nameOption = option (maybeStr2 Name)
  ( long "name" <>
    short 'n' <>
    metavar "NAME" <>
    value Nothing <>
    help "recipe name" )


recipeId :: Parser RecipeId
recipeId = argument ( RecipeId <$> auto ) (metavar "RECIPE_ID")

recipeIdOption :: Parser (Maybe RecipeId)
recipeIdOption = option ( maybeAuto RecipeId )
  ( long "id" <>
    short 'i' <>
    metavar "RECIPE_ID" <>
    value Nothing <>
    help "recipe id" )

maybeStr :: ReadM (Maybe String)
maybeStr = Just <$> str

maybeStr2 f = Just <$> (f <$> str)

maybeAuto f = Just <$> (f <$> auto)
