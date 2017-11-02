module Ingredient where
import Data.Monoid

import Options.Applicative
import Database.MySQL.Simple

data Command = Add deriving (Show, Eq)


addCommand :: Mod CommandFields Command
addCommand = command "add" ( info opts desc )
  where
    desc = progDesc "Add an ingredient to a recipe"
    opts = pure Add


commands :: Mod CommandFields Command
commands = addCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run Add _ = putStrLn "WORK IN PROGRESS"
