module Ingredient where
import Options.Applicative

import Database.MySQL.Simple

data Command = Add deriving (Show, Eq)


addCommand :: Mod CommandFields Command
addCommand = command "add" ( info opts desc )
  where
    desc = progDesc "Add an ingredient to a recipe"
    opts = pure Add


commandParser :: Parser Command
commandParser = hsubparser addCommand

run :: Command -> Connection -> IO ()
run Add _ = putStrLn "WORK IN PROGRESS"
