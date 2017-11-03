module Db where
import Data.Monoid
import Control.Monad

import Options.Applicative
import Database.MySQL.Simple

import Schema

data Command = Schema | Drop deriving (Show, Eq)


schemaCommand :: Mod CommandFields Command
schemaCommand = command "schema" ( info opts desc )
  where
    desc = progDesc "Print the Database Schema"
    opts = pure Schema


dropCommand :: Mod CommandFields Command
dropCommand = command "drop" ( info opts desc )
  where
    desc = progDesc "Drop Tables"
    opts = pure Drop


commands :: Mod CommandFields Command
commands = schemaCommand <> dropCommand


commandParser :: Parser Command
commandParser = hsubparser commands


run :: Command -> Connection -> IO ()
run Schema _ = forM_ Schema.tables printNamedSchema


printNamedSchema :: NamedSchema -> IO ()
printNamedSchema (name, schema) = do
  printComment name
  printQuery schema


printQuery :: Query -> IO ()
printQuery = putStrLn . queryToString

printComment :: String -> IO ()
printComment comment = putStrLn ("\n%% " ++ comment)
