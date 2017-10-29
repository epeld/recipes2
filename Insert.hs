{-# LANGUAGE OverloadedStrings #-}

module Insert where
import Types
import Database.MySQL.Simple

data Options = Options
  { name :: Name
  , description :: (Maybe Description) }
  deriving (Show, Eq)


run :: Options -> Connection -> IO ()
run opts conn = do
  let d = case description opts of
        Nothing -> "NULL"
        Just (Description dc) -> dc
      (Name n) = name opts
  count <- execute conn "INSERT INTO recipes (name, description) VALUES (?,?)" [n, d]
  putStrLn (show count ++ " recipe(s) added")
