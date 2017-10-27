
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

connectInfo = (defaultConnectInfo { connectPassword = "abcabc", connectDatabase = "recipes" })

-- setupTables :: IO ()
setupTables conn = let q = "CREATE TABLE IF NOT EXISTS recipes (id INT NOT NULL AUTO_INCREMENT, name VARCHAR(100) NOT NULL, PRIMARY KEY (id))"
                   in execute_ conn q

hello :: IO Int
hello = do
  conn <- connect connectInfo
  setupTables conn
  [Only i] <- query_ conn "select count(*) from recipes"
  return i
