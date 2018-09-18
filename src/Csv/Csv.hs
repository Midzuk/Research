{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Csv.Csv where

{-
--import qualified Data.Text        as T
import qualified Data.ByteString.Lazy as B
import           Data.Csv
import qualified Data.Map.Strict      as Map
import qualified Data.Text.IO         as T
import qualified Data.Vector          as V
--import           GHC.Generics         (Generic)
import qualified System.Directory     as Dir

type Node = Int
type Origin = Node
type Destination = Node

-- "~/Programming/Haskell/research/data"
data TripCsv = TripCsv Origin Destination Trip

data LinkCsv =
  LinkCsv Node Node Time Capacity Alpha Beta deriving Show

instance FromNamedRecord LinkCsv where
  parseNamedRecord m =
    LinkCsv
      <$> m .: "origin"
      <*> m .: "destination"
      <*> m .: "time"
      <*> m .: "capacity"
      <*> m .: "alpha"
      <*> m .: "beta"



g :: FilePath -> IO ()
g fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> "/data/" <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector LinkCsv)
  let lp = V.foldr (\(LinkCsv n1 n2 t c a b) n -> Map.insert (n1 :->: n2) (t, c, a, b) n) Map.empty ls

  return ()
-}
