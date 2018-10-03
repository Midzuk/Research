{-# LANGUAGE OverloadedStrings #-}

module Csv.NodeCsv where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir

type Node = Int

type Long = Double
type Lat = Double

data NodeCsvOut = NodeCsvOut Node Long Lat deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "nodeId"
      <*> m .: "x" --Longitude
      <*> m .: "y" --Latitude

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> "/data/" <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls


type NodeCsv =
  Map.Map Node (Long, Lat)

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n long lat) = Map.insert n (long, lat)

encodeNodeCsv :: NodeCsv -> String
encodeNodeCsv nc = 
  "Node,Long,Lat"
    <> Map.foldrWithKey
      (\node_ (long_, lat_) str_ -> str_ <> "\n" <> show node_ <> "," <> show long_ <> "," <> show lat_)
        "" nc
