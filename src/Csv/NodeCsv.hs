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

type Lat = Double
type Long = Double

data NodeCsvOut = NodeCsvOut Node Lat Long deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "nodeId"
      <*> m .: "x" --Latitude
      <*> m .: "y" --Longitude

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> "/data/" <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls


type NodeCsv =
  Map.Map Node (Lat, Long)

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv ncos = V.foldr f Map.empty ncos
  where
    f (NodeCsvOut n lat long) = Map.insert n (lat, long)

{-
type NodeCsvMap =
  Map.Map Node (Lat, Long)

makeNodeCsvMap :: V.Vector LinkCsv -> LinkCsvMap
makeNodeCsvMap ls = V.foldr f Map.empty ls
  where
    f (LinkCsv org dest dist highway oneway bridge width) =
      Map.insert (org :->: dest) (dist, (highway, oneway, bridge, width))
-}
