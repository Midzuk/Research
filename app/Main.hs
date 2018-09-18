{-# LANGUAGE ViewPatterns #-}
module Main where

import           Algorithm.FrankWolfe
import           Algorithm.Search
import           Csv.LinkCsv          (decodeLinkCsv, makeLinkCsv, encodeLinkCsv)
import           Csv.NodeCsv          (decodeNodeCsv, makeNodeCsv, encodeNodeCsv)
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Link
import           System.IO.Unsafe
import Csv.NetworkCsv (NetworkCsv(..), simplifyNetworkCsv)
import qualified System.Directory as Dir


--import           System.Random
--import           Data.Semigroup

main :: IO ()
main = do
  lc <- decodeLinkCsv "output_links.csv"
  nc <- decodeNodeCsv "output_nodes.csv"
  let nwc = NetworkCsv lc nc
  let NetworkCsv slc snc = simplifyNetworkCsv nwc
  
  cd <- Dir.getCurrentDirectory
  writeFile (cd <> "/output/simpleLink.csv") $ encodeLinkCsv slc
  writeFile (cd <> "/output/simpleNode.csv") $ encodeNodeCsv snc
  --print $ shortestPath network
  --print $ frankWolfe 0.01 trip linkParameter
--searchMin 0.01 (\x -> (x - 1) ^ 2 + 12) (0,10)

trip :: Trip
trip =
  Map.fromList
    [ (1 :->: 3, 5)
    --, (2 :->: 4, 10)
    ]



network :: Network
network = Map.union n1 n2
  where
    n1 =
      networkFromList
        [ (1 :->: 2, 3)
        , (1 :->: 3, 4)
        , (1 :->: 4, 5)
        , (2 :->: 3, 5)
        --, (2 :->: 4, 6)
        , (3 :->: 4, 7)
        ]
    n2 = inverseNetwork n1

linkParameter :: LinkParameter
linkParameter = makeLinkParameter 1 10 2 network

{-
main = go $ shortestPath graph'

  where
    go (Map.null -> True) = return ()
    go (Map.deleteFindMin -> ((od, l@(Link g c)), n)) =
      do
        putStrLn (show od <> "---Cost : " <> show d <> "---" <> show g)
        putStrLn (" ")
        go n

graph' :: Network
graph' =
  networkFromList
    [ (OD 0 1, 30)
    , (OD 0 2, 2)
    , (OD 0 3, 7)
    , (OD 1 2, 6)
    , (OD 2 3, 9)
    , (OD 2 4, 4)
    , (OD 3 4, 2)
    , (OD 4 5, 1)
    , (OD 3 5, 4)
    , (OD 5 6, 3)
    , (OD 4 6, 2)
    , (OD 3 7, 7)
    , (OD 2 7, 6)
    , (OD 1 8, 9)
    , (OD 7 8, 10)
    , (OD 4 9, 21)
    , (OD 5 9, 1)
    , (OD 8 9, 1)
    , (OD 0 10, 3)
    , (OD 10 2, 2)
    , (OD 10 3, 7)
    , (OD 11 2, 6)
    , (OD 2 11, 9)
    , (OD 12 4, 4)
    , (OD 3 12, 2)
    , (OD 13 5, 1)
    , (OD 3 13, 4)
    , (OD 14 6, 3)
    , (OD 4 14, 2)
    , (OD 15 7, 7)
    , (OD 2 15, 6)
    , (OD 16 8, 9)
    , (OD 7 16, 10)
    , (OD 4 16, 21)
    , (OD 16 9, 20)
    , (OD 16 10, 1)
    ]


  Graph . Set.fromList $ uncurry Link <$>
    zip (uncurry OD <$> [(a, b) | a <- [1..20], b <- [21..40]]) (Edge <$> randomRs (1,100) (mkStdGen 1))
    [ (OD 0 1, 3)
    , (OD 0 2, 2)
    , (OD 0 3, 7)
    , (OD 1 2, 6)
    , (OD 2 3, 9)
    , (OD 2 4, 4)
    , (OD 3 4, 2)
    , (OD 4 5, 1)
    , (OD 3 5, 4)
    ]
-}
