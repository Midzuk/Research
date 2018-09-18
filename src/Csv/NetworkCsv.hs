{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}


module Csv.NetworkCsv where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state)
import           Csv.LinkCsv                (LinkCsv, LinkCond, LinkWithCond)
import           Csv.NodeCsv                (NodeCsv)
import qualified Data.Map.Strict            as Map
import qualified Data.Vector                as V
import           Link
import           Data.Maybe (fromJust)
import qualified Data.Set as Set



data NetworkCsv = NetworkCsv LinkCsv NodeCsv deriving (Show)
type NetworkCsvInput = NetworkCsv
type NetworkCsvOutput = NetworkCsv

simplifyNetworkCsv :: NetworkCsv -> NetworkCsv
simplifyNetworkCsv (NetworkCsv lci nci) =
  let
    lco = simplifyLinkCsv lci
    nodes = f lco :: Set.Set Node
    nco = Map.filterWithKey (\node_ _ -> Set.member node_ nodes) nci
  in
    NetworkCsv lco nco

  where
    f lco = V.foldr (\(composeLink -> org_ :->: dest_, _) nodes_ -> (Set.insert org_ . Set.insert dest_) nodes_) Set.empty lco

simplifyLinkCsv :: LinkCsv -> LinkCsv
simplifyLinkCsv = V.foldr f []
  where
    --Nothing (交差点および行き止まり) を無視して和を計算
    f :: LinkWithCond -> LinkCsv -> LinkCsv
    f lwc@(link, cond) lco = V.cons (fromJust $ g1 lwc lco <> Just link <> g2 lwc lco, cond) lco
    
    g1 :: LinkWithCond -> LinkCsv -> Maybe Link
    g1 lwc@(link, cond) lco =
      case V.filter (\(link_, cond_) -> isNextLink link_ link && cond == cond_) lco of
        [(link1, _)] -> Just link1
        _            -> Nothing

    g2 :: LinkWithCond -> LinkCsv -> Maybe Link
    g2 lwc@(link, cond) lco =
      case V.filter (\(link_, cond_) -> isNextLink link link_ && cond == cond_) lco of
        [(link2, _)] -> Just link2
        _            -> Nothing

    
{-
--simplify :: Link -> LinkCsv -> NetworkCsv -> (LinkCsv, NetworkCsv)
simplify :: LinkWithCond -> NetworkCsvInput -> NetworkCsvOutput -> (NetworkCsvInput, NetworkCsvOutput)
simplify lwc@(l@(Link g _), cond) nwci@(NetworkCsv lci nci) nwco@(NetworkCsv lco nco)
  | Map.member org nco && Map.member dest nco = (nwci, NetworkCsv (V.cons lwc lco) nco)
  --simplify destination side
  | Map.member org nco =
    let
      (nlc, olc) = V.partition (\(l2, _) -> isNextLink l l2) lci :: (LinkCsv, LinkCsv)
    in
      if
        | V.length nlc == 1 ->
          case nlc of
            [(l2, cond2)] ->
              if cond == cond2
                then simplify ((l <> l2), cond) (NetworkCsv olc nci) nwco
                else
                  let
                    ll = nci Map.! dest
                  in
                    (nwci, NetworkCsv (V.cons lwc lco) (Map.insert dest ll nco))
        | otherwise -> 
          let
            ll = nci Map.! dest
          in
            (nwci, NetworkCsv (V.cons lwc lco) (Map.insert dest ll nco))
  where
    od@(org :->: dest) = compose g
-}
{-
--simplify origin side
simplifyOrg :: Link -> State LinkCsv Link
simplifyOrg l =
  state $ \lc ->
    let
      (nextLc, otherLc) = V.partition (\(l1, _) -> isNextLink l1 l) lc :: (LinkCsv, LinkCsv)
    in
      if
        | V.length nextLc == 1 ->
          case nextLc of
            [nl] -> undefined--runState (simplifyOrg otherLc) nl <> l
        | otherwise -> undefined


  where
    (nextLc, otherLc) = V.partition (\(l1, _) -> isNextLink l1 l) lc :: (LinkCsv, LinkCsv)

--simplify destination side
simplifyDest :: OD -> NetworkCsv -> NetworkCsv
simplifyDest = undefined

hasNode :: OD -> Node -> Bool
(org :->: dest) `hasNode` n
  | org == n = True
  | dest == n = True
  | otherwise = False
-}
{-
--Whether the node is a dead-end or not.
isEnd :: Node -> LinkCsv -> Bool
isEnd n lc = V.length (V.filter f lc) < 2
  where
    f (od, _) = od `hasNode` n

--Whether the node is an intersection or not.
isIntersect :: Node -> LinkCsv -> Bool
isIntersect n lc = V.length (V.filter f lc) > 2
  where
    f (od, _) = od `hasNode` n
-}
