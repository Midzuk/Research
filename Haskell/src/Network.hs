--{-# LANGUAGE FlexibleInstances    #-}
--{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns    #-}

module Network where

import qualified Data.Map.Strict as Map
import           Data.Semigroup
import qualified Data.Set        as Set
import qualified Data.Vector     as V
import           Debug.Trace



type Node = Int

type Cost = Double

data OD = Node :->: Node deriving (Eq, Show)
infixr 5 :->:

{-
instance Eq OD where
  (==) (n1 :->: n2) (n3 :->: n4) = n1 == n3 && n2 == n4
-}

instance Ord OD where
  compare od1@(n1 :->: n2) od2@(n3 :->: n4)
    | od1 == od2 = EQ
    | otherwise =
      case compare n1 n3 of
        EQ ->
          if n2 < n4 then LT else GT
        o -> o



instance Semigroup OD where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup OD Error."

data Link = Link Graph Cost deriving Show

instance Eq Link where
  Link g1 c1 == Link g2 c2 = g1 == g2 && c1 == c2

instance Ord Link where
  compare l1@(Link g1 c1) l2@(Link g2 c2)
    | l1 == l2 = EQ
    | c1 == c2 =
      if g1 < g2 then LT else GT
    | c1 < c2 = LT
    | otherwise = GT

instance Semigroup Link where
  Link g1 c1 <> Link g2 c2 = Link (g1 <> g2) (c1 + c2)

type Network = Map.Map OD Link

type Path = Network

data Graph = Edge OD | Graph (V.Vector OD) deriving Show

compose :: Graph -> OD
compose (Edge od) = od
compose (Graph v) = foldr1 (<>) v

composeLink :: Link -> OD
composeLink (Link g _) = compose g

instance Eq Graph where
  g1 == g2 = compose g1 == compose g2

instance Ord Graph where
  compare g1 g2
    | g1 == g2 = EQ
    | compose g1 < compose g2 = LT
    | otherwise = GT

instance Semigroup Graph where
  Edge od1 <> Edge od2 = Graph [od1, od2]
  Edge od <> Graph v = Graph $ V.cons od v
  Graph v <> Edge od = Graph $ V.snoc v od
  Graph v1 <> Graph v2 = Graph $ v1 <> v2



networkFromList :: [(OD, Cost)] -> Network
networkFromList odcs = Map.fromList $ (\(od, c) -> (od, Link (Edge od) c)) <$> odcs



insertLink :: Link -> Network -> Network
insertLink l@(Link (compose -> od) _) n =
  case n Map.!? od of
    Nothing -> Map.insert od l n
    Just _  -> n



isNextLink :: Link -> Link -> Bool
isNextLink (Link g1 _) (Link g2 _) = isNextGraph g1 g2



isNextGraph :: Graph -> Graph -> Bool
isNextGraph (Edge od1) (Edge od2)       = isNextOD od1 od2
isNextGraph (Edge od) g@(Graph v)       = isNextOD od (compose g) && notElem (invertOD od) v -- && V.notElem od v
isNextGraph g@(Graph v) (Edge od)       = isNextOD (compose g) od && notElem (invertOD od) v -- && V.notElem od v
isNextGraph g1@(Graph v1) g2@(Graph v2) = isNextOD (compose g1) (compose g2) && not (any (`elem` v1) v2) && not (any (`elem` v1) (invertOD <$> v2))

--Set.null (Set.intersection (nodeSet g1) (nodeSet g2))
{-
nodeSet :: Graph -> Set.Set Node
nodeSet (Edge (n1 :->: n2)) = Set.fromList [n1, n2]
nodeSet (Graph v@(V.head -> (n1 :->: _))) = V.foldr (\(_ :->: n2) s -> Set.insert n2 s) [n1] v
-}

isNextOD :: OD -> OD -> Bool
isNextOD (n1 :->: n2) (n3 :->: n4) = n2 == n3 && n1 /= n4



shortestNetwork :: Network -> Network
shortestNetwork n_ = go n_ Map.empty
  where
    go :: Network -> Path -> Path
    go (Map.null -> True) p = p
    go n@(minimum -> l@(Link g _)) p =
      case p Map.!? od of
        Just _  -> go n1 p
        Nothing -> go n3 $ Map.insert od l p
      where
        od = compose g
        n1 = Map.delete od n
        odls1 = Map.assocs $ Map.filter (isNextLink l) p
        n2 = foldr (\(od1, l1) n2 -> Map.insertWith min (od <> od1) (l <> l1) n2) n1 odls1 ----
        odls2 = Map.assocs $ Map.filter (`isNextLink` l) p
        n3 = foldr (\(od1, l1) n2 -> Map.insertWith min (od1 <> od) (l1 <> l) n2) n2 odls2 ----



inverseNetwork :: Network -> Network
inverseNetwork = Map.foldrWithKey (\(n1 :->: n2) (Link _ c) n -> Map.insert (n2 :->: n1) (Link (Edge (n2 :->: n1)) c) n) Map.empty

invertOD :: OD -> OD
invertOD (n1 :->: n2) = n2 :->: n1

overlap :: Graph -> Graph -> Bool
overlap (Edge od1) (Edge od2) = od1 == od2
overlap (Edge od) g@(Graph v) = od `elem` v
overlap g@(Graph v) (Edge od) = od `elem` v
overlap g1@(Graph v1) g2@(Graph v2) = any (`elem` v1) v2

overlapLink :: Link -> Link -> Bool
overlapLink (Link g1 _) (Link g2 _) = overlap g1 g2

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""