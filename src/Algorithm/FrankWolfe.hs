{-# LANGUAGE ViewPatterns #-}
module Algorithm.FrankWolfe where

import           Algorithm.Search (searchMin)
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Data.Vector      as V
import           Debug.Trace
import           Link



--交通フロー
type Flow = Double

type Trip = Map.Map OD Flow
type LinkFlow = Trip

type Time = Double
type Capacity = Double
type Alpha = Double
type Beta = Double
type LinkParameter = Map.Map OD (Time, Capacity, Alpha, Beta)

allOrNothing :: Trip -> Network -> LinkFlow
allOrNothing t n = go t Map.empty
  where
    p = shortestPath n

    go :: Trip -> LinkFlow -> LinkFlow
    go (Map.null -> True) lf = lf
    go (Map.deleteFindMin -> ((od, f), t1)) lf =
      case p Map.! od of
        Link (Graph s) _ -> go t1 $ V.foldr (\od1 lf1 -> Map.insertWith (+) od1 f lf1) lf s
        Link (Edge od) _ -> go t1 $ Map.insertWith (+) od f lf

type Tolerance = Double

--frankWolfe :: Tolerance -> Trip -> LinkParameter -> LinkFlow
frankWolfe e t lp = go lf_
  where
    lf_ = allOrNothing t $ makeNetwork lp

    go lf
      | abs ((sumTime lp lf2 - sumTime lp lf) / sum (diffLinkFlow lf2 lf)) < e = lf2
      | otherwise = go lf2
      where
        lf1 = allOrNothing t $ bpr lp lf
        x = searchMin (e / 100) (\x1 -> sumTime lp $ Map.unionWith (+) lf . Map.map (x1 *) $ diffLinkFlow lf1 lf) (0, 1)
        lf2 = Map.unionWith (+) lf . Map.map (x *) $ diffLinkFlow lf1 lf

diffLinkFlow :: LinkFlow -> LinkFlow -> LinkFlow
diffLinkFlow lf1 lf2 = Map.unionWith (+) lf1 $ Map.map negate lf2

--移動コストの更新
bpr :: LinkParameter -> LinkFlow -> Network
bpr lp _lf = go _lf (makeNetwork lp)
  where
    go (Map.null -> True) n = n
    go (Map.deleteFindMin -> ((od, f), lf)) n = go lf $ Map.insert od (Link (Edge od) (t * (1 + a * (f / c) ** b))) n
      where
        (t, c, a, b) = lp Map.! od

makeNetwork :: LinkParameter -> Network
makeNetwork = Map.mapWithKey (\od (t, c, a, b) -> Link (Edge od) t)

makeLinkParameter :: Capacity -> Alpha -> Beta -> Network -> LinkParameter
makeLinkParameter c a b = Map.mapWithKey (\od (Link _ t) -> (t, c, a, b))

sumTime :: LinkParameter -> LinkFlow -> Time
sumTime lp = Map.foldrWithKey g 0
  where
    g od f st = st + t * f * (1 + a / (b + 1) * (f / c) ** b)
      where
        (t, c, a, b) = lp Map.! od

