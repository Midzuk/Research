{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns    #-}

module Algorithm.Fratar
    ( fratar
    ) where

import           Control.Monad.Identity
import           Data.Array.Repa        hiding ((++))
import qualified Data.Vector.Unboxed    as U
import           Debug.Trace
import           Prelude                hiding (map)
import           Unsafe.Coerce

type ODMatrix = Array U DIM2 Double

type TripGeneration = Array U DIM1 Double --発生交通量
type TripAttraction = Array U DIM1 Double --集中交通量

type ConvergenceCriterion = Double --収束基準

fratar :: ConvergenceCriterion -> ODMatrix -> TripGeneration -> TripAttraction -> Identity ODMatrix
fratar cc odm@(extent -> Z :. n :. _) gs' as' =
  do
    ts' <- U.mapM (uncurry tM') . U.fromList $ (,) <$> [0 .. n - 1] <*> [0 .. n - 1]
    let odm' = fromUnboxed (Z :. n :. n) ts'
    let bs = map (\r -> abs (r - 1) < cc) $ odm' /^ odm
    let b = foldAllS (&&) True bs
    if b
      then
        return odm'
      else
        fratar cc odm' gs' as'

  where
    asM =
      sumP . transpose $ odm

    gsM =
      sumP odm

    tM' i j =
      do
        let g'_i = gs' ! (Z :. i)
        gs <- gsM
        let g_i = gs ! (Z :. i)

        let a'_j = as' ! (Z :. j)
        as <- asM
        let a_j = as ! (Z :. j)

        tfis <- U.mapM (\j_ -> (t i j_ *) <$> faM j_) [0 .. n - 1]
        let l_i = g_i / U.sum tfis

        tfjs <- U.mapM (\i_ -> (t i_ j *) <$> fgM i_) [0 .. n - 1]
        let l_j = a_j / U.sum tfjs

        return $ t i j * g'_i / g_i * a'_j / a_j * (l_i + l_j) / 2

      where
        t i_ j_ =
          odm ! (Z :. i_ :. j_)

        fgM i_ =
          do
            gs <- gsM
            let g_i = gs ! (Z :. i_)
            let g'_i = gs' ! (Z :. i_)
            return $ g'_i / g_i

        faM j_ =
          do
            as <- asM
            let a_j = as ! (Z :. j_)
            let a'_j = as' ! (Z :. j_)
            return $ a'_j / a_j

