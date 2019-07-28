module Algorithm.Search
  (searchMin
  )
  where

type Tolerance = Double
type X = Double
type Y = Double
type Range = (X, X)

--黄金比
phi :: Double
phi = (1 + sqrt 5) / 2

searchMin :: Tolerance -> (X -> Y) -> Range -> X
searchMin e f (x1_, x2_) = go False x3_ y3_ f (x1_, x2_)
  where
    x3_ = (x1_ + phi * x2_) / (phi + 1)
    y3_ = f x3_

    go :: Bool -> X -> Y -> (X -> Y) -> Range -> Double
    go b x3 y3 f (x1, x2)
      | abs (x2 - x1) < e = x3
      | b =
        let
          x4 = (x1 + phi * x2) / (phi + 1)
          y4 = f x4
        in
          if y4 < y3
            then go True x4 y4 f (x3, x2)
            else go False x3 y3 f (x1, x4)
      | otherwise =
        let
          x4 = (phi * x1 + x2) / (1 + phi)
          y4 = f x4
        in
          if y4 < y3
            then go False x4 y4 f (x1, x3)
            else go True x3 y3 f (x4, x2)
