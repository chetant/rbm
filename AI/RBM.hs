module AI.RBM where

import qualified Data.Array.Repa as R
import Data.Array.Repa(Z(..), All(..), (:.)(..), (*^), (+^))

type Vector = R.Array R.DIM1 Double
type Matrix = R.Array R.DIM2 Double

data RBM = RBM { 
      rbm_visBias :: Vector -- i
    , rbm_hidBias :: Vector -- j
    , rbm_weights :: Matrix -- i * j
    } deriving (Eq, Show)

dim1 size = Z :. size
dim2 rows cols = Z :. rows :. cols

fromLists :: Real a => ([a], [a], [[a]]) -> RBM
fromLists (vs, hs, ws)
    | length ws /= lh = error $ "Malformed RBM!"
    | otherwise = RBM { rbm_visBias = R.fromList (dim1 lv) (map realToFrac vs)
                      , rbm_hidBias = R.fromList (dim1 lh) (map realToFrac hs)
                      , rbm_weights = R.fromList (dim2 lh lv) (concatMap (map realToFrac) ws)
                      }
    where lv = length vs
          lh = length hs

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

vecFromList :: Real a => [a] -> Vector
vecFromList xs = R.fromList (dim1 $ length xs) (map realToFrac xs)

mtxFromList :: Real a => (Int, Int) -> [[a]] -> Matrix
mtxFromList (r, c) xs = R.fromList (dim2 r c) (concatMap (map realToFrac) xs)

mXv :: Matrix -> Vector -> Vector
mXv m v = R.sum (vm *^ m)
    where (_ :. lv) = R.extent v
          (_ :. hm :. wm) = R.extent m
          vm = R.extend (Z :. hm :. All) v

transpose :: Matrix -> Matrix
transpose m = R.backpermute extent' swap m
    where swap (Z :. i :. j) = Z :. j :. i
          extent' = swap (R.extent m)

getHiddenPGivenV :: RBM -> Vector -> Vector
getHiddenPGivenV rbm v = R.map sigmoid $ hbs +^ (ws `mXv` v)
    where hbs = rbm_hidBias rbm
          ws = rbm_weights rbm

getVisiblePGivenH :: RBM -> Vector -> Vector
getVisiblePGivenH rbm h = R.map sigmoid $ vbs +^ (ws `mXv` h)
    where vbs = rbm_visBias rbm
          ws = transpose $ rbm_weights rbm

-- sample binary vector given probabilities
sampleB :: Vector -> Vector
sampleB ps = undefined
