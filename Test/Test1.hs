import AI.RBM
import AI.Data.MNIST

import qualified Data.Array.Repa as R
import Data.Array.Repa(Z(..), (:.)(..), transpose, force, extent)
import Control.Applicative((<$>))

main = do
  let cdn = 20
      e = 0.07
      numSamples = 100
      numTrainExs = 4
      nh = 50
  trainSet <- map fst . (filter ((==8) . snd)) . flattenSet <$> getTrainingSet "/saiko/data/digits"
  let batch = force $ transpose $ foldr1 (R.++) $ take numTrainExs trainSet
      (Z :. _ :. nv) = extent batch
  rbm <- mkRBM nv nh
  rbm' <- trainBatch cdn numSamples e rbm batch
  return ()
  -- print rbm'
