module AI.RBM where

import Data.Vector as Vector

data RBM = RBM { rbm_numVisible :: Int
               , rbm_numHidden :: Int
               , rbm_weights :: Vector.Vector Double
               } derives (Eq, Show)

