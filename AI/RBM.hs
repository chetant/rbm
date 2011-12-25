{-# LANGUAGE BangPatterns #-}
module AI.RBM 
(
 RBM
,vec2Mtx
,mkRBM
,fromLists
,vecFromList
,mtxFromList
,trainBatch
,reconstruct
,dream
,showVec
,showMat
,visualizeWeights
) where

import Control.Applicative((<$>))

import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Normal

import qualified Data.Array.Repa as R
import Data.Array.Repa(transpose, Z(..), All(..), (:.)(..), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Algorithms.Matrix(multiplyMM)

import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.AreaSpots
import Graphics.Rendering.Chart.Plot.PixelMap
import Data.Colour.SRGB
import Data.Accessor

import Text.Printf

type Vector = R.Array R.DIM1 Double
type Matrix = R.Array R.DIM2 Double

data RBM = RBM { 
      rbm_weights  :: !Matrix -- j * (i+1) the +1 is the hidden bias
    , rbm_weightsT :: !Matrix -- i * (j+1) the +1 is the visible bias
    } deriving (Eq, Show)

dim1 size = Z :. size
dim2 rows cols = Z :. rows :. cols

-- -# INLINE prepend #-}
prepend :: Vector -> Matrix -> Matrix
prepend v m = transpose (vec2Mtx v R.++ (transpose m))

-- -# INLINE vec2Mtx #-}
vec2Mtx :: Vector -> Matrix
vec2Mtx v = R.extend (Z :. All :. (1 :: Int)) v

mkRBM :: Int -> Int -> IO RBM
mkRBM v h = do
  let vrow = sequence (replicate v (sample $ normal 0.0 0.1)) :: IO [Double]
  ws <- sequence $ replicate h vrow
  let vs = replicate v 0.0
      hs = replicate h 0.0
  return $ fromLists (vs, hs, ws)

fromLists :: Real a => ([a], [a], [[a]]) -> RBM
fromLists (vs, hs, ws)
    | length ws /= lh = error $ "Malformed RBM!"
    | otherwise = RBM { rbm_weights  = h0s R.++ ws'
                      , rbm_weightsT = v0s R.++ (transpose ws') }
    where lv = length vs
          lh = length hs
          ws' = R.fromList (dim2 lh lv) (concatMap (map realToFrac) ws)
          v0s = R.fromList (dim2 lv 1) (map realToFrac vs)
          h0s = R.fromList (dim2 lh 1) (map realToFrac hs)

-- -# INLINE sigmoid #-}
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- -# INLINE vecFromList #-}
vecFromList :: Real a => [a] -> Vector
vecFromList xs = R.fromList (dim1 $ length xs) (map realToFrac xs)

-- -# INLINE mtxFromList #-}
mtxFromList :: Real a => (Int, Int) -> [[a]] -> Matrix
mtxFromList (r, c) xs = R.fromList (dim2 r c) (concatMap (map realToFrac) xs)

-- -# INLINE mXv #-}
mXv :: Matrix -> Vector -> Vector
mXv m v = R.sum (vm *^ m)
    where (_ :. hm :. _) = R.extent m
          vm = R.extend (Z :. hm :. All) v

-- -# INLINE oneVec #-}
oneVec :: Vector
oneVec = R.fromFunction (Z :. 1) (const 1)

-- -# INLINE dupVec #-}
dupVec :: Int -> Vector -> Matrix
dupVec n v = R.extend (Z :. n :. All) v

getHiddenPGivenV :: RBM -> Vector -> Vector
getHiddenPGivenV rbm v = R.map sigmoid $ (ws `mXv` (oneVec R.++ v))
    where ws = rbm_weights rbm

getVisiblePGivenH :: RBM -> Vector -> Vector
getVisiblePGivenH rbm h = R.map sigmoid $ (ws `mXv` (oneVec R.++ h))
    where ws = rbm_weightsT rbm

-- -# INLINE getHiddenPGivenV #-}
getHiddenPGivenVs :: RBM -> Matrix -> Matrix
getHiddenPGivenVs rbm vs = R.map sigmoid $ R.sum (wm *^ vm)
    where ws = rbm_weights rbm
          (Z :. j :. _) = R.extent ws
          (Z :. n :. _) = R.extent vs
          oneV = R.fromFunction (Z :. n :. 1) (const 1)
          wm  = R.extend (Z :. n   :. All :. All) ws
          vm  = R.extend (Z :. All :. j   :. All) (oneV R.++ vs)

-- -# INLINE getVisiblePGivenHs #-}
getVisiblePGivenHs :: RBM -> Matrix -> Matrix
getVisiblePGivenHs rbm hs = R.map sigmoid $ R.sum (wm *^ hm)
    where ws = rbm_weightsT rbm
          (Z :. i :. _) = R.extent ws
          (Z :. n :. _) = R.extent hs
          oneH = R.fromFunction (Z :. n :. 1) (const 1)
          wm  = R.extend (Z :. n   :. All :. All) ws
          hm  = R.extend (Z :. All :. i   :. All) (oneH R.++ hs)

-- -# INLINE sampleP #-}
sampleP :: Vector -> IO Vector
sampleP ps = R.fromList (R.extent ps) <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps)

-- -# INLINE samplePN #-}
samplePN :: Vector -> Int -> IO Matrix
samplePN ps n = R.fromList (Z :. n :. l) <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps')
          (_ :. l) = R.extent ps
          ps' = R.extend (Z :. n :. All) ps

-- -# INLINE samplePNs #-}
samplePNs :: Matrix -> IO Matrix
samplePNs ps = R.fromList ext <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps)
          ext = R.extent ps

-- -# INLINE corr #-}
-- |Build expected value of <vh> given V(H) and `n` sampled H's(Vs)
corr :: Vector -> Matrix -> Matrix
corr v hs = sh `prepend` (sv R.++ (R.sum $ vm *^ hm))
    where (_ :. n :. j) = R.extent hs
          (_ :. i) = R.extent v
          hsT = transpose hs
          vm = R.extend (Z :. All :.   j :.   n) v
          hm = R.extend (Z :. i   :. All :. All) hsT
          sv = vec2Mtx v -- i,1
          sh = oneVec R.++ (R.sum hsT) -- <h> j + 1

-- -# INLINE corrs #-}
corrs :: Matrix -> Matrix -> Matrix
corrs vs hs = sh `prepend` (sv R.++ (R.sum $ vm *^ hm))
    where (Z :. n :. j) = R.extent hs
          (Z :. _ :. i) = R.extent vs
          vsT = transpose vs
          hsT = transpose hs
          vm = R.extend (Z :. All :.   j :. All) vsT
          hm = R.extend (Z :. i   :. All :. All) hsT
          sv = vec2Mtx (R.sum vsT) -- i,1
          sh = oneVec R.++ (R.sum hsT) -- <h> j + 1

-- -# INLINE mkWtConst #-}
mkWtConst :: RBM -> Double -> Matrix
mkWtConst r e = R.fromFunction (Z :. (h+1) :. w) (const e)
    where (Z :. h  :. w) = R.extent (rbm_weightsT r)

-- -# INLINE updateWeights #-}
updateWeights :: RBM -> Matrix -> RBM
updateWeights rbm delWt = rbm { rbm_weights  = wt
                              , rbm_weightsT = wtT }
    where dwtT  = R.traverse delWt (\(Z :. h :. w) -> (Z :. (h-1) :. w)) (\f (Z :. i :. j) -> f (Z :. (i+1) :. j)) -- (i+1)x(j+1) -> (i)x(j+1)
          dwt = R.traverse delWt (\(Z :. h :. w) -> (Z :. (w-1) :. h)) (\f (Z :. i :. j) -> f (Z :. j :. (i+1))) -- (i+1)x(j+1) -> (j)x(i+1)
          wt  = R.force $ rbm_weights  rbm +^ dwt
          wtT = R.force $ rbm_weightsT rbm +^ dwtT

-- -# INLINE cdLearn #-}
cdLearn :: Int -> Matrix -> RBM -> Matrix -> IO RBM
cdLearn n epsilon rbm pvs = do
  let (Z :. numSamples :. _) = R.extent pvs
      ns = mkWtConst rbm (realToFrac numSamples)
  vs <- samplePNs pvs
  hs <- samplePNs (getHiddenPGivenVs rbm vs)
  let posCor = corrs vs hs
      negRun !n hs = do
        vs' <- samplePNs (getVisiblePGivenHs rbm hs)
        hs' <- samplePNs (getHiddenPGivenVs rbm vs')
        case n of
          0 -> return $ corrs vs' hs'
          _ -> negRun (n-1) hs'
  negCor <- negRun n hs
  let delWt = epsilon *^ ((posCor -^ negCor) /^ ns)
  let rbm' = updateWeights rbm delWt
  return rbm'

-- |train a batch of inputs for an RBM
-- input is in the form of n samples of input probability vector v i.e. vs = n x v matrix
-- input is sampled i times, and each time we iterate through CD(cdn) to update rbm by epsilon
trainBatch :: Int -> Int -> Double -> RBM -> Matrix -> IO RBM
trainBatch cdn i epsilon rbm vs = do
  let e = mkWtConst rbm epsilon
      go 0 _ r _  = return r
      go i es r vs = do
        rbm' <- cdLearn cdn es r vs
        go (i-1) es rbm' vs
  go i e rbm vs

reconstruct :: RBM -> Int -> Int -> Vector -> IO Vector
reconstruct rbm iters numSamples v = do
  let go 0 vs = return vs
      go n vs = do
        hs  <- samplePNs (getHiddenPGivenVs  rbm vs)
        vs' <- samplePNs (getVisiblePGivenHs rbm hs)
        go (n-1) vs'
  vs <- go iters (dupVec numSamples v)
  return $ R.sum (transpose vs)

dream :: RBM -> Int -> Int -> IO Vector
dream rbm iters numSamples = do
  let go 0 vs = return vs
      go n vs = do
        hs  <- samplePNs (getHiddenPGivenVs  rbm vs)
        vs' <- samplePNs (getHiddenPGivenVs  rbm hs)
        go (n-1) vs'
      (_ :. i :. _) = R.extent (rbm_weightsT rbm)
  v <- sequence $ replicate numSamples $ sequence $ replicate i ((sample $ normal 0.0 0.1) :: IO Double)
  vs <- go iters (mtxFromList (numSamples, i) v)
  return $ R.sum (transpose vs)

showVec :: Vector -> IO ()
showVec v = putStrLn $ "  " ++ (foldr (\x s-> printf "%+3.2f" x ++ ",\t" ++ s) "" (R.toList v))

showMat :: String -> Matrix -> IO ()
showMat name m = do
  let (Z :. h :. w) = R.extent m
  putStrLn $ name ++ "(" ++ show h ++ ", " ++ show w ++ ") ="
  mapM_ (\h -> showVec (R.slice m (Z :. h :. All))) [0..(h-1)]

visualizeWeights :: (R.Elt z, PlotValue z, Ord z, Num z, DefaultRange z) => R.Array R.DIM2 z -> IO ()
visualizeWeights m = do
  renderableToWindow renderBars 640 480
    where renderBars :: Renderable ()
          renderBars = toRenderable $ wtsLayout
          wtsLayout :: Layout1 Int Int
          wtsLayout = layout1_title ^= "RBM Weights" 
                    $ layout1_plots ^= [ Left (plotPixelMap wtsPixMap) ]
                    $ layout1_left_axis ^= (laxis_reverse ^= True $ defaultLayoutAxis)
                    $ layout1_top_axis ^= defaultLayoutAxis
                    $ layout1_grid_last ^= True
                    $ defaultLayout1
          wtsPixMap = pixel_map_range ^= make_range (-9999) 9999 m
                    $ defaultPixelMap m
