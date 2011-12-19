module AI.RBM where

import Control.Applicative((<$>))

import Data.Random
import Data.Random.Distribution.Bernoulli

import qualified Data.Array.Repa as R
import Data.Array.Repa(transpose, Z(..), All(..), (:.)(..), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Algorithms.Matrix(multiplyMM)

import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.AreaSpots
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

{-# INLINE prepend #-}
prepend :: Vector -> Matrix -> Matrix
prepend v m = transpose (vec2Mtx v R.++ (transpose m))

{-# INLINE vec2Mtx #-}
vec2Mtx :: Vector -> Matrix
vec2Mtx v = R.extend (Z :. All :. (1 :: Int)) v

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

{-# INLINE sigmoid #-}
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

vecFromList :: Real a => [a] -> Vector
vecFromList xs = R.fromList (dim1 $ length xs) (map realToFrac xs)

mtxFromList :: Real a => (Int, Int) -> [[a]] -> Matrix
mtxFromList (r, c) xs = R.fromList (dim2 r c) (concatMap (map realToFrac) xs)

{-# INLINE mXv #-}
mXv :: Matrix -> Vector -> Vector
mXv m v = R.sum (vm *^ m)
    where (_ :. hm :. _) = R.extent m
          vm = R.extend (Z :. hm :. All) v

{-# INLINE oneVec #-}
oneVec :: Vector
oneVec = R.fromFunction (Z :. 1) (const 1)

{-# INLINE dupVec #-}
dupVec :: Int -> Vector -> Matrix
dupVec n v = R.extend (Z :. n :. All) v

getHiddenPGivenV :: RBM -> Vector -> Vector
getHiddenPGivenV rbm v = R.map sigmoid $ (ws `mXv` (oneVec R.++ v))
    where ws = rbm_weights rbm

getVisiblePGivenH :: RBM -> Vector -> Vector
getVisiblePGivenH rbm h = R.map sigmoid $ (ws `mXv` (oneVec R.++ h))
    where ws = rbm_weightsT rbm

getHiddenPGivenVs :: RBM -> Matrix -> Matrix
getHiddenPGivenVs rbm vs = R.map sigmoid $ R.sum (wm *^ vm)
    where ws = rbm_weights rbm
          (Z :. j :. _) = R.extent ws
          (Z :. n :. _) = R.extent vs
          oneV = R.fromFunction (Z :. n :. 1) (const 1)
          wm  = R.extend (Z :. n   :. All :. All) ws
          vm  = R.extend (Z :. All :. j   :. All) (oneV R.++ vs)

getVisiblePGivenHs :: RBM -> Matrix -> Matrix
getVisiblePGivenHs rbm hs = R.map sigmoid $ R.sum (wm *^ hm)
    where ws = rbm_weightsT rbm
          (Z :. i :. _) = R.extent ws
          (Z :. n :. _) = R.extent hs
          oneH = R.fromFunction (Z :. n :. 1) (const 1)
          wm  = R.extend (Z :. n   :. All :. All) ws
          hm  = R.extend (Z :. All :. i   :. All) (oneH R.++ hs)

sampleP :: Vector -> IO Vector
sampleP ps = R.fromList (R.extent ps) <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps)

samplePN :: Vector -> Int -> IO Matrix
samplePN ps n = R.fromList (Z :. n :. l) <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps')
          (_ :. l) = R.extent ps
          ps' = R.extend (Z :. n :. All) ps

samplePNs :: Matrix -> IO Matrix
samplePNs ps = R.fromList ext <$> sequence bs
    where bs :: [IO Double]
          bs = map (sample . bernoulli) (R.toList ps)
          ext = R.extent ps

-- build expected value of <vh> given V(H) and `n` sampled H's(Vs)
corr :: Vector -> Matrix -> Matrix
corr v hs = sh `prepend` (sv R.++ (R.sum $ vm *^ hm))
    where (_ :. n :. j) = R.extent hs
          (_ :. i) = R.extent v
          hsT = transpose hs
          vm = R.extend (Z :. All :.   j :.   n) v
          hm = R.extend (Z :. i   :. All :. All) hsT
          sv = vec2Mtx v -- i,1
          sh = oneVec R.++ (R.sum hsT) -- <h> j + 1

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

-- r = fromLists ([0.01, 0, 0.05], [0.035, (-0.013)], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
r = fromLists ([0, 0, 0], [0, 0], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
v = vecFromList [1, 0, 1]
e = mkWtConst r 0.07

mkWtConst :: RBM -> Double -> Matrix
mkWtConst r e = R.fromFunction (Z :. (h+1) :. w) (const e)
    where (Z :. h  :. w) = R.extent (rbm_weightsT r)

updateWeights :: RBM -> Matrix -> RBM
updateWeights rbm delWt = rbm { rbm_weights  = (rbm_weights  rbm +^ dwt)
                              , rbm_weightsT = (rbm_weightsT rbm +^ dwtT) }
    where dwtT  = R.traverse delWt (\(Z :. h :. w) -> (Z :. (h-1) :. w)) (\f (Z :. i :. j) -> f (Z :. (i+1) :. j)) -- (i+1)x(j+1) -> (i)x(j+1)
          dwt = R.traverse delWt (\(Z :. h :. w) -> (Z :. (w-1) :. h)) (\f (Z :. i :. j) -> f (Z :. j :. (i+1))) -- (i+1)x(j+1) -> (j)x(i+1)

cdLearn :: Int -> Matrix -> RBM -> Vector -> IO (RBM, Double)
cdLearn n epsilon rbm v = do
  let numSamples = 10
      ns = mkWtConst rbm (realToFrac numSamples)
  hs <- samplePN (getHiddenPGivenV rbm v) numSamples
  let posCor = corr v hs
  vs' <- samplePNs (getVisiblePGivenHs rbm hs)
  hs' <- samplePNs (getHiddenPGivenVs rbm vs')
  let negCor = corrs vs' hs'
  let delWt = epsilon *^ ((posCor -^ negCor) /^ ns)
  let rbm' = updateWeights rbm delWt
  -- Get reconstruction error squared
  let origVs = dupVec numSamples v
      err = (origVs -^ vs')
      errSq = (R.sumAll (err *^ err))
  -- showMat "hs" hs
  -- showMat "posCor" posCor
  -- showMat "vs'" vs'
  -- showMat "negCor" negCor
  -- showMat "epsilon" epsilon
  -- showMat "delWt" delWt
  -- print delWt
  -- print rbm
  return (rbm', errSq)

learnLoop :: Int -> RBM -> Vector -> IO RBM
learnLoop 0 rbm _ = return rbm
learnLoop n rbm v = do
  (rbm', err) <- cdLearn 1 e rbm v
  -- putStrLn $ show n ++ "," ++ show err
  -- putStrLn $ "N:" ++ show n ++ ", Error:" ++ show err
  learnLoop (n-1) rbm' v

reconstruct :: RBM -> Vector -> IO Vector
reconstruct rbm v = do
  let numSamples = 10
  hs <- samplePN (getHiddenPGivenV rbm v) numSamples
  vs <- samplePNs (getVisiblePGivenHs rbm hs)
  let vs' = R.sum (transpose vs)
  return vs'

showVec :: Vector -> IO ()
showVec v = putStrLn $ "  " ++ (foldr (\x s-> printf "%+3.2f" x ++ ",\t" ++ s) "" (R.toList v))

showMat :: String -> Matrix -> IO ()
showMat name m = do
  let (Z :. h :. w) = R.extent m
  putStrLn $ name ++ "(" ++ show h ++ ", " ++ show w ++ ") ="
  mapM_ (\h -> showVec (R.slice m (Z :. h :. All))) [0..(h-1)]

visualizeWeights :: RBM -> IO ()
visualizeWeights rbm = do
  print weights
  print pts
  renderableToWindow renderBars 640 480
    where renderBars :: Renderable ()
          renderBars = toRenderable $ wtsLayout
          wtsLayout :: Layout1 Int Int
          wtsLayout = layout1_title ^= "RBM Weights" 
                    $ layout1_plots ^= [ Left (toPlot wts) ]
                    $ defaultLayout1
          wts :: AreaSpots4D Int Int Int Int
          wts = area_spots_4d_palette ^= colors
              $ area_spots_4d_values ^= pts
              $ defaultAreaSpots4D
          maxWt = 5.0
          minWt = 1.0
          wtRange = maxWt - minWt
          mkColor x = sRGB x x x
          colors = map mkColor [x/255.0 | x <- [0..255]]
          sz = 1
          weights = rbm_weights rbm
          (_ :. h :. w) = R.extent weights
          pts = [ (i `mod` w, h - (i `div` w), sz, round (255 * ((x - minWt)/wtRange))) | (i, x) <- zip [0..] (R.toList weights)]

