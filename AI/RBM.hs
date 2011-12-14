module AI.RBM where

import Control.Applicative((<$>))

import Data.Random
import Data.Random.Distribution.Bernoulli

import qualified Data.Array.Repa as R
import Data.Array.Repa(Z(..), All(..), (:.)(..), (*^), (+^), (-^), (/^))
import Data.Array.Repa.Algorithms.Matrix(multiplyMM)

import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.AreaSpots
import Data.Colour.SRGB
import Data.Accessor

type Vector = R.Array R.DIM1 Double
type Matrix = R.Array R.DIM2 Double

data RBM = RBM { 
      rbm_visBias  :: !Vector -- i
    , rbm_hidBias  :: !Vector -- j
    , rbm_weights  :: !Matrix -- i * j
    , rbm_weightsT :: !Matrix -- j * i
    } deriving (Eq, Show)

dim1 size = Z :. size
dim2 rows cols = Z :. rows :. cols

fromLists :: Real a => ([a], [a], [[a]]) -> RBM
fromLists (vs, hs, ws)
    | length ws /= lh = error $ "Malformed RBM!"
    | otherwise = RBM { rbm_visBias = R.fromList (dim1 lv) (map realToFrac vs)
                      , rbm_hidBias = R.fromList (dim1 lh) (map realToFrac hs)
                      , rbm_weights = ws'
                      , rbm_weightsT = transpose ws'
                      }
    where lv = length vs
          lh = length hs
          ws' = R.fromList (dim2 lh lv) (concatMap (map realToFrac) ws)

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

dupVec :: Int -> Vector -> Matrix
dupVec n v = R.extend (Z :. n :. All) v
    where (Z :. h) = R.extent v

getHiddenPGivenV :: RBM -> Vector -> Vector
getHiddenPGivenV rbm v = R.map sigmoid $ hbs +^ (ws `mXv` v)
    where hbs = rbm_hidBias rbm
          ws = rbm_weights rbm

getVisiblePGivenH :: RBM -> Vector -> Vector
getVisiblePGivenH rbm h = R.map sigmoid $ vbs +^ (ws `mXv` h)
    where vbs = rbm_visBias rbm
          ws = rbm_weightsT rbm

getHiddenPGivenVs :: RBM -> Matrix -> Matrix
getHiddenPGivenVs rbm vs = R.map sigmoid $ hbm +^ (R.sum (wm *^ vm))
    where hbs = rbm_hidBias rbm
          ws = rbm_weights rbm
          (Z :. j :. i) = R.extent ws
          (Z :. n :. _) = R.extent vs
          hbm = R.extend (Z :. n   :. All) hbs
          wm  = R.extend (Z :. n   :. All :. All) ws
          vm  = R.extend (Z :. All :. j   :. All) vs

getVisiblePGivenHs :: RBM -> Matrix -> Matrix
getVisiblePGivenHs rbm hs = R.map sigmoid $ vbm +^ (R.sum (wm *^ hm))
    where vbs = rbm_visBias rbm
          ws = rbm_weightsT rbm
          (Z :. i :. j) = R.extent ws
          (Z :. n :. _) = R.extent hs
          vbm = R.extend (Z :. n   :. All) vbs
          wm  = R.extend (Z :. n   :. All :. All) ws
          hm  = R.extend (Z :. All :. i   :. All) hs

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
corr v hs = R.sum $ vm *^ hm
    where (_ :. n :. j) = R.extent hs
          (_ :. i) = R.extent v
          vm = R.extend (Z :. All :.   j :.   n) v
          hm = R.extend (Z :. i   :. All :. All) $ transpose hs

corrs :: Matrix -> Matrix -> Matrix
corrs vs hs = R.sum $ vm *^ hm
    where (Z :. n :. j) = R.extent hs
          (Z :. _ :. i) = R.extent vs
          vm = R.extend (Z :. All :.   j :. All) $ transpose vs
          hm = R.extend (Z :. i   :. All :. All) $ transpose hs

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


-- r = fromLists ([0.01, 0, 0.05], [0.035, (-0.013)], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
r = fromLists ([0, 0, 0], [0, 0], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
v = vecFromList [1, 0, 1]
e = mkWtConst r 0.07

mkWtConst :: RBM -> Double -> Matrix
mkWtConst r e = R.fromFunction (R.extent . rbm_weightsT $ r) (const e)

updateWeights :: RBM -> Matrix -> RBM
updateWeights rbm delWt = rbm { rbm_weights = (rbm_weights rbm +^ (R.transpose delWt))
                              , rbm_weightsT = (rbm_weightsT rbm +^ delWt) }

cdLearn :: Int -> Matrix -> RBM -> Vector -> IO (RBM, Double)
cdLearn n epsilon rbm v = do
  let numSamples = 10
      ns = mkWtConst rbm 10
  hs <- samplePN (getHiddenPGivenV rbm v) numSamples
  let posCor = corr v hs
  vs <- samplePNs (getVisiblePGivenHs rbm hs)
  let negCor = corrs vs hs
  let delWt = epsilon *^ ((posCor -^ negCor) /^ ns)
  -- print posCor
  -- print negCor
  -- print ns
  -- print delWt
  let rbm' = updateWeights rbm delWt
  -- TODO: Get reconstruction error squared
  let origVs = dupVec numSamples v
      err = (origVs -^ vs)
      errSq = (R.sumAll (err *^ err))
  -- print posCor
  -- print negCor
  -- print epsilon
  -- print delWt
  -- print rbm
  return (rbm', errSq)

learnLoop :: Int -> RBM -> Vector -> IO RBM
learnLoop 0 rbm _ = return rbm
learnLoop n rbm v = do
  (rbm', err) <- cdLearn 1 e rbm v
  putStrLn $ show n ++ "," ++ show err
  -- putStrLn $ "N:" ++ show n ++ ", Error:" ++ show err
  learnLoop (n-1) rbm' v

reconstruct :: RBM -> Vector -> IO Vector
reconstruct rbm v = do
  let numSamples = 10
  hs <- samplePN (getHiddenPGivenV rbm v) numSamples
  vs <- samplePNs (getVisiblePGivenHs rbm hs)
  let vs' = R.sum (transpose vs)
  return vs'
