{-# OPTIONS_GHC -XTemplateHaskell #-}
module Graphics.Rendering.Chart.Plot.PixelMap
  ( PixelMap(..)

  , defaultPixelMap
  , plotPixelMap

  , pixel_map_title
  , pixel_map_palette
  , pixel_map_values
  , pixel_map_range

  , greyscale
  , make_range
  ) where

import qualified Graphics.Rendering.Cairo as C

import qualified Data.Array.Repa as R
import Data.Array.Repa(Z(..), (:.)(..))

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Data.Colour
import Data.Colour.SRGB(sRGB)

import Data.Accessor.Template
import Control.Monad

data PixelMap z x y = PixelMap
  { pixel_map_title_      :: String
  , pixel_map_palette_    :: [Colour Double]
  , pixel_map_values_     :: R.Array R.DIM2 z
  , pixel_map_range_      :: (z, z)
  , pixel_map_locx_       :: [x]
  , pixel_map_locy_       :: [y]
  }

class DefaultRange a where
    minDefVal :: a
    maxDefVal :: a

instance DefaultRange Int where
    minDefVal = 0
    maxDefVal = 1

instance DefaultRange Double where
    minDefVal = 0.0
    maxDefVal = 1.0

instance DefaultRange Float where
    minDefVal = 0.0
    maxDefVal = 1.0

-- TODO: put a more sensible min here
make_range :: (R.Elt z, Ord z) => z -> z -> R.Array R.DIM2 z -> (z, z)
make_range minVal maxVal m = (R.foldAll min maxVal m
                             ,R.foldAll max minVal m)

defaultPixelMap :: (PlotValue z, DefaultRange z) => R.Array R.DIM2 z -> PixelMap z Int Int
defaultPixelMap = mkPixelMap greyscale "" minDefVal maxDefVal


-- TODO: Repa-rize me
greyscale :: [Colour Double]
greyscale = map (\x -> sRGB x x x) [x/255.0 | x <- [0..255]]

mkPixelMap :: (PlotValue z) => [Colour Double] -> String -> z -> z -> R.Array R.DIM2 z -> PixelMap z Int Int
mkPixelMap palette title min max m = PixelMap
  { pixel_map_title_      = ""
  , pixel_map_palette_    = palette
  , pixel_map_values_     = m
  , pixel_map_range_      = (min, max)
  , pixel_map_locx_       = [0, w]
  , pixel_map_locy_       = [0, h]
  }
    where (Z :. h :. w) = R.extent m

plotPixelMap :: (R.Elt z, PlotValue z) => PixelMap z Int Int -> Plot Int Int
plotPixelMap p = Plot { plot_render_ = renderPixelMap p
                      , plot_legend_ = [ (pixel_map_title_ p
                                         , renderSpotLegend p) ]
                      , plot_all_points_ = ( pixel_map_locx_ p
                                           , pixel_map_locy_ p )
                      }

renderPixelMap  :: (R.Elt z, PlotValue z) => PixelMap z Int Int -> PointMapFn Int Int -> CRender ()
renderPixelMap p pmap = preserveCState $ mapM_ drawPixel [(x, y, elem x y) | x <- [0..(w-1)], y <- [0..(h-1)]]
  where m = pixel_map_values_ p
        (Z :. h :. w) = R.extent m
        elem x y = m R.! (Z :. y :. x)
        (minVal_, maxVal_) = pixel_map_range_ p
        minVal = toValue minVal_
        maxVal = toValue maxVal_
        numColors = toValue $ (length (pixel_map_palette_ p)) - 1
        scale = fromValue . (\x -> numColors * (x - minVal)/(maxVal - minVal)) . toValue
        -- drawPixel :: (R.Elt z, PlotValue z) => (Int, Int, z) -> CRender ()
        drawPixel (x, y, c) = do
          setFillStyle $ solidFillStyle (opaque ((pixel_map_palette_ p) !! (scale c)))
          let p1 = mapXY pmap (x, y)
              p2 = mapXY pmap (x+1, y+1)
          fillPath (rectPath (Rect p1 p2))

renderSpotLegend :: PixelMap z x y -> Rect -> CRender ()
renderSpotLegend p r@(Rect p1 p2) = preserveCState $ do
                                      return ()
  --   let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
  --       centre = linearInterpolate p1 p2
  --   let (CairoPointStyle drawSpotAt)    = filledCircles radius
  --                                           (flip withOpacity 0.2 $
  --                                            head (pixel_map_palette__ p))
  --   drawSpotAt centre
  --   let (CairoPointStyle drawOutlineAt) = hollowCircles radius
  --                                           (pixel_map_linethick__ p)
  --                                           (opaque $
  --                                            head (pixel_map_palette__ p))
  --   drawOutlineAt centre
  -- where
  --   linearInterpolate (Point x0 y0) (Point x1 y1) =
  --       Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

$( deriveAccessors ''PixelMap )