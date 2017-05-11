{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3  #-}

module ImageTransformation where

import Codec.Picture.Repa
import Codec.Picture
import Control.Monad
import Data.Array.Repa as R
import Debug.Trace

-------------------------------------------------------------------------

-- RGB8 is a tuple of the red, green, and blue pixel values.
type RGB8 = (Pixel8, Pixel8, Pixel8)



--------------------------- Image Transformations ------------------------------

-- Blur takes a blur radius and returns an array of the blurred pixel values.
blur :: Int -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
blur r img = R.traverse img id remap where
    (Z :. width :. height) = extent img
    remap f (Z :. x :. y) = intToRGB (tmap (`div` len) rgb') where
        (minX, maxX, minY, maxY, len) = nbhd r x y width height
        rgbs = do
            x' <- [minX, minX + 1 .. maxX]
            y' <- [minY, minY + 1 .. maxY]
            return $ rgbToInt (f (Z :. x' :. y'))
        rgb'= tfold (+) (0,0,0) rgbs

-- Border adds a border of a given radius around the outside of the given image.
-- It also increases the image size based on the surrounding radius.
border :: Int -> RGB8 -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
border r c img = R.traverse img newDim remap where
    (Z :. width :. height) = extent img
    newDim (Z :. width :. height) = Z :. width + 2 * r :. height + 2 * r
    remap f (Z :. x :. y)
      | x < r ||
        x >= width + r ||
        y < r ||
        y >= height + r  = c
      | otherwise        = f (Z :. x - r :. y - r)

-- Brigten adds or takes away brightness to the image by adjusting the pixel
-- value by the given ratio of the max pixel value (a float between -1.0 and
-- 1.0).
brighten :: Float -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
brighten flt img = R.traverse img id remap where
    remap f (Z :. x :. y) = intToRGB (boundRGB (tmap fn rgb)) where
        rgb = rgbToInt (f (Z :. x :. y))
        fn x = x + round (255 * flt)

-- ColorInvert inverts the color of the given image.
colorInvert :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
colorInvert img = R.traverse img id remap where
    remap f (Z :. x :. y) = intToRGB (tmap (\x -> 255 - x) rgb) where
        rgb = rgbToInt (f (Z :. x :. y))

-- Contrast adjusts the contrast of the image by the given amount (a float
-- between -1.0 and 1.0).
contrast :: Float -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
contrast flt img = R.traverse img id remap where
    remap f (Z :. x :. y) = intToRGB (boundRGB (tmap fn rgb)) where
        rgb = rgbToFloat (f (Z :. x :. y))
        fn x = round ((x - 128) * tan ((flt + 1) * pi/4) + 128)

-- Edge runs an edge detection algorithm over the given image and returns the
-- result.
edge :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
edge img = R.traverse img id remap where
    (Z :. width :. height) = extent img
    remap f (Z :. x :. y) = intToRGB rgb' where
        (minX, maxX, minY, maxY, len) = nbhd 1 x y width height
        rgbs = do
            x' <- [minX, minX + 1 .. maxX]
            y' <- [minY, minY + 1 .. maxY]
            return $ tmap
                (if x' == x && y' == y
                    then ((len -1) *)
                    else negate)
                (rgbToInt (f (Z :. x' :. y')))
        rgb' = boundRGB (tfold (+) (0,0,0) rgbs)

-- Fill fills the entire extent of the original image with the given rgb color.
fill :: RGB8 -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
fill c img = R.traverse img id (\_ _ -> c)

-- FlipH flips the given image horizontally.
flipH :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
flipH img = R.traverse img id remap where
    (Z :. width :. height) = extent img
    remap f (Z :. x :. y) = f (Z :. width - x - 1 :. y)

-- FlipV flips the given image vertically.
flipV :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
flipV img = R.traverse img id remap where
    (Z :. width :. height) = extent img
    remap f (Z :. x :. y) = f (Z :. x :. height - y - 1)

-- Grayscale converts the given image's rgb color values into monochromatic
-- gray values.
grayscale :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
grayscale img = R.traverse img id remap where
    remap f (Z :. x :. y) = floatToRGB (gray,gray,gray) where
        (r,g,b) = rgbToFloat (f (Z :. x :. y))
        gray = 0.2989 * r + 0.5870 * g + 0.1140 * b

-- RotateCCW rotates the given image counter-clockwise.
rotateCCW :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
rotateCCW img = R.traverse img newDim remap where
    (Z :. width :. height) = extent img
    newDim (Z :. width :. height) = Z :. height :. width
    remap f (Z :. x :. y) = f (Z :. width - y - 1 :. x)

-- RotateCW rotates the given image clockwise.
rotateCW :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
rotateCW img = R.traverse img newDim remap where
    (Z :. width :. height) = extent img
    newDim (Z :. width :. height) = Z :. height :. width
    remap f (Z :. x :. y) = f (Z :. y :. height - x - 1)

-- Saturate adjusts the saturation by the given amount (a float between -1.0 and
-- 1.0).
saturate :: Float -> Array D DIM2 RGB8 -> Array D DIM2 RGB8
saturate flt img = R.traverse img id remap where
    remap f (Z :. x :. y) = intToRGB (boundRGB (tmap fn rgb)) where
        rgb@(r,g,b) = rgbToFloat (f (Z :. x :. y))
        gray = 0.2989 * r + 0.5870 * g + 0.1140 * b
        fn x = round (-gray * flt + x * (1 + flt))

-- Sharpen runs a sharpening algorithm on the given image and returns the
-- result.
sharpen :: Array D DIM2 RGB8 -> Array D DIM2 RGB8
sharpen img = R.traverse img id remap where
    (Z :. width :. height) = extent img
    remap f (Z :. x :. y) = intToRGB rgb' where
        (minX, maxX, minY, maxY, len) = nbhd 1 x y width height
        rgbs = do
            x' <- [minX, minX + 1 .. maxX]
            y' <- [minY, minY + 1 .. maxY]
            return $ tmap
                (if x' == x && y' == y
                    then (len *)
                    else negate)
                (rgbToInt (f (Z :. x' :. y')))
        rgb' = boundRGB (tfold (+) (0,0,0) rgbs)



----------------------------- Helper Functions ---------------------------------

-- BoundRGB ensures that the given rgb values are within the legal values (0 to
-- 255).
boundRGB :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
boundRGB = tmap (\i -> max (min i 255) 0)

-- RgbToInt takes an RGB8 and returns the rgb values as integers.
rgbToInt :: RGB8 -> (Integer, Integer, Integer)
rgbToInt (r,g,b) = (toInteger r, toInteger g, toInteger b)

-- RgbToFloat takes an RBG8 and returns the rgb values as floats.
rgbToFloat :: RGB8 -> (Float, Float, Float)
rgbToFloat (r,g,b) = (fromIntegral r, fromIntegral g, fromIntegral b)

-- IntToRGB converts the integer rgb values to an RGB8.
intToRGB :: (Integer, Integer, Integer) -> RGB8
intToRGB (r,g,b) = (fromIntegral r, fromIntegral g, fromIntegral b)

-- FloatToRGB converts teh float rgb values to an RGB8.
floatToRGB :: (Float, Float, Float) -> RGB8
floatToRGB (r,g,b) = intToRGB (round r, round g, round b)

-- Tmap maps a given function over the tuple values.
tmap :: (a->b) -> (a, a, a) -> (b, b, b)
tmap f (r,g,b) = (f r, f g, f b)

-- Tfold folds a given function over a list of tuples.
tfold :: (a->b->b) -> (b,b,b) -> [(a,a,a)] -> (b,b,b)
tfold f = foldr (\(r,g,b) (r',g',b') -> (f r r', f g g', f b b'))

-- Nbhd returns the bounds of the neighborhood of the given pixel (x,y), taking
-- the image borders into account.
nbhd :: Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Integer)
nbhd r x y w h = (minX, maxX, minY, maxY, len) where
    minX = max (x-r) 0
    maxX = min (x+r) (w - 1)
    minY = max (y-r) 0
    maxY = min (y+r) (h - 1)
    len  = toInteger ((maxX - minX + 1) * (maxY - minY + 1))

-- ToImage converts an array of RGB8 pixel values to an Image PixelRGB8.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

-- FromImage converts an Image PixelRGB8 to an array of RGB8 pixel values.
fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img@(Image imageWidth imageHeight _) =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))
