{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Evaluator where

import System.IO

import qualified Data.Array.Repa as R

import qualified Codec.Picture.Repa as J
import qualified Codec.Picture as P

import qualified ImageTransformation as T

-------------------------------------------------------------------------

-- | ADT for image transformations
data IT =
    FP FilePath                      -- image location on file system
  | Blur Int IT                      -- blur the given IT by the given radius
  | Border Int T.RGB8 IT             -- Add border of given width and RGB color
  | Brighten Float IT                -- brighten IT by ratio to max rgb value
  | ColorInvert IT                   -- invert colors of It
  | Contrast Float IT                -- Contrast IT  by given amount between -1 to 1
  | Edge IT                          -- perform edge detection on IT
  | Fill T.RGB8 IT                   -- Fill IT with given RGB color
  | FlipH IT                         -- Flip IT horizontally
  | FlipV IT                         -- Flip IT vertically
  | Grayscale IT                     -- grayscale IT
  | RotateCCW IT                     -- rotate IT counterclockwise
  | RotateCW IT                      -- rotate IT clockwise
  | Saturate Float IT                -- saturate IT
  | Sharpen IT                       -- sharpen IT
  deriving (Eq, Show)

-- | evaluate IT to a sequence of actions to perform on the image
evaluateIT :: IT -> IO (Either String (R.Array R.D R.DIM2 T.RGB8))

evaluateIT (FP file)        = readAsArray file
evaluateIT (Blur f i)       = evaluatorHelper (T.blur f) i
evaluateIT (Border w rgb i) = evaluatorHelper (T.border w rgb) i
evaluateIT (Brighten f i)   = evaluatorHelper (T.brighten f) i
evaluateIT (ColorInvert i)  = evaluatorHelper T.colorInvert i
evaluateIT (Contrast f i)   = evaluatorHelper (T.contrast f) i
evaluateIT (Edge i)         = evaluatorHelper T.edge i
evaluateIT (Fill rgb i)     = evaluatorHelper (T.fill rgb) i
evaluateIT (FlipH i)        = evaluatorHelper T.flipH i
evaluateIT (FlipV i)        = evaluatorHelper T.flipV i
evaluateIT (Grayscale i)    = evaluatorHelper T.grayscale i
evaluateIT (RotateCCW i)    = evaluatorHelper T.rotateCCW i
evaluateIT (RotateCW i)     = evaluatorHelper T.rotateCW i
evaluateIT (Saturate f i)   = evaluatorHelper (T.saturate f) i
evaluateIT (Sharpen i)      = evaluatorHelper T.sharpen i

-- | higher order function to evaluate one step of IT
evaluatorHelper :: (R.Array R.D R.DIM2 T.RGB8 -> R.Array R.D R.DIM2 T.RGB8) -> IT
   -> IO (Either String (R.Array R.D R.DIM2 T.RGB8))

evaluatorHelper f i = do
                       img <- evaluateIT i
                       case img of
                        Left s -> return img
                        Right i -> return $ (Right . f ) i


-- | read image specified by filepath to an array of RGB8 pixels
readAsArray :: FilePath -> IO (Either String (R.Array R.D R.DIM2 T.RGB8))

readAsArray file = do
  eImg <- P.readImage file
  case eImg of
    Left err -> return (Left err)
    Right img -> return $ Right ((T.fromImage . P.convertRGB8) img)

-- | write output of image transformation to disk with specified file path
write :: FilePath -> IO (Either String (R.Array R.D R.DIM2 T.RGB8)) -> IO ()

write n i =
  i >>= (\img -> case img of
                Left s -> putStrLn s
                Right a -> do
                  computed <- R.computeUnboxedP a
                  (P.saveJpgImage 100 n . P.ImageRGB8 . T.toImage) computed)
