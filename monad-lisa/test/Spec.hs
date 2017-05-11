module Main where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:))
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sample, sized, quickCheckWith,quickCheck, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>), Property)

import Test.QuickCheck.Monadic

import qualified Parser as P
import qualified ParserCombinators as P

import Codec.Picture (Pixel8)

import Control.Monad (liftM,liftM2,liftM3)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Word (Word8)

import ImageTransformation
import ImageParser as IP (itP)
import Evaluator

import Data.Array.Repa as R
import Data.Array.Repa.Arbitrary as R

--------------------------------------------------------------------------------

main :: IO ()
main = do
  _ <- runTestTT tAll
  quickCheckN 100 prop_flipHTwice
  quickCheckN 100 prop_flipVTwice
  quickCheckN 100 prop_rotateCWCCW
  quickCheckN 100 prop_rotateCCWCW
  quickCheckN 100 prop_rotateCWx4
  quickCheckN 100 prop_invColor
  quickCheckN 1   prop_monadicEvalID
  quickCheckN 1   prop_monadicEvalID2
  quickCheckN 1   prop_monadicEvalBlur
  quickCheckN 1   prop_monadicBorderComp
  quickCheckN 1   prop_monadicColorInvertComp
  quickCheckN 1   prop_monadicFillComp
  quickCheckN 1   prop_monadicFlipHComp
  quickCheckN 1   prop_monadicFlipVComp
  quickCheckN 1   prop_monadicFlipHTwiceComp
  quickCheckN 1   prop_monadicFlipVTwiceComp
  quickCheckN 1   prop_monadicGrayscaleComp
  quickCheckN 1   prop_monadicRotateCWCCWComp
  quickCheckN 1   prop_monadicRotateCCWCWComp
  return ()

-- RGB representations of colors --
lightBlue :: RGB8
lightBlue = intToRGB (173,216,230)

grayLightBlue :: RGB8
grayLightBlue = intToRGB (205, 205, 205)

invertLightBlue :: RGB8
invertLightBlue = intToRGB (82, 39, 25)

springGreen :: RGB8
springGreen = intToRGB (0,255,127)

graySpringGreen :: RGB8
graySpringGreen = intToRGB (164, 164, 164)

invertSpringGreen :: RGB8
invertSpringGreen = intToRGB (255, 0, 128)

red :: RGB8
red = intToRGB (255,0,0)

grayRed :: RGB8
grayRed = intToRGB (76, 76, 76)

invertRed :: RGB8
invertRed = intToRGB (0, 255, 255)

orange :: RGB8
orange = intToRGB (238,154, 0)

grayOrange :: RGB8
grayOrange = intToRGB (162, 162, 162)

invertOrange :: RGB8
invertOrange = intToRGB (17, 101, 255)

yellow :: RGB8
yellow = intToRGB (255, 255, 0)

grayYellow :: RGB8
grayYellow = intToRGB (226,226,226)

invertYellow :: RGB8
invertYellow = intToRGB (0, 0, 255)

green :: RGB8
green = intToRGB (0,255,0)

grayGreen :: RGB8
grayGreen = intToRGB (150, 150, 150)

invertGreen :: RGB8
invertGreen = intToRGB (255, 0, 255)

blue :: RGB8
blue = intToRGB (0,0,255)

grayBlue :: RGB8
grayBlue = intToRGB (29, 29, 29)

invertBlue :: RGB8
invertBlue = intToRGB (255, 255, 0)

pink :: RGB8
pink = intToRGB (255, 192, 203)

grayPink :: RGB8
grayPink = intToRGB (212, 212, 212)

invertPink :: RGB8
invertPink = intToRGB (0, 63, 52)

black :: RGB8
black = intToRGB (0, 0, 0)

grayBlack :: RGB8
grayBlack = intToRGB (0, 0, 0)

invertBlack :: RGB8
invertBlack = intToRGB (255, 255, 255)

--------------------------------------------------------
--- arrays of RGB representations --

imgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ lightBlue,  springGreen,  red,
                orange,     yellow,       green,
                blue,       pink,         black]

borderImgArray1 = fromListUnboxed (Z :. (5::Int) :. (5::Int))
              [ pink,  pink,       pink,         pink,   pink,
                pink,  lightBlue,  springGreen,  red,    pink,
                pink,  orange,     yellow,       green,  pink,
                pink,  blue,       pink,         black,  pink,
                pink,  pink,       pink,         pink,   pink]

colorInvertImgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ invertLightBlue, invertSpringGreen, invertRed,
                invertOrange,    invertYellow,      invertGreen,
                invertBlue,      invertPink,        invertBlack]

fillImgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ pink,  pink,  pink,
                pink,  pink,  pink,
                pink,  pink,  pink]

flipHImgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ blue, pink, black,
                orange, yellow, green,
                lightBlue, springGreen, red]

flipVImgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ red,    springGreen, lightBlue,
                green,  yellow,      orange,
                black,  pink,        blue      ]

grayImgArray1 = fromListUnboxed (Z :. (3::Int) :. (3::Int))
              [ grayLightBlue, graySpringGreen, grayRed,
                grayOrange,    grayYellow,      grayGreen,
                grayBlue,      grayPink,        grayBlack]


-- | delayed representation of ImgArray1
delayedImgArray1 = delay imgArray1

--------------------------------------------------------

tAll :: Test
tAll = TestList [ tBorder, tColorInvert, tFill, tFlipHOnce, tFlipVOnce,
                  tFlipHTwice, tFlipVTwice, tGrayscale, tRotateCWCCW,
                  tRotateCCWCW, tParseOneLevel, tParseRGB, tParseTwo, tParseWSP]

---- Image Transformation Tests --

-- | sequential comp
tBorder :: Test
tBorder = "tBorder" ~:
  computeS (border 1 pink delayedImgArray1) ~?= borderImgArray1

-- | parallel comp
prop_monadicBorderComp :: Property
prop_monadicBorderComp = monadicIO $ do
  a' <- computeUnboxedP (border 1 pink delayedImgArray1)
  assert (a' == borderImgArray1)

tColorInvert :: Test
tColorInvert = "tColorInvert" ~:
  computeS (colorInvert delayedImgArray1) ~?= colorInvertImgArray1

-- | parallel comp
prop_monadicColorInvertComp :: Property
prop_monadicColorInvertComp = monadicIO $ do
  a' <- computeUnboxedP (colorInvert delayedImgArray1)
  assert (a' == colorInvertImgArray1)

tFill :: Test
tFill = "tFill" ~:
  computeS (fill pink delayedImgArray1) ~?= fillImgArray1

prop_monadicFillComp :: Property
prop_monadicFillComp = monadicIO $ do
  a' <- computeUnboxedP (fill pink delayedImgArray1)
  assert (a' == fillImgArray1)

tFlipHOnce :: Test
tFlipHOnce = "tFlipHOnce" ~:
  computeS (flipH delayedImgArray1) ~?= flipHImgArray1

prop_monadicFlipHComp :: Property
prop_monadicFlipHComp = monadicIO $ do
  a' <- computeUnboxedP (flipH delayedImgArray1)
  assert (a' == flipHImgArray1)

tFlipVOnce :: Test
tFlipVOnce = "tFlipVOnce" ~:
  computeS (flipV delayedImgArray1) ~?= flipVImgArray1

prop_monadicFlipVComp :: Property
prop_monadicFlipVComp = monadicIO $ do
  a' <- computeUnboxedP (flipV delayedImgArray1)
  assert (a' == flipVImgArray1)

tFlipHTwice :: Test
tFlipHTwice = "tFlipHTwice" ~:
  computeS (flipH (flipH delayedImgArray1)) ~?= imgArray1

prop_monadicFlipHTwiceComp :: Property
prop_monadicFlipHTwiceComp = monadicIO $ do
  a' <- computeUnboxedP (flipH (flipH delayedImgArray1))
  assert (a' == imgArray1)

tFlipVTwice :: Test
tFlipVTwice = "tFlipVTwice" ~:
  computeS (flipV (flipV delayedImgArray1)) ~?= imgArray1

prop_monadicFlipVTwiceComp :: Property
prop_monadicFlipVTwiceComp = monadicIO $ do
  a' <- computeUnboxedP (flipV (flipV delayedImgArray1))
  assert (a' == imgArray1)

tGrayscale :: Test
tGrayscale = "tGrayscale" ~:
  computeS (grayscale delayedImgArray1) ~?= grayImgArray1

prop_monadicGrayscaleComp :: Property
prop_monadicGrayscaleComp = monadicIO $ do
  a' <- computeUnboxedP (grayscale delayedImgArray1)
  assert (a' == grayImgArray1)

tRotateCWCCW :: Test
tRotateCWCCW = "tRotateCW-CCW" ~:
  computeS (rotateCW (rotateCCW delayedImgArray1)) ~?= imgArray1

prop_monadicRotateCWCCWComp :: Property
prop_monadicRotateCWCCWComp = monadicIO $ do
  a' <- computeUnboxedP (rotateCW (rotateCCW delayedImgArray1))
  assert (a' == imgArray1)

tRotateCCWCW :: Test
tRotateCCWCW = "tRotateCCW-CW" ~:
  computeS (rotateCCW (rotateCW delayedImgArray1)) ~?= imgArray1

prop_monadicRotateCCWCWComp :: Property
prop_monadicRotateCCWCWComp = monadicIO $ do
  a' <- computeUnboxedP (rotateCCW (rotateCW delayedImgArray1))
  assert (a' == imgArray1)

--QuickCheckN --
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n}

--Arbitrary instance of RGB8 --

-- | generator for triplets
genTriple :: Gen a -> Gen b -> Gen c -> Gen (a,b,c)
genTriple = liftM3 (\x y z -> (x,y,z))

-- | generator for RGB8 type
genRGB8 :: Gen RGB8
genRGB8 = genTriple (arbitrary :: Gen Word8) (arbitrary :: Gen Word8) (arbitrary :: Gen Word8)

--Properties to test--

prop_flipHTwice :: Int -> Int -> Property
prop_flipHTwice w h =
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (flipH (flipH (delay a))) == a

prop_flipVTwice :: Int -> Int -> Property
prop_flipVTwice w h =
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (flipV (flipV (delay a))) == a

prop_rotateCWCCW :: Int -> Int -> Property
prop_rotateCWCCW w h =
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (rotateCW (rotateCCW (delay a))) == a

prop_rotateCCWCW :: Int -> Int -> Property
prop_rotateCCWCW w h=
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (rotateCCW (rotateCW (delay a))) == a

prop_rotateCWx4 :: Int -> Int -> Property
prop_rotateCWx4 w h =
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (rotateCW (rotateCW (rotateCW (rotateCW (delay a))))) == a

prop_rotateCCWx4 :: Int -> Int -> Property
prop_rotateCCWx4 w h =
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (rotateCCW (rotateCCW (rotateCCW (rotateCCW (delay a))))) == a

prop_invColor :: Int -> Int -> Property
prop_invColor w h=
  w > 0 && h > 0 ==>
    R.forAllUShaped
      (Z :. w :. h :: DIM2) $ \a -> computeS (colorInvert (colorInvert (delay a))) == a

-------------- Evaluator Tests ------

prop_monadicEvalID :: Property
prop_monadicEvalID = monadicIO $ do
  a' <- run (evaluateIT (FP "data/flower.jpg"))
  b' <- run (evaluateIT (FP "data/flower.jpg"))
  assert (a' == b')

prop_monadicEvalID2 :: Property
prop_monadicEvalID2 = monadicIO $ do
  a' <- run (evaluateIT (FP "data/flower2.jpg"))
  run (write "data/flower3.jpg" (return a'))
  b' <- run (evaluateIT (FP "data/flower3.jpg"))
  run (write "data/flower4.jpg" (return a'))
  c' <- run (evaluateIT (FP "data/flower4.jpg"))
  case (b', c') of
    (Right b'', Right c'') -> do
      computedB <- R.computeUnboxedP b''
      computedC <- R.computeUnboxedP c''
      assert (computedB == computedC)
    _  -> assert (1 == 0)

prop_monadicEvalBlur :: Property
prop_monadicEvalBlur = monadicIO $ do
  a' <- run (evaluateIT (FP "data/blurflower2.jpg"))
  b' <- run (evaluateIT (Blur 2 (FP "data/flower2.jpg")))
  run (write "data/tempblurflower2.jpg" (return b'))
  c' <- run (evaluateIT (FP "data/tempblurflower2.jpg"))
  case (a', c') of
    (Right a'', Right c'') -> do
      computedA <- R.computeUnboxedP a''
      computedC <- R.computeUnboxedP c''
      assert (computedA == computedC)
    _  -> assert (1 == 0)

-- Parser Tests --

sampleFP :: IT
sampleFP = FP "data/flower.jpg"

tParseOneLevel :: Test
tParseOneLevel =  "parse exp" ~: TestList [
                "border 10 (34, 23, 11)" ~:
                  P.parse IP.itP "border 10 (34, 23, 11)" ~?=
                  Left "No parses",

                "blur 2 \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "blur 2 \n filepath data/flower.jpg" ~?=
                  Right (Blur 2 sampleFP),

                "border 10 (34, 23, 11)\n filepath data/flower.jpg" ~:
                  P.parse IP.itP ("border 10 (34, 23, 11)\n" Prelude.++
                                 "filepath data/flower.jpg") ~?=
                  Right (Border 10 (34, 23, 11) sampleFP),

                "brighten 1.4 \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "brighten 1.4 \n filepath data/flower.jpg" ~?=
                  Right (Brighten 1.4 sampleFP),

                "colorInvert \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "colorInvert \n filepath data/flower.jpg" ~?=
                  Right (ColorInvert sampleFP),

                "contrast 3.2 \n filepath data/flower.jpg" ~:
                  P.parse IP.itP  "contrast 3.2 \n filepath data/flower.jpg"~?=
                  Right (Contrast 3.2 sampleFP),

                "edge\n filepath data/flower.jpg" ~:
                  P.parse IP.itP "edge\n filepath data/flower.jpg" ~?=
                  Right (Edge sampleFP),

                "fill (34, 23, 11)\n filepath data/flower.jpg" ~:
                  P.parse IP.itP ("fill (34, 23, 11)\n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Right (Fill (34, 23, 11) sampleFP),

                "flipH \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "flipH \n filepath data/flower.jpg" ~?=
                  Right (FlipH sampleFP),

                "flipV \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "flipV \n filepath data/flower.jpg" ~?=
                  Right (FlipV sampleFP),

                "grayscale \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "grayscale \n filepath data/flower.jpg" ~?=
                  Right (Grayscale sampleFP),

                "rotateCCW\n filepath data/flower.jpg" ~:
                  P.parse IP.itP "rotateCCW\n filepath data/flower.jpg" ~?=
                  Right (RotateCCW sampleFP),

                "rotateCW\n filepath data/flower.jpg" ~:
                  P.parse IP.itP "rotateCW\n filepath data/flower.jpg" ~?=
                  Right (RotateCW sampleFP),

                "saturate 3.2 \n filepath data/flower.jpg" ~:
                  P.parse IP.itP "saturate 3.2 \n filepath data/flower.jpg"  ~?=
                  Right (Saturate 3.2 sampleFP)
              ]

tParseRGB :: Test
tParseRGB =  "parse rbs" ~: TestList [
                "fill (340, 23, 11)\n filepath data/flower.jpg" ~:
                  P.parse IP.itP ("fill (340, 23, 11)\n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Left "No parses",
                "fill (34, -23, 11)\n filepath data/flower.jpg" ~:
                  P.parse IP.itP ("fill (34, -23, 11)\n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Left "No parses",
                  P.parse IP.itP ("fill (132, 230, 11)\n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Right (Fill (132, 230, 11) sampleFP)  ]

tParseTwo :: Test
tParseTwo =  "parse rbs" ~: TestList [
                "grayscale\n fill (34, 23, 11)\n filepath data/flower.jpg" ~:
                  P.parse IP.itP ("grayscale\n fill (34, 23, 11) \n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Right (Grayscale (Fill (34, 23, 11) (sampleFP))),
                "fill (34, -23, 11)\n grayscale" ~:
                  P.parse IP.itP ("fill (34, -23, 11)\n grayscale")~?=
                  Left "No parses",
                  P.parse IP.itP ("fill (132, 230, 11)\n saturate 3.2\n" Prelude.++
                                  "filepath data/flower.jpg")~?=
                  Right (Fill (132, 230, 11) (Saturate 3.2 sampleFP))  ]

tParseWSP :: Test
tParseWSP = "parse wsp" ~: TestList [
                "border   10       (34 ,   23,      11      )" ~:
                  P.parse IP.itP "border   10       (34 ,   23,      11      )" ~?=
                  Left "No parses",

                "blur  2       \n  filepath      data/flower.jpg" ~:
                  P.parse IP.itP "blur  2       \n  filepath      data/flower.jpg" ~?=
                  Right (Blur 2 sampleFP),

                "brighten       1.4 \n\n\n\n\n\n filepath    data/flower.jpg" ~:
                  P.parse IP.itP "brighten       1.4 \n\n\n\n\n\n filepath    data/flower.jpg" ~?=
                  Right (Brighten 1.4 sampleFP),

                "edge \n      colorInvert \n filepath     \n data/flower.jpg" ~:
                  P.parse IP.itP "edge \n      colorInvert \n filepath     \n data/flower.jpg" ~?=
                  Right (Edge (ColorInvert sampleFP))  ]



