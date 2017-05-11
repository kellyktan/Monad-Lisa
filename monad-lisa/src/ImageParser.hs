module ImageParser where

import Evaluator
import qualified ImageTransformation as T

import Control.Applicative

import qualified Parser as P
import qualified ParserCombinators as P

-----------------------------------------------------------------------------------------------------

wsPChar :: Char -> P.Parser Char
wsPChar = wsP . P.char

wsP :: P.Parser a -> P.Parser a
wsP p = p <* many P.space

-- | esnure int parsed in rgb (0-255 range)
intInWord8Range :: P.Parser Int
intInWord8Range = P.ensure (\a -> a >= 0 && a <= 255) P.int

-- | convert tuple of int into RGB8 type
intToRGB :: (Int, Int, Int) -> T.RGB8
intToRGB (r,g,b) = (fromIntegral (toInteger r),
                    fromIntegral (toInteger g),
                    fromIntegral (toInteger b))

-- | ignore first output of first parser and take second one
ignoreP1TakeP2 :: (a -> b) -> P.Parser c -> P.Parser a -> P.Parser b
ignoreP1TakeP2 f p1 p2 =
  f <$ p1 <*> p2

-- | parser for image transformations with 0 args
noArgItP :: (IT -> IT) -> String -> P.Parser IT
noArgItP f s =
      f <$
        wsP (P.string s) <*>
        wsP itP

-- | parser that takes one argument
oneArgItP :: (a -> IT -> IT) -> String -> P.Parser a -> P.Parser IT
oneArgItP f s p = ignoreP1TakeP2 f (wsP (P.string s)) p <*> itP

-- | parser for file path parser
filePathItP :: P.Parser IT
filePathItP = ignoreP1TakeP2 FP (wsP (P.string "filepath")) (many P.noSpace)

-- | parse (r,g,b) format to 3 int arguments to be consumed by parser p
rgbP :: P.Parser (Int -> Int -> Int -> b) -> P.Parser b
rgbP p = p <*
  wsPChar '(' <*>
  wsP intInWord8Range <*
  wsPChar ',' <*>
  wsP intInWord8Range <*
  wsPChar ',' <*>
  wsP intInWord8Range <*
  wsPChar ')'

-- | parse (r,g,b) and consume arguments for IT Parser
rgbItP :: P.Parser (Int -> Int -> Int -> IT -> IT) -> P.Parser IT
rgbItP p =
  rgbP p <*> itP

-- | parser for image transformations structure
itP :: P.Parser IT
itP = filePathItP                           <|>
      noArgItP    Edge        "edge"        <|>
      noArgItP    FlipH       "flipH"       <|>
      noArgItP    FlipV       "flipV"       <|>
      noArgItP    Grayscale   "grayscale"   <|>
      noArgItP    Sharpen     "sharpen"     <|>
      noArgItP    RotateCW    "rotateCW"    <|>
      noArgItP    RotateCCW   "rotateCCW"   <|>
      noArgItP    ColorInvert "colorInvert" <|>
      oneArgItP   Contrast    "contrast"   (wsP P.float) <|>
      oneArgItP   Brighten    "brighten"   (wsP P.float) <|>
      oneArgItP   Saturate    "saturate"   (wsP P.float) <|>
      oneArgItP   Blur        "blur"       (wsP P.int)   <|>
      rgbItP ((\ r g b i -> Fill (intToRGB (r,g,b)) i) <$ wsP (P.string "fill")) <|>
      rgbItP ((\ w r g b i -> Border w (intToRGB (r,g,b)) i) <$ wsP (P.string "border") <*> wsP P.int)

--itP :: P.Parser IT
--itP = FP <$ (wsP (P.string "filepath")) <*>
--          many P.noSpace <|>
--      Grayscale <$
--          (wsP (P.string "grayscale")) <*>
--           wsP itP <|>
--      Blur <$
--          (wsP (P.string "blur")) <*>
--          wsP P.int <*>
--          wsP itP <|>
--      Sharpen <$
--          (wsP (P.string "sharpen")) <*>
--          wsP itP <|>
--      Edge <$
--          (wsP (P.string "edge")) <*>
--          wsP itP <|>
--      Contrast <$
--          (wsP (P.string "contrast")) <*>
--          wsP P.float <*>
--          wsP itP <|>
--      RotateCW <$
--          (wsP (P.string "rotateCW")) <*>
--          wsP itP <|>
--      RotateCCW <$
--          (wsP (P.string "rotateCCW")) <*>
--          wsP itP <|>
--      FlipH <$
--          (wsP (P.string "flipH")) <*>
--          wsP itP <|>
--      FlipV <$
--          (wsP (P.string "flipV")) <*>
--          wsP itP <|>
--      (\ r g b i -> Fill (intToRGB (r,g,b)) i) <$
--          (wsPStrings ["fill", "("]) <*>
--          wsP P.int <*
--          wsPChar ',' <*>
--          wsP P.int <*
--          wsPChar ',' <*>
--          wsP P.int <*
--          wsPChar ')' <*>
--          wsP itP <|>
--      ColorInvert <$
--          (wsP (P.string "colorInvert")) <*>
--          wsP itP <|>
--      Brighten <$
--         (wsP (P.string "brighten")) <*>
--         wsP P.float <*>
--         wsP itP <|>
--     (\ w r g b i -> Border w (intToRGB (r,g,b)) i) <$
--          wsPStrings ["border"] <*>
--          wsP P.int <*
--          wsPChar '(' <*>
--          wsP P.int <*
--          wsPChar ',' <*>
--          wsP P.int <*
--          wsPChar ',' <*>
--          wsP P.int <*
--          wsPChar ')' <*>
--          wsP itP <|>
--     Saturate <$
--         wsPStrings ["saturate"] <*>
--         wsP P.float <*>
--         wsP itP
--
--itP :: P.Parser IT
--itP = (\_ f -> FP f) <$>
--          wsPStrings ["filepath"] <*>
--          many P.noSpace <|>
--      (\ _ i-> Grayscale i) <$>
--          wsPStrings ["grayscale"] <*>
--          wsP itP <|>
--      (\ _ r i -> Blur r i) <$>
--          wsPStrings ["blur"] <*>
--          wsP P.int <*>
--          wsP itP <|>
--      (\ _ i -> Sharpen i) <$>
--          wsPStrings ["sharpen"] <*>
--          wsP itP <|>
--      (\ _ i -> Edge i) <$>
--          wsPStrings ["edge"] <*>
--          wsP itP <|>
--      (\ _ r i -> Contrast r i) <$>
--          wsPStrings ["contrast"] <*>
--          wsP P.float <*>
--          wsP itP <|>
--      (\ _ i -> RotateCW i) <$>
--          wsPStrings ["rotateCW"] <*>
--          wsP itP <|>
--      (\ _ i -> RotateCCW i) <$>
--          wsPStrings ["rotateCCW"] <*>
--          wsP itP <|>
--      (\ _ i -> FlipH i) <$>
--          wsPStrings ["flipH"] <*>
--          wsP itP <|>
--      (\ _ i -> FlipV i) <$>
--          wsPStrings ["flipV"] <*>
--          wsP itP <|>
--      (\ _ r _ g _ b _ i -> Fill (intToRGB (r,g,b)) i) <$>
--          wsPStrings ["fill", "("] <*>
--          wsP P.int <*>
--          wsPChar ',' <*>
--          wsP P.int <*>
--          wsPChar ',' <*>
--          wsP P.int <*>
--          wsPChar ')' <*>
--          wsP itP <|>
--      (\ _ i -> ColorInvert i) <$>
--          wsPStrings ["colorInvert"] <*>
--          wsP itP <|>
--      (\ _ f i -> Brighten f i) <$>
--          wsPStrings ["brighten"] <*>
--          wsP P.float <*>
--          wsP itP <|>
--      (\ _ w _ r _ g _ b _ i -> Border w (intToRGB (r,g,b)) i) <$>
--          wsPStrings ["border"] <*>
--          wsP P.int <*>
--          wsPChar '(' <*>
--          wsP P.int <*>
--          wsPChar ',' <*>
--          wsP P.int <*>
--          wsPChar ',' <*>
--          wsP P.int <*>
--          wsPChar ')' <*>
--          wsP itP <|>
--      (\ _ f i -> Saturate f i) <$>
--          wsPStrings ["saturate"] <*>
--          wsP P.float <*>
--          wsP itP
--
