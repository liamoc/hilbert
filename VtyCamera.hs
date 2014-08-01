{-# LANGUAGE BangPatterns #-}
module VtyCamera where
import Graphics.Vty
import Graphics.Vty.Prelude
import Control.Arrow
import Data.Maybe
import UIModel
import View
import Prover
import Graphics.Vty.Image.Internal -- sorry
import Data.List (intersperse)


centerOnCamera :: Image -> DisplayRegion -> Bool -> Bool -> Image 
centerOnCamera img h' lr ud = let
  (x,y) = camera img
  y_diff = fromIntegral (regionHeight h') `div` 2 - y
  shiftUpDown | y_diff > 0  = if ud then (backgroundFill 1 (fromIntegral y_diff) <->) else id
              | y_diff == 0 = id
              | otherwise   = dropImageRows (1 - y_diff)
  x_diff :: Int
  x_diff = fromIntegral (regionWidth h') `div` 2 - x
  shiftLeftRight | x_diff < 0  = dropImageCols (1 - fromIntegral x_diff)
                 | x_diff == 0 = id
                 | otherwise   = if lr then (backgroundFill (fromIntegral x_diff) 1 <|>) else id

  in crop (regionWidth h') (regionHeight h') $
   resize (regionWidth h') (regionHeight h') $ shiftUpDown $ if lr then shiftLeftRight img else img

toRows :: Image -> [[Image]]
toRows i = case dropImageRows 1 i of
             EmptyImage -> [getRow 0 i]
             x          -> getRow 0 i : toRows x 

dropImageRows :: Int -> Image -> Image
dropImageRows h i = cropTop (imageHeight i - h) i

dropImageCols :: Int -> Image -> Image
dropImageCols w i = cropLeft (imageWidth i - w) i

getRow :: Int -> Image -> [Image]
getRow 0 x@(HorizText {}) = [x]
getRow _ (HorizText {}) = []
getRow v (HorizJoin l r _ _) = getRow v l ++ getRow v r
getRow v (VertJoin t b _ _) 
   | v >= fromEnum (imageHeight t) = getRow (v - fromEnum (imageHeight t)) b
   | otherwise = getRow v t
getRow v (BGFill w h) | h > fromIntegral v = [BGFill w 1]
                      | otherwise = []
getRow v (CropLeft i _ _ _) = getRow v i
getRow v (CropRight i _ _) = getRow v i
getRow v (CropTop i j _ _) = getRow (v + j) i
getRow v (CropBottom i _ _) = getRow v i
getRow _ EmptyImage = []

invisibleQ :: Image
invisibleQ = char (defAttr {attrStyle = SetTo 0x80}) ' '

getCol' :: Int -> [Image] -> Maybe Int
getCol' (!_) [] = Nothing
getCol' (!acc) (v@(HorizText _ _ _ charW) :xs)
    | v == invisibleQ  = let tw = takeWhile (/= invisibleQ) xs
                          in  Just $  acc + (fromIntegral (imageWidth $ horizCat tw) `div` 2)
    | otherwise = getCol' (acc + fromIntegral charW) xs
getCol' (!acc) (BGFill w 1:xs) = getCol' (acc + fromIntegral w) xs
getCol' _ _ = error "Getcol'"

camera :: Image -> (Int, Int)
camera i = camera' $ toRows i
  where camera' :: [[Image]] -> (Int, Int)
        camera' imgs = let (rs', cls') = span isNothing $ map (getCol' 0) imgs
                        in case (rs', cls') of
                           ([], []) -> ( fromIntegral $ imageWidth  i `div` 2
                                       , fromIntegral $ imageHeight i `div` 2
                                       )
                           (_, Just v:_) -> (v, length rs')
                           (_, []) -> (0, length rs')
                           x   -> error  $ "camera" ++ show x

