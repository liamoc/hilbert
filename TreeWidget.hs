{-# LANGUAGE BangPatterns #-}
module TreeWidget where
import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Prelude
import qualified Data.Foldable as F
import Control.Concurrent
import Control.Arrow
import Control.Applicative hiding ((<|>))
import Data.List(nub)
import Data.Maybe
import UIModel
import Rules hiding (subst)
import Display
import View
import Prover
import Parser
import Data.IORef
import Graphics.Vty.Image.Internal -- sorry

data KeyAction = MoveForward
               | MoveBack
               | MovePrev
               | MoveNext
               | ClearSubtree
               | EnterRulesMode
               | NextAssignment
               | PrevAssignment
               | NextLemma
               | PrevLemma
               | SubstituteVars
               | DecreasePane
               | IncreasePane


type KeyBindings = [(Key, KeyAction)]
type PromptString = String
type ResponseString = String
type InputCallback = (PromptString -> IO (MVar ResponseString)) 
type StringCallback = (PromptString -> IO ()) 

type Widget a = IORef a

keyActionToOp :: KeyAction -> Model -> Model
keyActionToOp  (MoveForward   ) = forward 
keyActionToOp  (MoveBack      ) = back
keyActionToOp  (MovePrev      ) = prev
keyActionToOp  (MoveNext      ) = next
keyActionToOp  (ClearSubtree  ) = clearSubtree
keyActionToOp  (EnterRulesMode) = rulemode 
keyActionToOp  (NextAssignment) = nextVariant 
keyActionToOp  (PrevAssignment) = prevVariant   
keyActionToOp  (NextLemma     ) = nextLemma
keyActionToOp  (PrevLemma     ) = prevLemma
keyActionToOp  (DecreasePane  ) = (second (max 0 . subtract 1))
keyActionToOp  (IncreasePane  ) = (second (min 70 . succ))
keyActionToOp  (SubstituteVars) = \m -> let (st, st') = (     allFreeVariables 
                                                         &&& allFreeVariables . backP
                                                       ) . snd . derefLZ . fst $ m
                                        in if null st && not (null st') 
                                            then 
                                              back m
                                            else m
  where
    allFreeVariables (Selected ( PTZ fv _ _)) = fv 
    allFreeVariables _ = []
{-
updateBar :: Widget Model -> StringCallback -> IO ()
updateBar ref c = do
    <- readIORef ref 
   c $ 
-}

toImage :: DisplaySkin -> Model -> DisplayRegion -> Image
toImage sk ref h = do 
   let (proofModel, w) = (snd . derefLZ *** fromIntegral) ref
       img = displayView sk $ viewModel proofModel
       img' = displaySideView sk $ sideViewModel proofModel
       bottomBar = let (ZZ l ((n,_),_) r, _) = ref
                       message = "Hilbert 2.0 " --
                              ++ verticalLine sk : replicate (length l) (unprovenChar sk)
                              ++ provenChar sk : replicate (length r) (unprovenChar sk)
                              ++ verticalLine sk : toSubscript n 
                    in string (defAttr `withStyle` reverseVideo) message 
                   <|> charFill (defAttr `withStyle` reverseVideo) ' ' (fst h - length message) 1 
    in (centerOnCamera img (second (subtract 1) . first (subtract w) $ h) True
    <|> charFill defAttr (verticalLine sk) 1 (regionHeight h - 1)
    <|> centerOnCamera img' (w - 1, snd h - 1) False)
    <-> bottomBar

centerOnCamera :: Image -> DisplayRegion -> Bool -> Image 
centerOnCamera img h' lr = let
  (x,y) = camera img
  y_diff = fromIntegral (regionHeight h') `div` 2 - y
  shiftUpDown | y_diff > 0  = if lr then (backgroundFill 1 (fromIntegral y_diff) <->) else id
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

{-dropImageRows _ (HorizText {}) = EmptyImage
dropImageRows v (HorizJoin l r _ _) = dropImageRows v l <|> dropImageRows v r
dropImageRows v (VertJoin t b _ _) 
    | v >= fromEnum (image_height t) = dropImageRows (v - fromEnum (image_height t)) b
    | otherwise = dropImageRows v t <-> b
dropImageRows v (BGFill w h) | h > fromIntegral v = BGFill w (h - fromIntegral v)
                             | otherwise = EmptyImage
dropImageRows _ EmptyImage = EmptyImage
dropImageRows v (Translation c i) = Translation c $ dropImageRows v i
dropImageRows v (ImageCrop c i) = ImageCrop c $ dropImageRows v i
dropImageRows v (ImagePad c i) = ImagePad c $ dropImageRows v i
-}
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

{-
dropImageCols :: Int -> Image -> Image
dropImageCols 0 i = i
dropImageCols v (HorizText at txt _ _) = string at (drop v $ map fst $ F.toList txt)
dropImageCols v (VertJoin t b _ _) = dropImageCols v t <-> dropImageCols v b
dropImageCols v (HorizJoin l r _ _) 
    | v >= fromEnum (image_width l) = dropImageCols (v - fromEnum (image_width l)) r
    | otherwise                     = dropImageCols v l <|> r
dropImageCols v (BGFill w h) | w > fromIntegral v = BGFill (w - fromIntegral v) h
                             | otherwise = EmptyImage
dropImageCols _ (EmptyImage) = EmptyImage
dropImageCols v (Translation c i) = Translation c $ dropImageCols v i
dropImageCols v (ImageCrop c i) = ImageCrop c $ dropImageCols v i
dropImageCols v (ImagePad c i) = ImagePad c $ dropImageCols v i
-}
getCol' :: Int -> [Image] -> Maybe Int
getCol' (!_) [] = Nothing
getCol' (!acc) (v@(HorizText _ _ _ charWidth) :xs)
    | v == invisibleQ  = let tw = takeWhile (/= invisibleQ) xs
                          in  Just $  acc + (fromIntegral (imageWidth $ horizCat tw) `div` 2)
    | otherwise = getCol' (acc + fromIntegral charWidth) xs
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

