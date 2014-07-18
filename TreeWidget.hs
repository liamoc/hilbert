{-# LANGUAGE BangPatterns #-}
module TreeWidget where
import Graphics.Vty.LLInput
import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.EventLoop
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
data KeyAction = ArbitraryIO (IO ())
               | MoveForward
               | MoveBack
               | MovePrev
               | MoveNext
               | ClearSubtree
               | EnterRulesMode
               | NextAssignment
               | PrevAssignment
               | SubstituteVars


type KeyBindings = [(Key, KeyAction)]
type PromptString = String
type ResponseString = String
type InputCallback = (PromptString -> IO (MVar ResponseString)) 

keyActionToIO :: [Rule] -> InputCallback -> KeyAction -> Widget Model -> IO ()
keyActionToIO _ _ (ArbitraryIO i ) _ = i
keyActionToIO _ _ (MoveForward   ) r = updateWidgetState r forward 
keyActionToIO _ _ (MoveBack      ) r = updateWidgetState r back
keyActionToIO _ _ (MovePrev      ) r = updateWidgetState r prev
keyActionToIO _ _ (MoveNext      ) r = updateWidgetState r next
keyActionToIO _ _ (ClearSubtree  ) r = updateWidgetState r clearSubtree
keyActionToIO l _ (EnterRulesMode) r = updateWidgetState r (rulemode l) 
keyActionToIO _ _ (NextAssignment) r = updateWidgetState r nextVariant 
keyActionToIO _ _ (PrevAssignment) r = updateWidgetState r prevVariant   
keyActionToIO _ c (SubstituteVars) r = do (st, st') <- (     allFreeVariables 
                                                         &&& allFreeVariables . back
                                                       ) <$> getState r
                                          if null st && not (null st') 
                                            then do
                                              updateWidgetState r back
                                              handleVarSubst $ nub st'
                                            else handleVarSubst $ nub st
  where
    handleVarSubst (x:xs) = do mv <- c $ "Enter assignment for: " ++ x
                               _ <- forkIO $ do maybev <- takeMVar mv
                                                case parseTerm maybev of 
                                                  Just v -> do schedule $ updateWidgetState r $ subst x v
                                                               handleVarSubst xs
                                                  Nothing -> return ()
                               return ()
    handleVarSubst [] = return ()
    allFreeVariables (Selected ( PTZ fv _ _)) = fv 
    allFreeVariables _ = []
proofTreeWidget :: [Rule] -> InputCallback -> DisplaySkin -> KeyBindings -> IO (Widget Model)
proofTreeWidget rules callback sk binds = newWidget (newModel (List [])) $ \w ->
      w { render_ = \ref h _ ->  toImage sk ref h 
        , keyEventHandler = \ ref k _ -> case lookup k binds of
                                           Just x  -> keyActionToIO rules callback x ref 
                                                   >> return True
                                           Nothing -> return False
        , growHorizontal_ = const $ return True
        , growVertical_ = const $ return True
        } 

toImage :: DisplaySkin -> Widget Model -> DisplayRegion -> IO Image
toImage sk ref h = do 
   img <- displayView sk . viewModel <$> getState ref 
   let (x,y) = camera img
       y_diff = fromIntegral (region_height h) `div` 2 - y
       shiftUpDown | y_diff > 0  = (background_fill (image_width img) (fromIntegral y_diff) <->)
                   | y_diff == 0 = id
                   | otherwise   = dropImageRows (negate y_diff)
       x_diff :: Int
       x_diff = fromIntegral (region_width h) `div` 2 - x
       shiftLeftRight | x_diff < 0  = dropImageCols (negate $ fromIntegral x_diff)
                      | x_diff == 0 = id
                      | otherwise   = (background_fill (fromIntegral x_diff) (image_height img) <|>)

   return $ crop (region_width h, region_height h ) 
          $ pad  (region_width h, region_height h) 
          $ shiftUpDown $ shiftLeftRight img 

toRows :: Image -> [[Image]]
toRows i = case dropImageRows 1 i of
             EmptyImage -> [getRow 0 i]
             x          -> getRow 0 i : toRows x 

dropImageRows :: Int -> Image -> Image
dropImageRows 0 i = i
dropImageRows _ (HorizText {}) = EmptyImage
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

getRow :: Int -> Image -> [Image]
getRow 0 x@(HorizText {}) = [x]
getRow _ (HorizText {}) = []
getRow v (HorizJoin l r _ _) = getRow v l ++ getRow v r
getRow v (VertJoin t b _ _) 
   | v >= fromEnum (image_height t) = getRow (v - fromEnum (image_height t)) b
   | otherwise = getRow v t
getRow v (BGFill w h) | h > fromIntegral v = [BGFill w 1]
                      | otherwise = []
getRow _ EmptyImage = []
getRow v (Translation _ l) = getRow v l
getRow v (ImageCrop _ l) = getRow v l
getRow v (ImagePad _ l) = getRow v l

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

getCol' :: Int -> [Image] -> Maybe Int
getCol' (!_) [] = Nothing
getCol' (!acc) (v@(HorizText _ _ _ charWidth) :xs)
    | v == invisibleQ  = let tw = takeWhile (/= invisibleQ) xs
                          in  Just $  acc + (fromIntegral (image_width $ horiz_cat tw) `div` 2)
    | otherwise = getCol' (acc + fromIntegral charWidth) xs
getCol' (!acc) (BGFill w 1:xs) = getCol' (acc + fromIntegral w) xs
getCol' _ _ = error "Getcol'"

camera :: Image -> (Int, Int)
camera i = camera' $ toRows i
  where camera' :: [[Image]] -> (Int, Int)
        camera' imgs = let (rs', cls') = span isNothing $ map (getCol' 0) imgs
                        in case (rs', cls') of
                           ([], []) -> ( fromIntegral $ image_width  i `div` 2
                                       , fromIntegral $ image_height i `div` 2
                                       )
                           (_, Just v:_) -> (v, length rs')
                           (_, []) -> (0, length rs')
                           x   -> error  $ "camera" ++ show x

