{-# LANGUAGE RecordWildCards #-}
module Display where
import View
import VtyCamera
import Graphics.Vty
import Data.List 
import Graphics.Vty.Prelude
import Control.Arrow

data DisplaySkin = DS { titleAttr :: Bool -> ViewMode -> Attr
                      , sentenceAttr :: ViewMode -> Attr
                      , skolemAttr :: ViewMode -> Attr
                      , variableAttr :: ViewMode -> Attr
                      , skolemIntroAttr :: ViewMode -> Attr
                      , ruleIntroAttr :: ViewMode -> Attr
                      , ruleVarAttr :: ViewMode -> Attr
                      , separatePremises :: Image 
                      , vinculumPadding :: Int
                      , vinculumAttr   :: ViewMode -> Attr
                      , vinculumChar   :: IsHypothetical -> Char
                      , vinculumCenterChar :: IsHypothetical -> Char
                      , verticalLine :: Char
                      , unprovenChar :: Char
                      , provenChar :: Char
                      , topCornerChar :: Char
                      , horizontalLine :: Char
                      , displayTopBar :: IsHypothetical -> Bool
                      , premiseLeftAnnot :: ViewMode -> (Attr, String)
                      , premiseRightAnnot :: ViewMode -> (Attr, String)
                      , vincLeftAnnot :: ViewMode -> (Attr, String)
                      , vincRightAnnot :: ViewMode -> (Attr, String)
                      , rulesPanelVertPadding :: Int
                      , rulesPanelCenterRules :: Bool 
                      , bgAttr :: Attr
                      , showSchematicDependencies :: Bool
                      }


displayBar :: DisplaySkin -> BarView -> String 
displayBar sk x = intersperse (Str $ " " ++ verticalLine sk : " ") x >>= displaySegment
   where displaySegment (Str s) = s
         displaySegment (Bullets l r) = replicate l (unprovenChar sk)
                                     ++ [provenChar sk]
                                     ++ replicate r (unprovenChar sk)

displayScreenView :: DisplaySkin -> ScreenView -> DisplayRegion -> Image
displayScreenView sk (v, sv, message, w) h = 
   let img = displayView sk v
       (title, img') = displaySideView sk sv
       bottomBar = string (defAttr `withStyle` reverseVideo) (displayBar sk message)
                <|> charFill (defAttr `withStyle` reverseVideo) ' ' (fst h - length message) 1 
    in (centerOnCamera img (second (subtract 1) . first (subtract w) $ h) True True
    <|> (string defAttr [topCornerChar sk] <-> charFill defAttr (verticalLine sk) 1 (regionHeight h - 2))
    <|> (string defAttr (horizontalLine sk:' ':title ++ " " ++ replicate (w - 4 - length title) (horizontalLine sk))
         <-> centerOnCamera img' (w - 1, snd h - 2) (rulesPanelCenterRules sk) False))
    <-> bottomBar

displaySideView :: DisplaySkin -> SideView -> (String, Image)
displaySideView sk (NormalView s) = ("Available Local Facts", vertCatSk sk $ (string defAttr " ":) $ intersperse (backgroundFill 1 $ rulesPanelVertPadding sk)
                                                                           $ map (displayView sk) s)
displaySideView sk (SelectingView ls c rs) = ((,) "Compatible Rules") $ vertCatSk sk $ (string defAttr " ":) $  intersperse (backgroundFill 1 $ rulesPanelVertPadding sk) $ rules 
     where rules = map (displayView sk) ls ++ [displayView sk c] ++ map (displayView sk) rs

vertCatSk sk = if rulesPanelCenterRules sk then vertCatMid else vertCat

displayView :: DisplaySkin -> View -> Image
displayView sk@(DS {..}) (ViewNode m hyp tks title cs) = let 
    sidenote = displayViewTitle sk title m
    conclusion = displayViewSchema sk m tks
    premise = horizCatBot $ [uncurry string (premiseLeftAnnot m )]
                         ++ intersperse separatePremises (map (displayView sk) cs)
                         ++ [uncurry string (premiseRightAnnot m)]
    vinculum = vinculumFor sk m (fromIntegral $ imageWidth premise) 
                                (fromIntegral $ imageWidth conclusion) 
                                (fromIntegral $ imageWidth sidenote)
                                hyp (null cs)
    middle   = uncurry string (vincLeftAnnot m) 
           <|> invisibleQ' <|> vinculum <|> invisibleQ' <|> sidenote <|> char bgAttr ' '
           <|> uncurry string (vincRightAnnot m)
  in vertCatMid $ [premise, middle, conclusion]
 where invisibleQ' = case m of 
         Selection -> invisibleQ
         Selecting {} -> invisibleQ
         _         -> char bgAttr ' '

vinculumFor :: DisplaySkin -> ViewMode -> Int -> Int -> Int -> IsHypothetical -> Bool -> Image
vinculumFor (DS {..}) m prem conc label isHypothetical nocs = let
    size = max  (max prem conc + vinculumPadding ) 4
    half1 = fromIntegral size `div` 2
    half2 = size - half1 - 1 - (label `div` 2)
  in string (vinculumAttr m) (replicate half1 (vinculumChar isHypothetical) 
                          ++ ((if nocs then vinculumChar else vinculumCenterChar) isHypothetical:replicate half2 (vinculumChar isHypothetical)))

horizCatBot, vertCatMid :: [Image] -> Image
horizCatBot = horizCat . uniform
  where 
    uniform ls = let m = maximum $ map imageHeight ls
                  in map (\i -> backgroundFill (imageWidth i) (m - imageHeight i) <-> i) ls
vertCatMid = vertCat . uniform'
  where
    uniform' ls = let m = maximum $ map imageWidth ls
                   in map (\i -> let p1 = (m - imageWidth i) `div` 2
                                     p2 = (m - imageWidth i) - p1
                                  in horizCat [ backgroundFill p1 (imageHeight i)
                                              , i
                                              , backgroundFill p2 (imageHeight i)])
                          ls

displayViewTitle :: DisplaySkin -> RuleTitle -> ViewMode -> Image
displayViewTitle _ NoRule _ = emptyImage
displayViewTitle (DS {..}) (Proven s (I sk rs)) m = string (titleAttr True m)  s <|> string (sentenceAttr m) " " <|> horizCat (intersperse (string (sentenceAttr m) " ") $ map (string (skolemIntroAttr m)) sk ++ map (string (ruleIntroAttr m)) rs) 
displayViewTitle (DS {..}) (Unproven s (I sk rs)) m = string (titleAttr False m) s <|> string (sentenceAttr m) " " <|> horizCat (intersperse (string (sentenceAttr m) " ") $ map (string (skolemIntroAttr m)) sk ++ map (string (ruleIntroAttr m)) rs) 


displayViewSchema :: DisplaySkin -> ViewMode -> SentenceView -> Image
displayViewSchema sk@(DS {..}) m (ViewList ss) = horizCat (intersperse (string (sentenceAttr m) " ") $ map (displayViewSchema' sk m) ss) 
displayViewSchema sk n v = displayViewSchema' sk n v
displayViewSchema' :: DisplaySkin -> ViewMode -> SentenceView -> Image
displayViewSchema' sk@(DS {..}) m (ViewList ss) = string (sentenceAttr m) "(" 
                                              <|> displayViewSchema sk m (ViewList ss)
                                              <|> string (sentenceAttr m) ")"
displayViewSchema' (DS {..}) m (ViewVariable s ds) 
    | showSchematicDependencies && not (null ds) = displayViewSchema' DS {showSchematicDependencies = False, ..} m (ViewList (ViewVariable s []: map ViewSkolem ds))
    | otherwise =  string (variableAttr m) s 
displayViewSchema' (DS {..}) m (ViewSkolem s) = string (skolemAttr m) s 
displayViewSchema' (DS {..}) m (ViewRuleVar s) = string (ruleVarAttr m) s 
displayViewSchema' (DS {..}) m (ViewSymbol s) = string (sentenceAttr m) s 
                                              

