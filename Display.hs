{-# LANGUAGE RecordWildCards #-}
module Display where
import View
import Graphics.Vty
import Data.List 
data DisplaySkin = DS { titleAttr :: Bool -> ViewMode -> Attr
                      , sentenceAttr :: ViewMode -> Attr
                      , skolemAttr :: ViewMode -> Attr
                      , variableAttr :: ViewMode -> Attr
                      , skolemIntroAttr :: ViewMode -> Attr
                      , ruleIntroAttr :: ViewMode -> Attr
                      , separatePremises :: Image 
                      , vinculumPadding :: Int
                      , vinculumAttr   :: ViewMode -> Attr
                      , vinculumChar   :: Char
                      , premiseLeftAnnot :: ViewMode -> (Attr, String)
                      , premiseRightAnnot :: ViewMode -> (Attr, String)
                      , vincLeftAnnot :: ViewMode -> (Attr, String)
                      , vincRightAnnot :: ViewMode -> (Attr, String)
                      , bgAttr :: Attr
                      , showSchematicDependencies :: Bool
                      }

displayView :: DisplaySkin -> View -> Image
displayView sk@(DS {..}) (ViewNode m tks title cs) = let 
    sidenote = displayViewTitle sk title m
    conclusion = displayViewSchema sk m tks
    premise = horizCatBot $ [uncurry string (premiseLeftAnnot m )]
                         ++ intersperse separatePremises (map (displayView sk) cs)
                         ++ [uncurry string (premiseRightAnnot m)]
    vinculum = vinculumFor sk m (fromIntegral $ image_width premise) 
                                (fromIntegral $ image_width conclusion) 
                                (fromIntegral $ image_width sidenote)
    middle   = uncurry string (vincLeftAnnot m) 
           <|> invisibleQ' <|> vinculum <|> invisibleQ' <|> sidenote <|> char bgAttr ' '
           <|> uncurry string (vincRightAnnot m)
  in vertCatMid [premise,middle,conclusion]
 where invisibleQ' = case m of 
         Selection -> invisibleQ
         Selecting {} -> invisibleQ
         _         -> char bgAttr ' '
vinculumFor :: DisplaySkin -> ViewMode -> Int -> Int -> Int -> Image
vinculumFor (DS {..}) m prem conc label = let
    size = max  (max prem conc + vinculumPadding - (label `div` 2)) 4
  in string (vinculumAttr m) (replicate (fromIntegral size) vinculumChar)

horizCatBot, vertCatMid :: [Image] -> Image
horizCatBot = horiz_cat . uniform
  where 
    uniform ls = let m = maximum $ map image_height ls
                  in map (\i -> background_fill (image_width i) (m - image_height i) <-> i) ls
vertCatMid = vert_cat . uniform'
  where
    uniform' ls = let m = maximum $ map image_width ls
                   in map (\i -> let p1 = (m - image_width i) `div` 2
                                     p2 = (m - image_width i) - p1
                                  in horiz_cat [ background_fill p1 (image_height i)
                                               , i
                                               , background_fill p2 (image_height i)])
                          ls

displayViewTitle :: DisplaySkin -> RuleTitle -> ViewMode -> Image
displayViewTitle _ NoRule _ = empty_image
displayViewTitle (DS {..}) (Proven s (I sk rs)) m = string (titleAttr True m)  s <|> string (sentenceAttr m) " " <|> horiz_cat (intersperse (string (sentenceAttr m) " ") $ map (string (skolemIntroAttr m)) sk ++ map (string (ruleIntroAttr m)) rs) 
displayViewTitle (DS {..}) (Unproven s (I sk rs)) m = string (titleAttr False m) s <|> string (sentenceAttr m) " " <|> horiz_cat (intersperse (string (sentenceAttr m) " ") $ map (string (skolemIntroAttr m)) sk ++ map (string (ruleIntroAttr m)) rs) 


displayViewSchema :: DisplaySkin -> ViewMode -> SentenceView -> Image
displayViewSchema sk@(DS {..}) m (ViewList ss) = horiz_cat (intersperse (string (sentenceAttr m) " ") $ map (displayViewSchema' sk m) ss) 
displayViewSchema sk n v = displayViewSchema' sk n v
displayViewSchema' :: DisplaySkin -> ViewMode -> SentenceView -> Image
displayViewSchema' sk@(DS {..}) m (ViewList ss) = string (sentenceAttr m) "(" 
                                              <|> displayViewSchema sk m (ViewList ss)
                                              <|> string (sentenceAttr m) ")"
displayViewSchema' (DS {..}) m (ViewVariable s ds) 
    | showSchematicDependencies && not (null ds) = displayViewSchema' DS {showSchematicDependencies = False, ..} m (ViewList (ViewVariable s []: map ViewSkolem ds))
    | otherwise =  string (variableAttr m) s 
displayViewSchema' (DS {..}) m (ViewSkolem s) = string (skolemAttr m) s 
displayViewSchema' (DS {..}) m (ViewSymbol s) = string (sentenceAttr m) s 
                                              

invisibleQ :: Image
invisibleQ = char (def_attr {attr_style = SetTo 0x80}) ' '
