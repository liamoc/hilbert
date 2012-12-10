{-# LANGUAGE RecordWildCards #-}
module Display where
import View
import Graphics.Vty
import Data.List 

data DisplaySkin = DS { titleAttr :: Bool -> ViewMode -> Attr
                      , sentenceAttr :: ViewMode -> Attr
                      , substitutionAttr :: ViewMode -> Attr
                      , variableAttr :: ViewMode -> Attr
                      , separatePremises :: Image 
                      , vinculumPadding :: Int
                      , vinculumAttr   :: ViewMode -> Attr
                      , vinculumChar   :: Char
                      , premiseLeftAnnot :: ViewMode -> (Attr, String)
                      , premiseRightAnnot :: ViewMode -> (Attr, String)
                      , vincLeftAnnot :: ViewMode -> (Attr, String)
                      , vincRightAnnot :: ViewMode -> (Attr, String)
                      , bgAttr :: Attr
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
displayViewTitle (DS {..}) (Proven s) m = string (titleAttr True m)  s
displayViewTitle (DS {..}) (Unproven s) m = string (titleAttr False m) s


displayViewSchema :: DisplaySkin -> ViewMode -> [SentenceSchemaToken] -> Image
displayViewSchema _ _ [] = empty_image
displayViewSchema sk@(DS {..}) m (TextChunk s : rest) = string (sentenceAttr m) s 
                                                    <|> displayViewSchema sk m rest
displayViewSchema sk@(DS {..}) m (Substitution s : rest) = string (substitutionAttr m) s 
                                                    <|> displayViewSchema sk m rest
displayViewSchema sk@(DS {..}) m (Variable s : rest) = char (variableAttr m) s 
                                                    <|> displayViewSchema sk m rest

invisibleQ :: Image
invisibleQ = char (def_attr {attr_style = SetTo 0x80}) ' '
