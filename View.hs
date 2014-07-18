{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, PatternGuards #-}
module View where

import Control.Arrow
import qualified Data.Map as M
import UIModel
import Rules
import Prover
data ViewMode = Normal
                 | Speculative
                 | Selection
                 | Selecting { isNextRule :: Bool
                             , isPrevRule :: Bool
                             , isNextAssign :: Bool
                             , isPrevAssign :: Bool
                             }

data SentenceSchemaToken = TextChunk String
                         | Variable Char
                         | Substitution String

data RuleTitle = Proven String
               | Unproven String
               | NoRule

data View = ViewNode ViewMode [SentenceSchemaToken] RuleTitle [View]


viewModel :: Model -> View
viewModel (Selected p) = viewZipper Selection p
viewModel (Tentative (ZZ l (ZZ ul m ur) r) _) = viewZipper (Selecting (not $ null r)
                                                                      (not $ null l)
                                                                      (not $ null ur)
                                                                      (not $ null ul)
                                                           ) m

viewZipper :: ViewMode -> ProofTreeZipper -> View
viewZipper m (PTZ [] pt) = fst $ viewTree m M.empty pt
viewZipper m (PTZ ctx@(PTC (_, _, subst) _ _  :_) p) = fst $ viewZipper' Normal ctx $ viewTree m subst p
  where viewZipper' m (PTC (str, name, subst) l r : ctx) acc 
           = let str' = viewSentence (substOf ctx) str
                 [l', r'] = map (map (viewTree m subst)) [reverse l, r]
                 (cs, wf) = second and $ unzip $ l' ++ (acc:r')
             in viewZipper' m ctx (ViewNode m str' ((if wf then Proven else Unproven) name) cs, wf)
        viewZipper' _ [] acc = acc
        substOf [] = M.empty
        substOf (PTC (_,_, s) _ _:_) = s

downwards :: ViewMode -> ViewMode
downwards Speculative = Speculative
downwards (Selecting {}) = Speculative
downwards _ = Normal

viewTree :: ViewMode -> Substitution -> ProofTree -> (View, Bool)
viewTree m subst (PT sent ms) = case ms of 
    Just (r, subst', cs) -> let
       (cs', wf) = second and $ unzip $ map (viewTree (downwards m) subst') cs
     in (ViewNode m sent' ((if wf then Proven else Unproven) r) cs', wf)
    Nothing -> (ViewNode m sent' NoRule [], False)
  where sent' = viewSentence subst sent

viewSentence :: Substitution -> SentenceSchema -> [SentenceSchemaToken]
viewSentence subst = viewSentence' [] where         
  viewSentence' [] [] = []
  viewSentence' (!acc) [] = [TextChunk $ reverse acc]
  viewSentence' (!acc) ('?':v:r) | Just v' <- M.lookup v subst = TextChunk (reverse acc)
                                                               : Substitution v' 
                                                               : viewSentence' [] r
                                 | otherwise = TextChunk (reverse acc)
                                             : Variable v
                                             : viewSentence' [] r
  viewSentence' (!acc) (x:xs) = viewSentence' (x:acc) xs  
