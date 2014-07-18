{-# OPTIONS -fno-warn-name-shadowing #-}
module View where

import Control.Arrow
import Data.Monoid
import UIModel
import Data.Char
import Rules
import Prover
data ViewMode    = Normal
                 | Speculative
                 | Selection
                 | Selecting { isNextRule :: Bool
                             , isPrevRule :: Bool
                             , isNextAssign :: Bool
                             , isPrevAssign :: Bool
                             }

data SentenceView = ViewList [SentenceView]
                  | ViewVariable String [String]
                  | ViewSkolem String
                  | ViewSymbol String

data RuleTitle = Proven String Intros
               | Unproven String Intros
               | NoRule


data Intros = I [Variable] [RuleName]


instance Monoid Intros where 
  mempty = I [] []
  mappend (I a b) (I c d) = I (a ++ c) (b ++ d)

data View = ViewNode ViewMode SentenceView RuleTitle [View]


viewModel :: Model -> View
viewModel (Selected p) = viewZipper Selection p
viewModel (Tentative (ZZ l (ZZ ul m ur) r) _) = viewZipper (Selecting (not $ null r)
                                                                      (not $ null l)
                                                                      (not $ null ur)
                                                                      (not $ null ul)
                                                           ) m




viewZipper :: ViewMode -> ProofTreeZipper -> View
viewZipper m (PTZ fv [] pt) = fst $ fst $ viewTree [] m pt
viewZipper m (PTZ fv ctx p) = fst $ fst $ viewZipper' Normal ctx $ viewTree (concatMap skolemsC ctx) m p
  where viewZipper' m (PTC (sks, lrs, str, name) l r : ctx) acc 
           = let str' = viewSentence str
                 [l', r'] = map (map (viewTree (concatMap skolemsC ctx ++ sks) m)) [reverse l, r]
                 ((cs,is), wf) = unzip *** and $ unzip $ l' ++ (acc:r')
             in viewZipper' m ctx ((ViewNode m str' ((if wf then Proven else Unproven) (toSubscript name) (mconcat is)) cs, I sks (map Rules.name lrs)), wf)
        viewZipper' _ [] acc = acc

downwards :: ViewMode -> ViewMode
downwards Speculative = Speculative
downwards (Selecting {}) = Speculative
downwards _ = Normal

viewTree :: [Variable] -> ViewMode -> ProofTree -> ((View, Intros), Bool)
viewTree sks m (PT vs lrs sent ms) = case ms of 
    Just (r, cs) -> let
       ((cs',is), wf) = unzip *** and $ unzip $ map (viewTree (sks ++ vs) (downwards m)) cs
     in ((ViewNode m sent' ((if wf then Proven else Unproven) (toSubscript r) (mconcat is)) cs',I vs (map Rules.name lrs)), wf)
    Nothing -> ((ViewNode m sent' NoRule [], I vs (map Rules.name lrs)), False)
  where sent' = viewSentence sent


viewSentence :: SentenceSchema -> SentenceView
viewSentence (List x)   = ViewList (map viewSentence x)
viewSentence (Symbol q) = ViewSymbol (toSubscript q)
viewSentence (Variable v vs) = ViewVariable (toSubscript v) (map toSubscript vs)
viewSentence (Skolem v ) = ViewSkolem (toSubscript v)

toSubscript :: String -> String 
toSubscript p@(x:_) | isDigit x = p
toSubscript k = map sub k
    where
          sub '0' = '₀'
          sub '1' = '₁'
          sub '2' = '₂'
          sub '3' = '₃'  
          sub '4' = '₄'
          sub '5' = '₅'
          sub '6' = '₆'
          sub '7' = '₇'
          sub '8' = '₈'
          sub '9' = '₉'
          sub x   = x
     
