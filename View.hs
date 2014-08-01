{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module View where

import Control.Arrow
import Data.Monoid
import UIModel
import Data.Char
import Rules
import Prover
import Vec(at)
import Data.List(nub)

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
                  | ViewRuleVar String
                  | ViewSymbol String

data RuleTitle = Proven String Intros
               | Unproven String Intros
               | NoRule


data Intros = I [Variable] [RuleName]
type IsHypothetical = Bool

instance Monoid Intros where 
  mempty = I [] []
  mappend (I a b) (I c d) = I (a ++ c) (b ++ d)

data View = ViewNode ViewMode IsHypothetical SentenceView RuleTitle [View]

data SideView = SelectingView [View] View [View]
              | NormalView [View]

data BarSection = Str String | Bullets Int Int

type BarView = [BarSection]

type ScreenView = (View, SideView, BarView, Int)

viewModel :: Model -> ScreenView
viewModel m@(ZZ _ ((_,_),pm) _, w) = (viewProofModel pm, sideViewProofModel pm, barViewModel m, w)

barViewModel :: Model -> BarView 
barViewModel (ZZ l ((n,_),_) r,_) = [ Str "Hilbert 2.0"
                                    , Bullets (length l) (length r)
                                    , Str $ toSubscript n
                                    ]
viewLocalRule :: ViewMode -> LocalRule ->  View
viewLocalRule = viewRule viewSkolemsAndSchematics False

viewRule :: (a -> SentenceView) -> IsHypothetical -> ViewMode -> Rule a -> View
viewRule f h m  (Rule {..}) = ViewNode m h (viewSentence f' conclusion) (Proven name (I [] []))  (map (viewRule f' (not h) (downwards m)) premises)
  where f' = either (ViewRuleVar . (binders `at`)) f

sideViewProofModel :: ProofModel -> SideView 
sideViewProofModel (Tentative (ZZ l (c,_) r) _) = SelectingView (reverse $ map (viewLocalRule Speculative . fst) l) (viewLocalRule Selection c) (map (viewLocalRule Speculative .fst) r)
sideViewProofModel (Selected c) = NormalView $ map (viewLocalRule Normal) $ localRules c

viewProofModel :: ProofModel -> View
viewProofModel (Selected p) = viewZipper Selection p
viewProofModel (Tentative (ZZ l (_, ZZ ul m ur) r) _) = viewZipper (Selecting (not $ null r)
                                                                              (not $ null l)
                                                                              (not $ null ur)
                                                                              (not $ null ul)
                                                                   ) m


viewZipper :: ViewMode -> ProofTreeZipper -> View
viewZipper m (PTZ _ [] pt) = fst $ fst $ viewTree [] m pt
viewZipper m (PTZ _ ctx p) = fst $ fst $ viewZipper' Normal ctx $ viewTree (concatMap skolemsC ctx) m p
  where viewZipper' m (PTC (sks, lrs, str, name) l r : ctx) acc 
           = let str' = viewSentence viewSkolemsAndSchematics str
                 [l', r'] = map (map (viewTree (concatMap skolemsC ctx ++ sks) m)) [reverse l, r]
                 ((cs,is), wf) = unzip *** and $ unzip $ l' ++ (acc:r')
             in viewZipper' m ctx ((ViewNode m False str' ((if wf then Proven else Unproven) (toSubscript name) (mconcat is)) cs, I sks (map Rules.name lrs)), wf)
        viewZipper' _ [] acc = acc

downwards :: ViewMode -> ViewMode
downwards Speculative = Speculative
downwards (Selecting {}) = Speculative
downwards _ = Normal

viewTree :: [Variable] -> ViewMode -> ProofTree -> ((View, Intros), Bool)
viewTree sks m (PT vs lrs sent ms) = case ms of 
    Just (r, cs) -> let
       ((cs',is), wf) = unzip *** and $ unzip $ map (viewTree (sks ++ vs) (downwards m)) cs
     in ((ViewNode m False sent' ((if wf then Proven else Unproven) (toSubscript r) (mconcat is)) cs',I vs (map (toSubscript . Rules.name) lrs)), wf)
    Nothing -> ((ViewNode m False sent' NoRule [], I vs (map (toSubscript . Rules.name) lrs)), False)
  where sent' = viewSentence viewSkolemsAndSchematics sent

viewSkolemsAndSchematics :: SkolemsAndSchematics -> SentenceView
viewSkolemsAndSchematics (Schematic v vs) = ViewVariable (toSubscript v) (nub $ map toSubscript vs)
viewSkolemsAndSchematics (Skolem v)       = ViewSkolem (toSubscript v)

viewSentence :: (a -> SentenceView) -> Term a -> SentenceView
viewSentence f (List x)   = ViewList (map (viewSentence f) x)
viewSentence _ (Symbol q) = ViewSymbol (toSubscript q)
viewSentence f (Variable v) = f v

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
     
