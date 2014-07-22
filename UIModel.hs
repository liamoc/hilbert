{-# LANGUAGE PatternGuards, TupleSections #-}
module UIModel where
import Control.Applicative
import Prover
import Rules
import Data.Maybe
import Control.Arrow (first, second)

data ListZipper a = ZZ [a] a [a] deriving Show


leftLZ :: ListZipper a -> ListZipper a
leftLZ (ZZ ls m (r:rs)) = ZZ (m:ls) r rs
leftLZ x = x

rightLZ :: ListZipper a -> ListZipper a
rightLZ (ZZ (l:ls) m rs) = ZZ ls l (m:rs)
rightLZ x = x

derefLZ :: ListZipper a -> a
derefLZ (ZZ _ m _) = m

withLZ :: (a -> a) -> ListZipper a -> ListZipper a
withLZ f (ZZ l m r) = ZZ l (f m) r

toLZ :: [a] -> Maybe (ListZipper a)
toLZ [] = Nothing
toLZ (x:xs) = Just $ ZZ [] x xs
type SidebarWidth = Int
type Model = (ListZipper ((RuleName, [Axiom]) , ProofModel) , SidebarWidth)

data ProofModel = Selected ProofTreeZipper
                | Tentative (ListZipper (LocalRule , ListZipper ProofTreeZipper)) ProofTreeZipper
     
instance Show ProofModel where 
  show _ = "get fucked vty"

proofModel :: Model -> ProofModel
proofModel = snd . derefLZ . fst

getProofTreeZipper :: ProofModel -> ProofTreeZipper 
getProofTreeZipper (Selected p) = p
getProofTreeZipper (Tentative _ z) = z

withProofModel :: (ProofModel -> ProofModel) -> Model -> Model
withProofModel f = first (withLZ (second f) )

next, prev, nextLemma, prevLemma, forward, back, prevVariant, nextVariant,clearSubtree, rulemode :: Model -> Model
next         = withProofModel nextP
prev         = withProofModel prevP
forward      = withProofModel forwardP
back         = withProofModel backP
nextVariant  = withProofModel nextVariantP
prevVariant  = withProofModel prevVariantP
clearSubtree = withProofModel clearSubtreeP

sentence :: Model -> GoalTerm
sentence = sentenceP . snd . derefLZ . fst

rulemode m | rules <- snd . fst . derefLZ . fst $ m = withProofModel (rulemodeP rules) m
nextLemma = first rightLZ
prevLemma = first leftLZ

newProofModel :: Goal -> ProofModel
newProofModel r = Selected $ newTree (schematicsInGoal r) $ toSubgoal r
              
newModel :: Script -> Maybe Model
newModel = fmap (,41) . toLZ . newModel' []
  where newModel' rules (Axiom r n) = newModel' (r:rules) n
        newModel' rules (Obligation (rn,g) n) = ((rn, rules),newProofModel g) : newModel' (maybeToList (assume rn g) ++ rules) n
        newModel' _ End = [] 

nextP :: ProofModel -> ProofModel
nextP (Selected p) = Selected $ right p
nextP (Tentative p o) = Tentative (rightLZ p) o

prevP :: ProofModel -> ProofModel
prevP (Selected p) = Selected $ left p
prevP (Tentative p o) = Tentative (leftLZ p) o

forwardP :: ProofModel -> ProofModel
forwardP (Selected p) = Selected $ down p
forwardP (Tentative c _) = Selected $ derefLZ $ snd $ derefLZ c

backP :: ProofModel -> ProofModel
backP (Selected p) = Selected $ up p
backP (Tentative _ o) = Selected o

prevVariantP :: ProofModel -> ProofModel
prevVariantP (Tentative lz o) = Tentative (withLZ (second leftLZ) lz) o
prevVariantP x = x

nextVariantP :: ProofModel -> ProofModel
nextVariantP (Tentative lz o) = Tentative (withLZ (second rightLZ) lz) o
nextVariantP x = x

clearSubtreeP :: ProofModel -> ProofModel
clearSubtreeP (Tentative _ o) = Selected o
clearSubtreeP (Selected p) = Selected $ oops p

sentenceP :: ProofModel -> GoalTerm
sentenceP (Selected p) = goalZ p
sentenceP (Tentative _ p) = goalZ p

rulemodeP :: [Axiom] -> ProofModel -> ProofModel 
rulemodeP rs (Selected p) = let theBuiltins = map (\(a,b) -> (a, fromJust . toLZ $ return b)) (builtins p)
                                theRules = mapMaybe (\r -> (r,) <$> toLZ (rule r p)) $ localRules p ++ map localise rs
                             in (maybe Selected Tentative $ toLZ $ theRules ++ theBuiltins) p
rulemodeP _ x = x                      

substP :: Variable -> GoalTerm -> ProofModel -> ProofModel
substP v s (Selected p) = Selected $ addSubst v s p
substP _ _ x = x
