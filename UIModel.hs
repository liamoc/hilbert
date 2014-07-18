module UIModel where

import Prover
import Rules
import Data.Maybe

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

data Model = Selected ProofTreeZipper
           | Tentative (ListZipper (ListZipper ProofTreeZipper)) ProofTreeZipper
     
instance Show Model where 
  show _ = "get fucked vty"

next :: Model -> Model
next (Selected p) = Selected $ right p
next (Tentative p o) = Tentative (rightLZ p) o

prev :: Model -> Model
prev (Selected p) = Selected $ left p
prev (Tentative p o) = Tentative (leftLZ p) o

forward :: Model -> Model
forward (Selected p) = Selected $ down p
forward (Tentative c _) = Selected $ derefLZ $ derefLZ c

back :: Model -> Model
back (Selected p) = Selected $ up p
back (Tentative _ o) = Selected o

prevVariant :: Model -> Model
prevVariant (Tentative lz o) = Tentative (withLZ leftLZ lz) o
prevVariant x = x

nextVariant :: Model -> Model
nextVariant (Tentative lz o) = Tentative (withLZ rightLZ lz) o
nextVariant x = x

clearSubtree :: Model -> Model
clearSubtree (Tentative _ o) = Selected o
clearSubtree (Selected p) = Selected $ oops p

sentence :: Model -> SentenceSchema
sentence (Selected p) = getSentence p
sentence (Tentative _ p) = getSentence p

rulemode :: [Rule] -> Model -> Model 
rulemode rs (Selected p) = maybe Selected Tentative (toLZ $ mapMaybe (toLZ . flip rule p) (rs ++ localRules p)) p
rulemode _ x = x                      

newModel :: Sentence -> Model
newModel = Selected . newTree

subst :: Variable -> Term -> Model -> Model
subst v s (Selected p) = Selected $ addSubst v s p
subst _ _ x = x
