{-# OPTIONS -fno-warn-name-shadowing #-}
module Prover where

import qualified Data.Map as M
import Rules

data ProofTree = PT Sentence (Maybe (RuleName, Substitution, [ProofTree]))
                 deriving (Show, Eq)

data ProofTreeContext = PTC (Sentence, RuleName, Substitution) [ProofTree] [ProofTree] 
                        deriving (Show, Eq)

data ProofTreeZipper = PTZ [ProofTreeContext] ProofTree
                     deriving (Show, Eq)

newTree :: Sentence -> ProofTreeZipper
newTree s = PTZ [] $ PT s Nothing  

up :: ProofTreeZipper -> ProofTreeZipper
up (PTZ (PTC (str, name, subst) l r :cs) pt) = PTZ cs (PT str $ Just (name, subst, l ++ (pt:r)))
up x = x

down :: ProofTreeZipper -> ProofTreeZipper
down (PTZ cs (PT str (Just (name, subst, x:xs)))) = PTZ (PTC (str, name, subst) [] xs:cs) x
down x = x

right :: ProofTreeZipper -> ProofTreeZipper
right (PTZ (PTC payload (l:ls) rs : cs) pt) = PTZ (PTC payload ls (pt:rs) : cs) l
right x = x


left :: ProofTreeZipper -> ProofTreeZipper
left (PTZ (PTC payload ls (r:rs) : cs) pt) = PTZ (PTC payload (pt:ls) rs : cs) r
left x = x

oops :: ProofTreeZipper -> ProofTreeZipper
oops (PTZ ctx (PT str _)) = PTZ ctx (PT str Nothing)

rule :: Rule -> ProofTreeZipper -> [ProofTreeZipper]
rule r p@(PTZ ctx (PT str _)) = if nonSchema then
                                   map ( PTZ ctx
                                       . (\v -> PT str $ Just (name r, M.empty, v))
                                       . map (flip PT Nothing)
                                       ) $ runRule r $ getSentence p
                                else []
   where nonSchema = '?' `notElem` getSentence p

addSubst :: Variable -> String -> ProofTreeZipper -> ProofTreeZipper
addSubst k v (PTZ ctx (PT str (Just (n,s,cs)))) 
   | '?' `notElem` v = PTZ ctx (PT str (Just (n,M.insertWith (flip const) k v s, cs)))
addSubst _ _ p = p

getSentence :: ProofTreeZipper -> SentenceSchema
getSentence (PTZ ctx (PT str _)) = substitute (getSubst ctx) str
  where
    getSubst :: [ProofTreeContext] -> Substitution
    getSubst (PTC (_,_,s) _ _:_) = s
    getSubst _ = M.empty
