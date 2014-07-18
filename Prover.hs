{-# OPTIONS -fno-warn-name-shadowing #-}
module Prover where

import Data.Monoid
import Rules
import Control.Arrow(second)
import Data.List

data ProofTree = PT [Variable] [Rule] Sentence (Maybe (RuleName, [ProofTree]))

data ProofTreeContext = PTC ([Variable], [Rule], Sentence, RuleName) [ProofTree] [ProofTree] 

data ProofTreeZipper = PTZ [Variable] [ProofTreeContext] ProofTree



{-

  rule  = [x y. hypothesis* => sentence]
        | [hypothesis* => sentence]
        | [sentence]
        | sentence


  hypothesis = sentence
             | [sentence]
             | [x y. sentence]
             | [x y. n: rule* => y  ]

-}


newTree :: Sentence -> ProofTreeZipper
newTree s = PTZ (freeVariables s) [] $ PT [] [] s Nothing  

up :: ProofTreeZipper -> ProofTreeZipper
up (PTZ fv (PTC (sks, lrs, str, name) l r :cs) pt) = PTZ fv cs (PT sks lrs str $ Just (name, l ++ (pt:r)))
up x = x

down :: ProofTreeZipper -> ProofTreeZipper
down (PTZ fv cs (PT sks lrs str (Just (name, x:xs)))) = PTZ fv (PTC (sks, lrs, str, name) [] xs:cs) x
down x = x

right :: ProofTreeZipper -> ProofTreeZipper
right (PTZ fv (PTC payload (l:ls) rs : cs) pt) = PTZ fv (PTC payload ls (pt:rs) : cs) l
right x = x


left :: ProofTreeZipper -> ProofTreeZipper
left (PTZ fv (PTC payload ls (r:rs) : cs) pt) = PTZ fv (PTC payload (pt:ls) rs : cs) r
left x = x

oops :: ProofTreeZipper -> ProofTreeZipper
oops (PTZ fv ctx (PT sks lrs str _)) = PTZ fv ctx (PT sks lrs str Nothing)

rule :: Rule -> ProofTreeZipper -> [ProofTreeZipper]
rule r p@(PTZ fv ctx (PT sks lrs str _)) = map (uncurry (applyRule p)) $ runRule r skols fv $ getSentence p
  where applyRule :: ProofTreeZipper -> [Rule] -> Substitution -> ProofTreeZipper
        applyRule (PTZ fv ctx (PT sks lrs str _)) ps subst 
                = applySubst subst (PTZ (fv ++ concatMap (freeVariablesRule skols) ps) ctx (PT sks lrs str premises))
           where premises = Just (name r, map makePT ps)
                 makePT (Rule _ vs ps c) = PT vs (map (freshen allRuleNames) ps) c Nothing
        skols = allSkolems p
        allRuleNames = map name $ localRules p
        freshen banned v = v { name = head (dropWhile (`elem` banned) (map (name v ++) subscripts))}
        subscripts =  map show $ iterate (+1) 1

  
applySubst :: Substitution -> ProofTreeZipper -> ProofTreeZipper 
applySubst u (PTZ fv ctx pt) = let 
                                   fv'  = concatMap (freeVariables . lookupSubst s) fv
                                   (s, ctx') = mapAccumR applySubstPTC u ctx
                                   pt'  = applySubstPT s pt
                                in PTZ fv' ctx' pt'
   where applySubstPT :: Substitution -> ProofTree -> ProofTree 
         applySubstPT u (PT vs lrs str ps) = let 
                                   s    = ignoreSubst vs u
                                   str' = substitute s str 
                                   ps'  = fmap (second (map (applySubstPT s))) ps 
                                   lrs' = map (substituteRule s) lrs
                          in PT vs lrs' str' ps' 
         applySubstPTC :: Substitution -> ProofTreeContext -> (Substitution, ProofTreeContext)
         applySubstPTC u (PTC (vs,lrs,str,r) pts pts') =  let s = ignoreSubst vs u in
                (s,PTC (vs,map (substituteRule s) lrs, substitute s str,r) (map (applySubstPT s) pts) (map (applySubstPT s) pts'))
         

addSubst :: Variable -> Term -> ProofTreeZipper -> ProofTreeZipper
addSubst k v = applySubst (subst k v)
localRules :: ProofTreeZipper -> [Rule]
localRules (PTZ fv ctx (PT sks lrs str _)) = concatMap localRulesC ctx ++ lrs
   where localRulesC (PTC (sks,lrs,_,_)_ _) = lrs
allSkolems :: ProofTreeZipper -> [Variable]
allSkolems (PTZ fv ctx (PT sks lrs str _)) = concatMap skolemsC ctx ++ sks
skolemsC :: ProofTreeContext -> [Variable]
skolemsC (PTC (sks,lrs,_,_)_ _) = sks
getSentence :: ProofTreeZipper -> SentenceSchema
getSentence (PTZ fv ctx (PT _ _ str _)) = str
