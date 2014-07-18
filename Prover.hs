{-# LANGUAGE PatternGuards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Prover where

import Data.Monoid
import Rules
import Control.Arrow(second)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Traversable
import Prelude hiding (mapM)
data ProofTree = PT [Variable] [Rule] Sentence (Maybe (RuleName, [ProofTree]))

data ProofTreeContext = PTC ([Variable], [Rule], Sentence, RuleName) [ProofTree] [ProofTree] 

data ProofTreeZipper = PTZ [Variable] [ProofTreeContext] ProofTree



{-



-}


newTree :: Sentence -> ProofTreeZipper
newTree s = PTZ (freeVariables s) [] $ PT [] [] s Nothing  

up :: ProofTreeZipper -> ProofTreeZipper
up (PTZ fv (PTC (sks, lrs, str, name) l r :cs) pt) = PTZ fv cs (PT sks lrs str $ Just (name, reverse l ++ (pt:r)))
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

builtins :: ProofTreeZipper -> [ProofTreeZipper]
builtins (PTZ fv ctx (PT _ _ str _)) = catMaybes [atom, inside]
    where atom :: Maybe ProofTreeZipper 
          atom | (List [Symbol _,Symbol "atom"]) <- str = Just (PTZ fv ctx (PT [] [] str (Just ("⟪ atom ⟫", []))))
               | otherwise = Nothing
          inside | (List [t,Symbol "in",List ls]) <- str, t `elem` ls = Just (PTZ fv ctx (PT [] [] str (Just ("⟪ in ⟫", []))))
                 | otherwise = Nothing

rule :: Rule -> ProofTreeZipper -> [ProofTreeZipper]
rule r p@(PTZ fv ctx (PT sks lrs str _)) = mapMaybe (uncurry (applyRule p)) $ runRule r skols fv $ getSentence p
  where applyRule :: ProofTreeZipper -> [Rule] -> Substitution -> Maybe ProofTreeZipper
        applyRule (PTZ fv ctx (PT sks lrs str _)) ps subst 
                = applySubst subst (PTZ (fv ++ concatMap (freeVariablesRule skols) ps) ctx (PT sks lrs str premises))
           where premises = Just (name r, map (makePT . skolemise (skols ++ fv)) ps)
                 makePT (Rule _ vs ps c) = PT vs (map (freshen allRuleNames) ps) c Nothing
        skols = allSkolems p
        allRuleNames = map name $ localRules p
        freshen banned v = v { name = head (dropWhile (`elem` banned) (map (name v ++) subscripts))}
        subscripts =  map show $ iterate (+1) 1

 

shittymapM :: (a -> Maybe b) -> (c,a) -> Maybe (c, b)
shittymapM f (a,b) = (,) a <$> f b
 
applySubst :: Substitution -> ProofTreeZipper -> Maybe ProofTreeZipper 
applySubst s (PTZ fv ctx pt) = let fv'  = variableSetSubst s fv
                                in do ctx' <- mapM (applySubstPTC s) ctx
                                      pt'  <- applySubstPT s pt
                                      return $ PTZ fv' ctx' pt'
   where applySubstPT :: Substitution -> ProofTree -> Maybe ProofTree 
         applySubstPT s (PT vs lrs str ps) = do
                 str' <- substitute s str
                 ps'  <- mapM (shittymapM (mapM (applySubstPT s))) ps 
                 lrs' <- mapM (substituteRule s) lrs
                 return $  PT vs lrs' str' ps' 
         applySubstPTC :: Substitution -> ProofTreeContext -> Maybe ProofTreeContext
         applySubstPTC s (PTC (vs,lrs,str,r) pts1 pts2) = do
                 lrs' <- mapM (substituteRule s) lrs
                 str' <- substitute s str 
                 pts1' <- mapM (applySubstPT s) pts1
                 pts2' <- mapM (applySubstPT s) pts2
                 return (PTC (vs,lrs', str',r) pts1' pts2')
         

addSubst :: Variable -> Term -> ProofTreeZipper -> ProofTreeZipper
addSubst k v z = case applySubst (subst k v) z of Just x -> x; _ -> z
localRules :: ProofTreeZipper -> [Rule]
localRules (PTZ fv ctx (PT sks lrs str _)) = concatMap localRulesC ctx ++ lrs
   where localRulesC (PTC (sks,lrs,_,_)_ _) = lrs
allSkolems :: ProofTreeZipper -> [Variable]
allSkolems (PTZ fv ctx (PT sks lrs str _)) = concatMap skolemsC ctx ++ sks
skolemsC :: ProofTreeContext -> [Variable]
skolemsC (PTC (sks,lrs,_,_)_ _) = sks
getSentence :: ProofTreeZipper -> SentenceSchema
getSentence (PTZ fv ctx (PT _ _ str _)) = str
