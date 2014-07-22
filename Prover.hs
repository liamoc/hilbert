{-# LANGUAGE PatternGuards, RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Prover where

import Rules
import Data.List hiding (concatMap, elem, foldr, all)
import Data.Maybe
import Data.Foldable
import Prelude hiding (mapM, concatMap, elem, sequence,foldr, all)
import Control.Monad
import qualified Vec

data ProofTree = PT { skolemsT :: [Variable]
                    , rulesT :: [LocalRule]
                    , goalT :: GoalTerm 
                    , subgoalsT :: Maybe (RuleName, [ProofTree])
                    }

data ProofTreeContext = PTC ([Variable], [LocalRule], GoalTerm, RuleName) [ProofTree] [ProofTree] 

data ProofTreeZipper = PTZ { schematicsZ :: [Variable] , contextZ :: [ProofTreeContext] , goalTreeZ :: ProofTree }

skolemsZ :: ProofTreeZipper -> [Variable]
skolemsZ (PTZ _ ctx (PT sks _ _ _)) = concatMap skolemsC ctx ++ sks

skolemsC :: ProofTreeContext -> [Variable]
skolemsC (PTC (sks,_,_,_)_ _) = sks

goalZ :: ProofTreeZipper -> GoalTerm
goalZ = goalT . goalTreeZ

newTree :: [Variable] -> ProofTree -> ProofTreeZipper
newTree vs = PTZ vs []  

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

builtins :: ProofTreeZipper -> [(LocalRule, ProofTreeZipper)]
builtins (PTZ fv ctx (PT _ _ str _)) = catMaybes [atom, inside]
    where atom :: Maybe (LocalRule, ProofTreeZipper)
          atom | (List [Symbol _,Symbol "atom"]) <- str = Just (atomRule, PTZ fv ctx (PT [] [] str (Just ("⟪ atom ⟫", []))))
               | otherwise = Nothing
          inside | (List [t,Symbol "in",List ls]) <- str, t `elem` ls = Just (insideRule, PTZ fv ctx (PT [] [] str (Just ("⟪ in ⟫", []))))
                 | otherwise = Nothing
          atomRule = Rule { name = "⟪ atom ⟫", binders = Vec.Nil, premises = [], conclusion = Symbol "⟪ a syntactic atom ⟫ atom"}
          insideRule = Rule { name = "⟪ inside ⟫", binders = Vec.Nil, premises = [], conclusion = Symbol "⟪ x ⟫ in ⟪ a list containing x ⟫"}

toSubgoal :: Goal -> ProofTree
toSubgoal (Goal {..}) = PT boundSkolems assumptions goal Nothing

rule :: LocalRule -> ProofTreeZipper -> [ProofTreeZipper]
rule r p = maybeToList $ 
 let gamma = KnownVars (skolemsZ p) (schematicsZ p) (map name $ localRules p)
  in do (n,u,subgoals,gamma') <- runRule gamma r (goalZ p)
        let p' = applySubst u p
        return $ p' { schematicsZ = knownSchematics gamma', goalTreeZ = (goalTreeZ p') { subgoalsT = Just (n, map toSubgoal subgoals)} }

applySubst :: Unifier -> ProofTreeZipper -> ProofTreeZipper 
applySubst u (PTZ fv ctx pt) = let fv' = fv `afterSubst` u
                                   ctx' = map (applySubstPTC u) ctx
                                   pt'  = applySubstPT u pt
                                in PTZ fv' ctx' pt'
   where applySubstPT :: Unifier -> ProofTree -> ProofTree 
         applySubstPT u (PT sks lrs str gs) 
           = PT sks (map (`substRule` u) lrs)
                    (str >>= u)
                    (fmap (fmap $ map $ applySubstPT u) gs)
         applySubstPTC :: Unifier -> ProofTreeContext -> ProofTreeContext 
         applySubstPTC u (PTC (vs,lrs,str,r) pts1 pts2)
           = PTC (vs, map (`substRule` u) lrs, str >>= u, r)
                 (map (applySubstPT u) pts1) 
                 (map (applySubstPT u) pts2)

addSubst :: Variable -> GoalTerm -> ProofTreeZipper -> ProofTreeZipper
addSubst k v z = 
  let r = filter (== Schematic k []) $ toList $ goalZ z
   in applySubst (foldr (\x s -> if canSubst x v then (x ~> v) >=> s else s) return r) z
  where canSubst :: SkolemsAndSchematics -> GoalTerm -> Bool
        canSubst (Schematic _ ks) t | all (`elem` ks) (allSkolems t) = True
        canSubst _ _ = False 

localRules :: ProofTreeZipper -> [LocalRule]
localRules (PTZ _ ctx (PT _ lrs _ _)) = concatMap localRulesC ctx ++ lrs
   where localRulesC (PTC (_,lrs,_,_)_ _) = lrs
