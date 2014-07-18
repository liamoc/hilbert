{-# LANGUAGE RecordWildCards, PatternGuards #-}
module Rules ( Rule (..)
             , Term (..)
             , RuleName 
             , Substitution
             , SentenceSchema
             , Sentence 
             , Variable
             , runRule
             , substitute
             , freeVariables
             , subst
             , lookupSubst
             , ignoreSubst
             , substituteRule
             , freeVariablesRule
             ) where

import           Data.Monoid

data Term = Symbol String | Variable String | List [Term] deriving Show

type RuleName = String
newtype Substitution = S { lookupSubst :: Variable -> Term }
type SentenceSchema = Term
type Sentence = Term
type Variable = String

instance Monoid Substitution where 
  mempty = S Variable
  mappend a (S b) = S (substitute a . b)

subst :: Variable -> Term -> Substitution 
subst v t = S $ \x -> if x == v then t else Variable x 


data Rule = Rule { name       :: RuleName
                 , schematics :: [Variable]
                 , premises   :: [Rule]
                 , conclusion :: SentenceSchema
                 } deriving Show

-- First order unification as described by Robinson
mgu :: [Variable] -> Term -> Term -> Maybe Substitution
mgu _ (Variable v)  (Variable v') | v == v' = return mempty
mgu _ (Symbol t)    (Symbol t')   | t == t' = return mempty
mgu _ (List [])     (List [])               = return mempty
mgu s (List (x:xs)) (List (y:ys))           = do sigma1 <- mgu s x y
                                                 sigma2 <- mgu s (List (map (substitute sigma1) xs))
                                                                 (List (map (substitute sigma1) ys))
                                                 return (sigma1 <> sigma2)
mgu s (Variable v) t 
  | v `notElem` freeVariables t           
  , v `notElem` s                         = return (subst v t)
mgu s t (Variable v) 
  | v `notElem` freeVariables t          
  , v `notElem` s                         = return (subst v t)
mgu _ _ _                                 = Nothing

runRule :: Rule -> [Variable] -> [Variable] -> Sentence -> [([Rule], Substitution)]
runRule (Rule {..}) sks fv str = let fs = freshen schematics (fv ++ sks)
                                     premises' = map (substituteRule fs) premises
                                  in case mgu sks str (substitute fs conclusion)
                                     of Nothing -> []
                                        Just sigma -> [ (premises', sigma) ]
        where freshen vars banned = mconcat $ map (\v -> subst v (Variable $ head $ dropWhile (`elem` banned) $ map (v ++) subscripts)) vars
              subscripts =  map show $ iterate (+1) 1

substituteRule :: Substitution -> Rule -> Rule 
substituteRule subst (Rule n vs ps c) = let
    subst' = ignoreSubst vs subst
 in Rule n vs (map (substituteRule subst) ps) (substitute subst c)

ignoreSubst :: [Variable] -> Substitution -> Substitution
ignoreSubst vs subst = S $ \ v -> if v `elem` vs then Variable v
                                                 else lookupSubst subst v 

substitute :: Substitution -> SentenceSchema -> SentenceSchema
substitute _     (Symbol s)   = Symbol s
substitute sigma (Variable v) = lookupSubst sigma v 
substitute sigma (List terms) = List $ map (substitute sigma) terms

freeVariables :: Term -> [Variable]
freeVariables (Variable s) = [s]
freeVariables (Symbol _)   = []
freeVariables (List t)     = t >>= freeVariables

freeVariablesRule :: [Variable] -> Rule -> [Variable]
freeVariablesRule sks (Rule {..}) = filter (`notElem` (schematics ++ sks)) $ freeVariables conclusion ++ concatMap (freeVariablesRule sks) premises
